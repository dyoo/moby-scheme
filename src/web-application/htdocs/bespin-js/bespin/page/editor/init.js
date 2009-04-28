/* ***** BEGIN LICENSE BLOCK *****
 * Version: MPL 1.1
 *
 * The contents of this file are subject to the Mozilla Public License
 * Version 1.1 (the "License"); you may not use this file except in
 * compliance with the License. You may obtain a copy of the License at
 * http://www.mozilla.org/MPL/
 *
 * Software distributed under the License is distributed on an "AS IS"
 * basis, WITHOUT WARRANTY OF ANY KIND, either express or implied.
 * See the License for the specific language governing rights and
 * limitations under the License.
 *
 * The Original Code is Bespin.
 *
 * The Initial Developer of the Original Code is Mozilla.
 * Portions created by the Initial Developer are Copyright (C) 2009
 * the Initial Developer. All Rights Reserved.
 *
 * Contributor(s):
 *   Bespin Team (bespin@mozilla.com)
 *
 * ***** END LICENSE BLOCK ***** */

dojo.provide("bespin.page.editor.init");

// = Bootstrap =
//
// This file is the editor bootstrap code that is loaded via script src
// from /editor.html.
//
// It handles setting up the objects that are to be used on the editor
// and deals with layout changes.

// ** {{{ Globals }}}
//
// One day we will get rid of all of these bar the core bespin object.

// pieces in the scene
(function() {
    var projectLabel;
    var fileLabel;
    var dirtyLabel;
    var scene;

    dojo.mixin(bespin.page.editor, {
        // ** {{{ recalcLayout() }}} **
        //
        // When a change to the UI is needed due to opening or closing a feature
        // (e.g. file view, session view) move the items around
        recalcLayout: function() {
            var subheader = dojo.byId("subheader");
            var footer = dojo.byId("footer");
            var editor = dojo.byId("editor");
            var files = dojo.byId("files");
            var collab = dojo.byId("collab");
            var target = dojo.byId("target_browsers");

            var move = [ subheader, footer, editor ];

            if (bespin.get('toolbar').showCollab) {
                collab.style.display = "block";
                dojo.forEach(move, function(item) { item.style.right = "201px"; });
            } else {
                collab.style.display = "none";
                dojo.forEach(move, function(item) { item.style.right = "0"; });
            }

            if (bespin.get('toolbar').showTarget) {
                target.style.display = "block";
            } else {
                target.style.display = "none";
            }

            this.doResize();            
        },

        // ** {{{ doResize() }}} **
        //
        // When a user resizes the window, deal with resizing the canvas and repaint
        doResize: function() {
            var d = dojo.coords('status');
            dojo.attr('projectLabel', { width: d.w, height: d.h });

            // Repaint the various canvas'
            scene.paint();
            bespin.get('editor').paint();            
            bespin.get('commandLine').infoResizer();
        }
    });

    // ** {{{ window.load time }}} **
    //
    // Loads and configures the objects that the editor needs
    dojo.addOnLoad(function() {
        var editor = bespin.register('editor', new bespin.editor.API('editor'));
        var editSession = bespin.register('editSession', new bespin.client.session.EditSession(editor));
        var server = bespin.register('server', new bespin.client.Server());
        var files = bespin.register('files', new bespin.client.FileSystem());

        bespin.register('actions', editor.ui.actions);
        bespin.register('filesearch', new bespin.editor.filesearch.API());
        bespin.register('toolbar', new bespin.editor.Toolbar(editor, { setupDefault: true }));
        bespin.register('quickopen', new bespin.editor.quickopen.API());

        // Get going when settings are loaded
        bespin.subscribe("settings:loaded", function(event) {
            bespin.get('settings').loadSession();  // load the last file or what is passed in
            bespin.page.editor.doResize();
        });
        
        var whenLoggedIn = function(userinfo) {
            bespin.get('editSession').setUserinfo(userinfo);

            bespin.register('settings', new bespin.client.settings.Core());
            bespin.register('commandLine', new bespin.cmd.commandline.Interface('command', bespin.cmd.editorcommands.Commands));

            // Set up message retrieval
            server.processMessages();

            bespin.publish("authenticated");
        };

        var whenNotLoggedIn = function() {
            bespin.util.navigate.home(); // go back
        };

        // Force a login just in case the user session isn't around
        server.currentuser(whenLoggedIn, whenNotLoggedIn);

        // Set the version info
        bespin.displayVersion();

        // START SEARCH BINDINGS
        // bind in things for search :)
        // some of the key-bindings go to the window object direct, to make them happen all over the window
        dojo.connect(window, 'keydown', function(e) {
            if (e.keyCode == bespin.util.keys.Key.F && (e.metaKey || e.ctrlKey)) {
                bespin.get('actions').toggleFilesearch();
                dojo.stopEvent(e);
            } else if (e.keyCode == bespin.util.keys.Key.G && (e.metaKey || e.ctrlKey)) {
                if (e.shiftKey) {
                    bespin.get('actions').findPrev();
                } else {
                    bespin.get('actions').findNext();
                }
                dojo.stopEvent(e);
            }
        });

        // Handle Enter & Escape
        dojo.connect(dojo.byId('searchquery'), 'keydown', function(e) {
            var key = bespin.util.keys.Key;

            if (e.keyCode == key.ESCAPE) {
                dojo.byId('searchresult').style.display = 'none';
                dojo.byId('searchquery').blur();
                bespin.get('editor').setFocus(true);
                dojo.stopEvent(e);
            } else if (e.keyCode == key.ENTER) {
                bespin.get('actions').startSearch(dojo.byId('searchquery').value, 'toolbar', e.shiftKey);
                dojo.stopEvent(e);
            }
        });

        // preform a new search after a character has been added to the searchquery-input-field
        dojo.connect(dojo.byId('searchquery'), 'keypress', function(e) {
            var key = bespin.util.keys.Key;    
            var isOkay = false;
        
            // check to let only some keys perform a new search!
            if ([key.ENTER, key.BACKSPACE, key.DELETE].indexOf(e.keyCode) != -1) isOkay = true;
            if ([64 /*@*/, 91/*[*/, 92/*\*/, 93/*]*/, 94/*^*/, 123/*{*/, 124/*|*/, 125/*}*/, 126/*~*/ ].indexOf(e.charCode) != -1)  isOkay = true;
            if ((e.charCode >= 32) && (e.charCode <= 126) || e.charCode >= 160) isOkay = true;
        
            if (!isOkay) {
                // the key was not a character!
                return;
            }
        
            // perform a search only each 300ms
            var ui = bespin.get('editor').ui;
            if (ui.serachTimeout) {
                clearTimeout(ui.serachTimeout);
            }
            ui.serachTimeout = setTimeout(dojo.hitch(ui, function () {
                this.actions.startSearch(dojo.byId('searchquery').value, 'toolbar');
            }), 300);   
        });

        // handle things when search field get focused
        dojo.connect(dojo.byId('searchquery'), 'focus', function(e) {
            bespin.get('editor').setFocus(false);
            dojo.byId('searchquery').select();
        });
        
        // little helper function ;)
        function addButtonEvents(elm, filename, clickFunc) {
            dojo.connect(elm, 'mouseover', function() {
                elm.src = "images/" + filename + "_on.png";
            });

            dojo.connect(elm, 'mouseout', function() {
                elm.src = "images/" + filename + ".png";
            });
            
            dojo.connect(elm, 'click', clickFunc);
        }
        
        // stuff for the buttons
        addButtonEvents(dojo.byId('searchprev'), 'button_left', function() {
            bespin.get('actions').findPrev();
        });

        addButtonEvents(dojo.byId('searchnext'), 'button_right', function() {
            bespin.get('actions').findNext();    
        });
        
        addButtonEvents(dojo.byId('searchdone'), 'button_done', function() {
            dojo.byId('searchresult').style.display = 'none';
            bespin.get('editor').setFocus(true);
            dojo.byId('searchquery').blur();
        });
        
        // END SEARCH BINDINGS

        dojo.connect(window, 'resize', bespin.page.editor, "doResize");

        scene = new th.Scene(dojo.byId("projectLabel"));

        var panel = new th.components.Panel();
        scene.root.add(panel);

        projectLabel = new th.components.Label({ style: {
            color: "white",
            font: "12pt Calibri, Arial, sans-serif"
        }});
        var symbolThingie = new th.components.Label({ text: ":", style: {
            color: "gray",
            font: "12pt Calibri, Arial, sans-serif"
        }});
        fileLabel = new th.components.Label({ style: {
            color: "white",
            font: "12pt Calibri, Arial, sans-serif"
        }});
        dirtyLabel = new th.components.Label({ text: "", style: {
            color: "white",
            font: "12pt Calibri, Arial, sans-serif"
        }});

        panel.add([ projectLabel, symbolThingie, fileLabel, dirtyLabel ]);
        panel.layout = function() {
            var d = this.d();

            var x = 0;
            for (var i = 0; i < 2; i++) {
                var width = this.children[i].getPreferredWidth(d.b.h);
                this.children[i].bounds = { x: x, y: 0, width: width, height: d.b.h };
                x += width;
            }

            var dirtyWidth = this.children[3].getPreferredWidth(d.b.h);
            var filenameWidth = d.b.w - d.i.w - x - dirtyWidth;
            this.children[2].bounds = { x: x, y: 0, width: filenameWidth, height: d.b.h };
            x += filenameWidth - 18; // 18 is an evil magic number that is caused by a DOM bug. The new Thunderhead will fix this :)
            this.children[3].bounds = { x: x, y: 0, width: dirtyWidth, height: d.b.h };
        };
        scene.render();
    });

    // ** {{{ Event: editor:openfile:opensuccess }}} **
    // 
    // When a file is opened successfully change the project and file status area.
    // Then change the window title, and change the URL hash area
    bespin.subscribe("editor:openfile:opensuccess", function(event) {
        var project = event.project || bespin.get('editSession').project; 
        var filename = event.file.name;

        projectLabel.attributes.text = project;
        fileLabel.attributes.text = filename;
        scene.render();
    });
    
    // ** {{{ Event: editor:dirty }}} **
    // 
    // Add a notifier to show that the file is dirty and needs to be saved
    bespin.subscribe("editor:dirty", function(event) {
        dirtyLabel.attributes.text = "●";
        scene.render();
    });

    // ** {{{ Event: editor:dirty }}} **
    // 
    // Change the HTML title to change - to ● as a subtle indicator
    bespin.subscribe("editor:dirty", function() {
        document.title = document.title.replace('- editing with Bespin', '● editing with Bespin');
    });

    // ** {{{ Event: editor:clean }}} **
    // 
    // Take away the notifier. Just saved
    bespin.subscribe("editor:clean", function(event) {
        dirtyLabel.attributes.text = "";
        scene.render();
    });

    // ** {{{ Event: editor:clean }}} **
    // 
    // Take away the notifier from the HTML title.
    bespin.subscribe("editor:clean", function(event) {
        document.title = document.title.replace('● editing with Bespin', '- editing with Bespin');
    });

})();
