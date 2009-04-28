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

dojo.provide("bespin.page.dashboard.init");  

if (!bespin.page.dashboard) bespin.page.dashboard = {};

// = Dashboard =
//
// This file is the dashboard code that is loaded via script src
// from /dashboard.html.

(function() {
    var heightDiff;
    var projects;
    var scene;
    var tree;
    var infoPanel;
    var currentProject;
    var go = bespin.util.navigate; // short cut static method 
    var bd = bespin.page.dashboard;

    var server;
    var settings;
    var editSession;
    var files;
    var commandLine;
    
    dojo.mixin(bespin.page.dashboard, {
        tree: null,
        lastSelectedPath: null,
        urlTimeout: null,
        isLoggedIn: false,
        
        sizeCanvas: function(canvas) {
            if (!heightDiff) {
                heightDiff = dojo.byId("header").clientHeight + dojo.byId("subheader").clientHeight + dojo.byId("footer").clientHeight;
            }
            var height = window.innerHeight - heightDiff + 11;
            dojo.attr(canvas, { width: window.innerWidth, height: height });
        },
        
        loggedIn: function(userinfo)  {
            // in some cases the user gets logged in twice => causes problems with restorePath()...
            if (bd.isLoggedIn) return;
            
            bd.isLoggedIn = true;
            editSession.setUserinfo(userinfo);

            server.list(null, null, bd.displayProjects); // get projects
            server.listOpen(bd.displaySessions); // get sessions
        },

        notLoggedIn: function(xhr) {
            go.home();
        },

        prepareFilesForTree: function(files) {
            if (files.length == 0) return [];

            var name;
            var fdata = [];
            var settings = bespin.get('settings');
            for (var i = 0; i < files.length; i++) {
                name = files[i].name;
                if (settings && settings.isSettingOff('dotmode') && name[0] == '.') {
                    continue;
                }
                if (/\/$/.test(name)) {
                    name = name.substring(0, name.length - 1);
                    fdata.push({ name: name, contents: bd.fetchFiles });
                } else {
                    fdata.push({ name: name });
                }
            }

            return fdata;
        },

        getFilePath: function(treePath) {
            var filepath = "";

            for (var i = 0; i < treePath.length; i++) {
                if (treePath[i] && treePath[i].name) {
                    filepath += treePath[i].name + ((i < treePath.length - 1) ? "/" : "");
                }
            }
            return filepath;
        },
        
        fetchFiles: function(path, tree) {
            var filepath = bd.getFilePath(path);

            server.list(filepath, null, function(files) {
                tree.updateData(path[path.length - 1], bd.prepareFilesForTree(files));
                tree.render();
            });
        },

        displaySessions: function(sessions) {
            infoPanel.removeAll();

            for (var project in sessions) {
                for (var file in sessions[project]) {
                    var lastSlash = file.lastIndexOf("/");
                    var path = (lastSlash == -1) ? "" : file.substring(0, lastSlash);
                    var name = (lastSlash == -1) ? file : file.substring(lastSlash + 1);

                    var panel = new th.BespinSessionPanel({ filename: name, project: project, path: path });
                    infoPanel.add(panel);
                    panel.bus.bind("dblclick", panel, function(e) {
                        var newTab = e.shiftKey;
                        go.editor(e.thComponent.session.project, e.thComponent.session.path + (e.thComponent.session.path != '' ? '/' : '' ) + e.thComponent.session.filename, newTab);
                    });
                }
            }
            infoPanel.render();
        },

        restorePath: function(newPath) {         
            bd.lastSelectedPath = bd.lastSelectedPath || '';
            newPath = newPath || '';
            var oldPath = bd.lastSelectedPath;
            bd.lastSelectedPath = newPath;
                        
            if (newPath == oldPath && newPath != '') return;     // the path has not changed

            newPath = newPath.split('/');
            oldPath = oldPath.split('/');
            currentProject = newPath[0];

            tree.lists[0].selectItemByText(newPath[0]);    // this also perform a rendering of the project.list
            scene.renderAllowed = false;

            var sameLevel = 1;  // the value is 1 and not 0, as the first list (the project list) is not affected!
            while (sameLevel < Math.min(newPath.length, oldPath.length) && newPath[sameLevel] == oldPath[sameLevel] && newPath[sameLevel] != '') {
                sameLevel ++;
            }
                                                                                                  
            var fakePath = new Array(newPath.length);
            for (var x = 1; x < newPath.length; x++) {
                var fakeItem = new Object();
                fakeItem.name = newPath[x];
                if (x != newPath.length - 1) {
                    fakeItem.contents = 'fake';   
                }
                if (x > bd.tree.lists.length - 1) {
                   bd.tree.showChildren(null, new Array(fakeItem)); 
                }  
                if (newPath[x] != '') {
                    bd.tree.lists[x].selectItemByText(newPath[x]);   
                }
                fakePath[x] = fakeItem;
            }
            
            if (newPath.length <= bd.tree.lists.length) {
                bd.tree.removeListsFrom(newPath.length);
            }
                                            
            var contentsPath = new Array(newPath.length);
            var countSetupPaths = sameLevel;

            // this function should stay here, as this funciton is accessing "pathContents" and "countSetupPaths"
            var displayFetchedFiles = function(files) {
                // "this" is the callbackData object!
                var contents = bd.prepareFilesForTree(files);
                if (this.index != 0) {
                    contentsPath[this.index] = contents;
                }
                
                bd.tree.replaceList(this.index, contents);
                bd.tree.lists[this.index].selectItemByText(fakePath[this.index].name);
                countSetupPaths ++;
                
                if (countSetupPaths == newPath.length) {
                    for (var x = 0; x < newPath.length - 1; x++) {
                        // when the path is not restored from the root, then there are contents without contents!
                        if (contentsPath[x + 1]) {
                            // todo: I added the if () to fix an error, not sure if it was a symptom of something larger
                            if (bd.tree.lists[x].selected) bd.tree.lists[x].selected.contents = contentsPath[x + 1];                            
                        }
                    }
                }
            };
            
            // get the data for the lists
            for (var x = sameLevel; x < newPath.length; x++) {                                                
                var selected = bd.tree.lists[x - 1].selected;
                if (selected && selected.contents && dojo.isArray(selected.contents)) {
                    // restore filelist from local memory (the filelists was ones fetched)
                    if (x > bd.tree.lists.length - 1) {
                        bd.tree.showChildren(null, selected.contents);
                    } else {
                        bd.tree.replaceList(x, selected.contents);
                    }
                    bd.tree.lists[x].selectItemByText(fakePath[x].name);                        
                    countSetupPaths ++;
                } else {
                    // load filelist form server                                                            
                    var filepath = currentProject + "/" + bd.getFilePath(fakePath.slice(1, x));
                    server.list(filepath, null, dojo.hitch({index: x}, displayFetchedFiles));                    
                }
            }
            
            // deselect lists if needed
            for (var x = newPath.length; x < tree.lists.length; x++) {
                delete tree.lists[x].selected;
            }
            
            scene.renderAllowed = true;
            scene.render();
        },

        displayProjects: function(projectItems) {
            for (var i = 0; i < projectItems.length; i++) {
                projectItems[i] = { name: projectItems[i].name.substring(0, projectItems[i].name.length - 1) , contents: bd.fetchFiles};
            }
            
            tree.replaceList(0, projectItems);
                                    
            // Restore the last selected file
            var path =  (new bespin.client.settings.URL()).get('path');
            if (!bd.lastSelectedPath) {
                bd.restorePath(path);
            } else {
                scene.render();                
            }
        },

        refreshProjects: function() {
            server.list(null, null, bd.displayProjects);
        }
    }); 
    
    dojo.connect(window, "resize", function() {
        bd.sizeCanvas(dojo.byId("canvas"));
    });
    
    dojo.addOnLoad(function() {
        bd.sizeCanvas(dojo.byId("canvas"));

        dojo.forEach(['subheader', 'header'], function(i) { dojo.setSelectable(i, false); });

        bespin.displayVersion(); // display the version on the page

        scene = new th.Scene(dojo.byId("canvas"));  

        tree = new th.HorizontalTree({ id: "htree" });

        bd.tree = tree;
        
        // invoking showChildren() here causes the List containing the children to be created, which is necessary
        // for us to manipulate it a touch here
        bd.tree.showChildren(null, [{name: ''}]);

        // set various properties of this first list, which contains the projects to display
        tree.lists[0].addTopLabel(new th.Label({ text: "Projects" }));
        tree.lists[0].allowDeselection = false;

        var topPanel = new th.Panel();
        topPanel.add([ tree ]);
        topPanel.layout = function() {
            var d = this.d();
            tree.bounds = { x: d.i.l, y: d.i.t, width: d.b.w - d.i.w, height: d.b.h - d.i.h };
        };

        infoPanel = new th.ExpandingInfoPanel();

        var splitPanel = new th.SplitPanel({ id: "splitPanel",
            orientation: th.VERTICAL,
            regions: [ { size: "75%", contents: topPanel }, { size: "25%", contents: infoPanel } ]
        });

        splitPanel.regions[0].label = new th.Label({ text: "Open Sessions" });

        scene.root.add(splitPanel);

        scene.render();

        scene.bus.bind("dblclick", tree, function(e) {
            var newTab = e.shiftKey;
            var path = tree.getSelectedPath();
            if (path.length == 0 || path[path.length - 1].contents) return; // don't allow directories either
            go.editor(currentProject, bd.getFilePath(path.slice(1, path.length)), newTab);
        });

        scene.bus.bind("itemselected", tree, function(e) {
            console.log("item selected on tree");

            var pathSelected = tree.getSelectedPath(true);
            var db = bespin.page.dashboard;
            // this keeps the url to be changed if the file path changes to frequently
            if (db.urlTimeout) {
                clearTimeout(db.urlTimeout);
            }
            db.urlTimeout = setTimeout(dojo.hitch(pathSelected, function () {
                bespin.page.dashboard.lastSelectedPath = this;
                location.hash = '#path=' + this;
            }), 300);
        });

        scene.bus.bind("itemselected", tree.lists[0], function(e) {
            currentProject = e.item.name;
            bespin.publish("project:set", { project: currentProject, suppressPopup: true, fromDashboardItemSelected: true });
        });

        // setup the command line
        server = bespin.register('server', new bespin.client.Server());
        settings = bespin.register('settings', new bespin.client.settings.Core());
        editSession = bespin.register('editSession', new bespin.client.session.EditSession());
        files = bespin.register('files', new bespin.client.FileSystem());
        commandLine = bespin.register('commandLine', new bespin.cmd.commandline.Interface('command', bespin.cmd.dashboardcommands.Commands));

        // Handle jumping to the command line
        dojo.connect(document, "onkeypress", function(e) {
            var handled = commandLine.handleCommandLineFocus(e);
            if (handled) return false;
        });

        // get logged in name; if not logged in, display an error of some kind
        server.currentuser(bd.loggedIn, bd.notLoggedIn);   
        
        // provide history for the dashboard
        bespin.subscribe("url:changed", function(e) {
            var pathSelected =  (new bespin.client.settings.URL()).get('path');
            bespin.page.dashboard.restorePath(pathSelected);
        });

        // TODO: commenting this out as it is throwing errors at the moment
        // provide arrow navigation to dashboard
        dojo.connect(window, "keydown", dojo.hitch(tree, function(e) {
            // catch focus on commandline
            if(commandLine.handleCommandLineFocus(e)) return false;

            var key = bespin.util.keys.Key;
            var path = this.getSelectedPath();
            if (path === undefined) return;
            // things to make life much more easy :)
            var index = path.length - 1;
            var list = this.lists[index];
            var listNext = (this.lists.length > index ? this.lists[index + 1] : false);
            var listPre = (index != 0 ? this.lists[index - 1] : false);

            switch (e.keyCode) {
                case key.LEFT_ARROW:
                    if (!listPre) break;
                    // listPre.selected.lastSelected = list.selected.name;  // save the selection, if the user comes back to this list
                    listPre.bus.fire("itemselected", { container: listPre, item: list.selected }, listPre);
                    break;
                case key.RIGHT_ARROW:
                    if (!listNext) break;
                    if (list.selected.lastSelected) {
                        listNext.selectItemByText(list.selected.lastSelected);
                        listNext.bus.fire("itemselected", { container: listNext, item: list.selected }, listNext);
                    } else {
                        listNext.selected = listNext.items[0];
                        listNext.bus.fire("itemselected", { container: listNext, item: list.selected }, listNext);
                    }
                    break;
                case key.UP_ARROW:
                    list.moveSelectionUp();
                    break;
                case key.DOWN_ARROW:
                    list.moveSelectionDown();
                    break;
                case key.ENTER:
                    this.bus.fire("dblclick", e, tree);
                    break;
            }
        }));
        
        // Set up message retrieval
        server.processMessages();
    });
})();
