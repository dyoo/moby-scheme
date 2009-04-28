dojo.provide("bespin.editor.component");

dojo.require("bespin.bespin");

dojo.require("bespin.util.canvas");
dojo.require("bespin.util.keys");
dojo.require("bespin.util.navigate");
dojo.require("bespin.util.path");
dojo.require("bespin.util.tokenobject");
dojo.require("bespin.util.util");
dojo.require("bespin.util.mousewheelevent");
dojo.require("bespin.util.urlbar");

dojo.require("bespin.client.filesystem");
dojo.require("bespin.client.settings");
dojo.require("bespin.client.status");
dojo.require("bespin.client.server");
dojo.require("bespin.client.session");

dojo.require("bespin.editor.actions");
dojo.require("bespin.editor.clipboard");
dojo.require("bespin.editor.cursor");
dojo.require("bespin.editor.editor");
dojo.require("bespin.editor.events");
dojo.require("bespin.editor.model");
dojo.require("bespin.editor.toolbar");
dojo.require("bespin.editor.undo");

dojo.require("bespin.themes.default");

dojo.require("bespin.syntax.base"); 
dojo.require("bespin.syntax.simple._base");

dojo.require("bespin.cmd.commandline");
dojo.require("bespin.cmd.commands");
dojo.require("bespin.cmd.editorcommands");

dojo.require("th.helpers"); // -- Thunderhead... hooooo
dojo.require("th.css");
dojo.require("th.th");
dojo.require("th.models");
dojo.require("th.borders");
dojo.require("th.components");

// = Editor Component =
//
// This is a component that you can use to embed the Bespin Editor component anywhere you wish.
//
// There are a set of options that you pass in, as well as the container element
//
// * {{{loadfromdiv}}} : Take the innerHTML from the given div and load it into the editor
// * {{{content}}} : Feed the editor the string as the initial content (loadfromdiv trumps this)
// * {{{language}}} : The given syntax highlighter language to turn on (not people language!)
// * {{{dontstealfocus}}} : by default the component will steal focus when it loads, but you can change that by setting this to true

dojo.declare("bespin.editor.Component", null, {
    // ** {{{ constructor }}} **
    //
    // Takes a container element, and the set of options for the component which include those noted above.
    constructor: function(container, opts) {
        opts.actsAsComponent = true;
        
        var initialcontent;
        if (opts.loadfromdiv) {
            initialcontent = dojo.byId(container).innerHTML;
        } else if (opts.content) {
            initialcontent = opts.content;
        }
        
        this.editor = bespin.register('editor', opts.editor || new bespin.editor.API(container, opts));
        this.editSession = bespin.register('editSession', opts.editSession || new bespin.client.session.EditSession(this.editor));
        this.server = bespin.register('server', opts.server || new bespin.client.Server());
        this.files = bespin.register('files', opts.files || new bespin.client.FileSystem());
        
        // Use in memory settings here instead of saving to the server which is default. Potentially use Cookie settings
        bespin.register('settings', opts.settings || new bespin.client.settings.Core(bespin.client.settings.InMemory));

        dojo.connect(window, 'resize', opts.resize || dojo.hitch(this, function() {
            this.editor.paint();
        }));

        if (initialcontent) {
            this.setContent(initialcontent);
        }

        if (opts.language) { // -- turn on syntax highlighting
            bespin.publish("settings:language", { language: opts.language });
        }

        if (!opts.dontstealfocus) {
            this.editor.canvas.focus();
        }

        if (opts.set) { // we have generic settings
            for (var key in opts.set) {
                if (opts.set.hasOwnProperty(key)) {
                    this.set(key, opts.set[key]);
                }
            }
        }
    },

    // ** {{{ getContent }}} **
    //
    // Returns the contents of the editor
    getContent: function() {
        return this.editor.model.getDocument();
    },

    // ** {{{ setContent }}} **
    //
    // Takes the content and inserts it fresh into the document
    setContent: function(content) {
        return this.editor.model.insertDocument(content);
    },

    // ** {{{ setFocus(bool) }}} **
    //
    // If you pass in true, focus will be set on the editor, if false, it will not.
    setFocus: function(bool) {
        return this.editor.setFocus(bool);
    },

    // ** {{{ setLineNumber }}} **
    //
    // Pass in the line number to jump to (and refresh)
    setLineNumber: function(linenum) {
        bespin.publish("editor:moveandcenter", {
            row: linenum
        });
    },

    // ** {{{ setLineNumber }}} **
    //
    // Talk to the Bespin settings structure and pass in the key/value
    set: function(key, value) {
        bespin.publish("settings:set", {
           key: key,
           value: value 
        });
    },

    // ** {{{ onchange }}} **
    //
    // Track changes in the document
    onchange: function(callback) {
        bespin.subscribe("editor:document:changed", callback);
    },

    // ** {{{ bindKey }}} **
    //
    // Given the options that should contain a modifier, key, and action.
    //
    // For example,
    //
    // { 
    //   modifiers: "ctrl",
    //   key: "b",
    //   action: "moveCursorLeft"
    // }
    bindKey: function(opts) {
        bespin.publish("editor:bindkey", opts);
    }

});

