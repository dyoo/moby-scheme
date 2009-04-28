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

dojo.provide("bespin.client.settings");

// = Settings =
//
// This settings module provides a base implementation to store settings for users.
// It also contains various "stores" to save that data, including:
//
// * {{{bespin.client.settings.Core}}} : Core interface to settings. User code always goes through here.
// * {{{bespin.client.settings.Server}}} : The main store. Saves back to the Bespin Server API
// * {{{bespin.client.settings.InMemory}}} : In memory settings that are used primarily for debugging
// * {{{bespin.client.settings.Cookie}}} : Store in a cookie using cookie-jar
// * {{{bespin.client.settings.URL}}} : Intercept settings in the URL. Often used to override
// * {{{bespin.client.settings.DB}}} : Commented out for now, but a way to store settings locally
// * {{{bespin.client.settings.Events}}} : Custom events that the settings store can intercept and send

// ** {{{ bespin.client.settings.Core }}} **
//
// Handles load/save of user settings.
// TODO: tie into the sessions servlet; eliminate Gears dependency

dojo.declare("bespin.client.settings.Core", null, {
    constructor: function(store) {
        this.browserOverrides = {};
        this.fromURL = new bespin.client.settings.URL();
        this.customEvents = new bespin.client.settings.Events(this);

        this.loadStore(store); // Load up the correct settings store
    },

    loadSession: function() {
        var path    = this.fromURL.get('path') || this.get('_path');
        var project = this.fromURL.get('project') || this.get('_project');

        bespin.publish("settings:init", { // -- time to init my friends
            path: path,
            project: project
        });
    },

    defaultSettings: function() {
        return {
            'tabsize': '2',
            'tabmode': 'off',
            'tabarrow': 'on',
            'fontsize': '10',
            'autocomplete': 'off',
            'collaborate': 'off',
            'language': 'auto',
            'strictlines': 'on',
            'syntaxengine': 'simple',
            'preview': 'window',
            'smartmove': 'on',
            'jslint': 'off'
        };
    },

    isOn: function(value) {
        return value == 'on' || value == 'true';
    },

    isOff: function(value) {
        return value == 'off' || value == 'false' || value === undefined;
    },
    
    isSettingOn: function(key) {
        return this.isOn(this.get(key));
    },

    isSettingOff: function(key) {
        return this.isOff(this.get(key));
    },

    // ** {{{ Settings.loadStore() }}} **
    //
    // This is where we choose which store to load
    loadStore: function(store) {
        this.store = new (store || bespin.client.settings.ServerFile)(this);
    },

    toList: function() {
        var settings = [];
        var storeSettings = this.store.settings;
        for (var prop in storeSettings) {
            if (storeSettings.hasOwnProperty(prop)) {
                settings.push({ 'key': prop, 'value': storeSettings[prop] });
            }
        }
        return settings;
    },

    set: function(key, value) {
        this.store.set(key, value);

        bespin.publish("settings:set:" + key, { value: value });
    },

    get: function(key) {
        var fromURL = this.fromURL.get(key); // short circuit
        if (fromURL) return fromURL;

        return this.store.get(key);
    },

    unset: function(key) {
        this.store.unset(key);
    },

    list: function() {
        if (typeof this.store['list'] == "function") {
            return this.store.list();
        } else {
            return this.toList();
        }
    }

});

// ** {{{ bespin.client.settings.InMemory }}} **
//
// Debugging in memory settings (die when browser is closed)

dojo.declare("bespin.client.settings.InMemory", null, {
    constructor: function(parent) {
        this.parent = parent;
        this.settings = this.parent.defaultSettings(); 
        bespin.publish("settings:loaded"); 
    },

    set: function(key, value) {
        this.settings[key] = value;
    },

    get: function(key) {
        return this.settings[key];
    },

    unset: function(key) {
        delete this.settings[key];
    }
});

// ** {{{ bespin.client.settings.Cookie }}} **
//
// Save the settings in a cookie

dojo.declare("bespin.client.settings.Cookie", null, {
    constructor: function(parent) {
        var expirationInHours = 1;
        this.cookieSettings = {
            expires: expirationInHours / 24,
            path: '/'
        };

        var settings = dojo.fromJson(dojo.cookie("settings"));

        if (settings) {
            this.settings = settings;
        } else {
            this.settings = {
                'tabsize': '2',
                'fontsize': '10',
                'autocomplete': 'off',
                'collaborate': 'off',
                '_username': 'dion'
            };
            dojo.cookie("settings", dojo.toJson(this.settings), this.cookieSettings);
        }
        bespin.publish("settings:loaded");
    },

    set: function(key, value) {
        this.settings[key] = value;
        dojo.cookie("settings", dojo.toJson(this.settings), this.cookieSettings);
    },

    get: function(key) {
        return this.settings[key];
    },

    unset: function(key) {
        delete this.settings[key];
        dojo.cookie("settings", dojo.toJson(this.settings), this.cookieSettings);
    }
});    

// ** {{{ bespin.client.settings.ServerAPI }}} **
//
// The real grand-daddy that implements uses {{{Server}}} to access the backend

dojo.declare("bespin.client.settings.ServerAPI", null, {
    constructor: function(parent) {
        this.parent = parent;
        this.server = bespin.get('server');
        this.settings = this.parent.defaultSettings(); // seed defaults just for now!

        // TODO: seed the settings  
        this.server.listSettings(dojo.hitch(this, function(settings) {
            this.settings = settings;
            if (settings['tabsize'] === undefined) {
                this.settings = this.parent.defaultSettings();
                this.server.setSettings(this.settings);
            }
            bespin.publish("settings:loaded");
        }));
    },

    set: function(key, value) {
        this.settings[key] = value;
        this.server.setSetting(key, value);
    },

    get: function(key) {
        return this.settings[key];
    },

    unset: function(key) {
        delete this.settings[key];
        this.server.unsetSetting(key);
    }
});


// ** {{{ bespin.client.settings.ServerFile }}} **
//
// Store the settings in the file system

dojo.declare("bespin.client.settings.ServerFile", null, {
    constructor: function(parent) {
        this.parent = parent;
        this.server = bespin.get('server');
        this.settings = this.parent.defaultSettings(); // seed defaults just for now!
        this.loaded = false;

        // Load up settings from the file system
        this._load();
    },

    set: function(key, value) {
        this.settings[key] = value;

        if (key[0] != '_') this._save(); // Save back to the file system unless this is a hidden setting
    },

    get: function(key) {
        return this.settings[key];
    },

    unset: function(key) {
        delete this.settings[key];

        this._save(); // Save back to the file system
    },

    _save: function() {
        if (!this.loaded) return; // short circuit to make sure that we don't save the defaults over your settings

        var settings = "";
        for (var key in this.settings) {
            if (this.settings.hasOwnProperty(key) && (key[0] != '_')) {
                settings += key + " " + this.settings[key] + "\n";
            }
        }

        bespin.get('files').saveFile(bespin.userSettingsProject, {
            name: "settings",
            content: settings,
            timestamp: new Date().getTime()
        });
    },

    _load: function() {
        
        var self = this;

        var checkLoaded = function() {
            if (!self.loaded) { // first time load
                self.loaded = true;
                bespin.publish("settings:loaded");
            }
        };

        var loadSettings = function() {
            bespin.get('files').loadContents(bespin.userSettingsProject, "settings", function(file) {
                dojo.forEach(file.content.split(/\n/), function(setting) {
                    if (setting.match(/^\s*#/)) return; // if comments are added ignore
                    if (setting.match(/\S+\s+\S+/)) {
                        var pieces = setting.split(/\s+/);
                        self.settings[dojo.trim(pieces[0])] = dojo.trim(pieces[1]);
                    }
                });

                checkLoaded();
            }, checkLoaded); // unable to load the file, so kick this off and a save should kick in
        };

        setTimeout(loadSettings, 0);

        /*
        if (bespin.authenticated) {
            loadSettings();
        }
        else {
            bespin.subscribe("authenticated", function() {
                loadSettings();
            });
        }
        */
    }
});


// ** {{{ bespin.client.settings.DB }}} **
//
// Taken out for now to allow us to not require gears_db.js (and Gears itself).
// Experimental ability to save locally in the SQLite database.
// The plan is to migrate to ActiveRecord.js or something like it to abstract on top
// of various stores (HTML5, Gears, globalStorage, etc.)

/*
// turn off for now so we can take gears_db.js out

Bespin.Settings.DB = Class.create({
    initialize: function(parent) {
        this.parent = parent;
        this.db = new GearsDB('wideboy');

        //this.db.run('drop table settings');
        this.db.run('create table if not exists settings (' +
               'id integer primary key,' +
               'key varchar(255) unique not null,' +
               'value varchar(255) not null,' +
               'timestamp int not null)');

        this.db.run('CREATE INDEX IF NOT EXISTS settings_id_index ON settings (id)');
        bespin.publish("settings:loaded");
    },

    set: function(key, value) {
        this.db.forceRow('settings', { 'key': key, 'value': value, timestamp: new Date().getTime() }, 'key');
    },

    get: function(key) {
        var rs = this.db.run('select distinct value from settings where key = ?', [ key ]);
        try {
            if (rs && rs.isValidRow()) {
              return rs.field(0);
            }
        } catch (e) {
            console.log(e.message);
        } finally {
            rs.close();
        }
    },

    unset: function(key) {
        this.db.run('delete from settings where key = ?', [ key ]);
    },

    list: function() {
        // TODO: Need to override with browser settings
        return this.db.selectRows('settings', '1=1');
    },

    // -- Private-y
    seed: function() {
        this.db.run('delete from settings');

        // TODO: loop through the settings
        this.db.run('insert into settings (key, value, timestamp) values (?, ?, ?)', ['keybindings', 'emacs', 1183878000000]);
        this.db.run('insert into settings (key, value, timestamp) values (?, ?, ?)', ['tabsize', '2', 1183878000000]);
        this.db.run('insert into settings (key, value, timestamp) values (?, ?, ?)', ['fontsize', '10', 1183878000000]);
        this.db.run('insert into settings (key, value, timestamp) values (?, ?, ?)', ['autocomplete', 'off', 1183878000000]);
    }
});
*/

// ** {{{ bespin.client.settings.URL }}} **
//
// Grab the setting from the URL, either via # or ?   

dojo.declare("bespin.client.settings.URL", null, {
    constructor: function(queryString) {            
        this.results = dojo.queryToObject(this.stripHash(queryString || window.location.hash));
    },

    get: function(key) {
        return this.results[key];
    },

    set: function(key, value) {
        this.results[key] = value;
    },
    
    stripHash: function(url) {
        var tobe = url.split('');
        tobe.shift();
        return tobe.join('');
    }
});

// ** {{{ bespin.client.settings.Events }}} **
//
// Custom Event holder for the Settings work. 
// It deals with both settings themselves, and other events that
// settings need to watch and look for

dojo.declare("bespin.client.settings.Events", null, {
    constructor: function(settings) {
        var editSession = bespin.get('editSession');
        var editor = bespin.get('editor');

        // ** {{{ Event: settings:set }}} **
        // 
        // Watch for someone wanting to do a set operation
        bespin.subscribe("settings:set", function(event) {
            var key = event.key;
            var value = event.value;

            settings.set(key, value);
        });

        // ** {{{ Event: editor:openfile:opensuccess }}} **
        // 
        // Change the session settings when a new file is opened
        bespin.subscribe("editor:openfile:opensuccess", function(event) {
            editSession.path = event.file.name;

            settings.set('_project',  editSession.project);
            settings.set('_path',     editSession.path);
            settings.set('_username', editSession.username);

            if (editSession.syncHelper) editSession.syncHelper.syncWithServer();
        });

        // ** {{{ Event: editor:openfile:opensuccess }}} **
        // 
        // Change the syntax highlighter when a new file is opened
        bespin.subscribe("editor:openfile:opensuccess", function(event) {
            var split = event.file.name.split('.');
            var type = split[split.length - 1]; 

            if (type) {
                bespin.publish("settings:language", { language: type });
            }
        });

        // ** {{{ Event: settings:set:language }}} **
        // 
        // When the syntax setting is changed, tell the syntax system to change
        bespin.subscribe("settings:set:language", function(event) {
            bespin.publish("settings:language", { language: event.value, fromCommand: true });
        });

        // ** {{{ Event: settings:language }}} **
        // 
        // Given a new language command, change the editor.language
        bespin.subscribe("settings:language", function(event) {
            var language = event.language;
            var fromCommand = event.fromCommand;
            var languageSetting = settings.get('language') || "auto";

            if (language == editor.language) return; // already set to be that language

            if (bespin.util.include(['auto', 'on'], language)) {
                var split = window.location.hash.split('.');
                var type = split[split.length - 1];                
                if (type) editor.language = type;
            } else if (bespin.util.include(['auto', 'on'], languageSetting) || fromCommand) {
                editor.language = language;
            } else if (languageSetting == 'off') {
                editor.language = 'off';
            }
        });

        // ** {{{ Event: settings:set:collaborate }}} **
        // 
        // Turn on the collaboration system if set to be on
        bespin.subscribe("settings:set:collaborate", function(event) {
            editSession.collaborate = settings.isOn(event.value);
        });

        // ** {{{ Event: settings:set:fontsize }}} **
        // 
        // Change the font size for the editor
        bespin.subscribe("settings:set:fontsize", function(event) {
            var fontsize = parseInt(event.value);
            editor.theme.editorTextFont = editor.theme.editorTextFont.replace(/[0-9]{1,}pt/, fontsize+'pt');
            editor.theme.lineNumberFont = editor.theme.lineNumberFont.replace(/[0-9]{1,}pt/, fontsize+'pt');
        });

        // ** {{{ Event: settings:set:theme }}} **
        // 
        // Change the Theme object used by the editor
        bespin.subscribe("settings:set:theme", function(event) {
            var theme = event.value;
            
            var checkSetAndExit = function() {
                var themeSettings = bespin.themes[theme];
                if (themeSettings) {
                    if (themeSettings != editor.theme) {
                        editor.theme = themeSettings;
                        bespin.publish("settings:set:fontsize", {value: settings.get('fontsize')});
                    }
                    return true;
                }
                return false;
            }
            
            if (theme) {
                // Try to load the theme from the themes hash
                if (checkSetAndExit()) return true;

                // Not in the default themes, load from bespin.themes.ThemeName file
                try {
                    var dr = dojo.require;
                    // the build system doesn't like dynamic names.
                    dr("bespin.themes." + theme);
                    if (checkSetAndExit()) return true;
                } catch (e) {
                    //console.log(e);
                }

                // Not in bespin.themes, load from users directory
                bespin.get('files').loadContents(bespin.userSettingsProject, "/themes/" + theme + ".js", dojo.hitch(this, function(file) {
                    try {
                        eval(file.content);
                    } catch (e) {
                        //console.log(e)
                    }

                    if (!checkSetAndExit()) {                    
                        bespin.publish("message", {
                            msg: "Sorry old chap. No theme called '" + theme + "'. Fancy making it?"
                        });
                    }
                }), function() {
                    bespin.publish("message", {
                        msg: "Sorry old chap. No theme called '" + theme + "'. Fancy making it?"
                    });                    
                });
            }
        });

        // ** {{{ Event: settings:set:keybindings }}} **
        // 
        // Add in emacs key bindings
        bespin.subscribe("settings:set:keybindings", function(event) {
            var value = event.value;

            if (value == "emacs") {
                bespin.publish("editor:bindkey", {
                    modifiers: "ctrl",
                    key: "b",
                    action: "moveCursorLeft"
                });

                bespin.publish("editor:bindkey", {
                    modifiers: "ctrl",
                    key: "f",
                    action: "moveCursorRight"
                });

                bespin.publish("editor:bindkey", {
                    modifiers: "ctrl",
                    key: "p",
                    action: "moveCursorUp"
                });

                bespin.publish("editor:bindkey", {
                    modifiers: "ctrl",
                    key: "n",
                    action: "moveCursorDown"
                });

                bespin.publish("editor:bindkey", {
                    modifiers: "ctrl",
                    key: "a",
                    action: "moveToLineStart"
                });

                bespin.publish("editor:bindkey", {
                    modifiers: "ctrl",
                    key: "e",
                    action: "moveToLineEnd"
                });
            }
        });

        bespin.subscribe("settings:set:debugmode", function(event) {
            editor.debugMode = settings.isOn(event.value);

            if (editor.debugMode && bespin.debug) {
                bespin.debug.loadBreakpoints(function() {
                    editor.paint(true);
                });
            }

            editor.paint(true);
        });

        // ** {{{ Event: settings:set:cursorblink }}} **
        // 
        // The frequency of the cursor blink in milliseconds (defaults to 250)
        bespin.subscribe("settings:set:cursorblink", function(event) {
            var ms = parseInt(event.value); // get the number of milliseconds

            if (ms) {
                editor.ui.toggleCursorFrequency = ms;
            }
        });

        // ** {{{ Event: settings:set:trimonsave }}} **
        // 
        // Run the trim command before saving the file
        var _trimOnSave; // store the subscribe handler away

        bespin.subscribe("settings:set:trimonsave", function(event) {
            if (settings.isOn(event.value)) {
                _trimOnSave = bespin.subscribe("editor:savefile:before", function(event) {
                    bespin.publish("command:execute", { name: "trim" });
                });
            } else {
                bespin.unsubscribe(_trimOnSave);
            }
        });

        // ** {{{ Event: settings:set:syntaxcheck }}} **
        // 
        // Turn the syntax parser on or off
        bespin.subscribe("settings:set:syntaxcheck", function (data) {
            if (settings.isOff(data.value)) {
                bespin.publish("parser:stop");
            } else {
                bespin.publish("parser:start");
            }
        });

        // ** {{{ Event: settings:init }}} **
        // 
        // If we are opening up a new file
        bespin.subscribe("settings:init", function(event) {
            var path    = event.path;
            var project = event.project;

            if (project && (editSession.project != project)) {
                bespin.publish("project:set", { project: project });
            }

            // if this is a new file, deal with it and setup the state
            var newfile = settings.fromURL.get('new');
            if (newfile) { // scratch file
                bespin.publish("editor:newfile", {
                   project: project,
                   newfilename: path,
                   content: settings.fromURL.get('content') || " "
                });
            // existing file, so open it
            } else {
                if (path) {
                    bespin.publish("editor:openfile", { filename: path });
                }
            }
        });

        // ** {{{ Event: settings:init }}} **
        //
        // For every setting that has a bespin:settings:set:nameofsetting callback, init it
        bespin.subscribe("settings:init", function(event) {
            // Goes deep into internals which is naughty!
            // In this case, going into the Dojo saved "topics" that you subscribe/publish too
            for (var topic in dojo._topics) {
                var settingsTopicBase = "bespin:settings:set:";
                if (topic.indexOf(settingsTopicBase) == 0) {
                    var settingKey = topic.substring(settingsTopicBase.length);
                    bespin.publish("settings:set:" + settingKey, {
                        value: settings.get(settingKey)
                    });
                }
            }
        });

        // ** {{{ Event: bespin:settings:init }}} **
        //
        // Check for auto load
        bespin.subscribe("settings:init", function() {
            if (settings.isOff(settings.get('autoconfig'))) return;

            bespin.publish("editor:config:run");
        });
    }
});
