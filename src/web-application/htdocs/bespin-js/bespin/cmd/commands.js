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

dojo.provide("bespin.cmd.commands");

// = Commands =
//
// This array stores all of the default commands.

// ** {{{bespin.cmd.commands.store}}} **
//
// The core store to hold commands that others share.
bespin.cmd.commands.store = {};

// ** {{{bespin.cmd.commands.add}}} **
//
// Add the command to the store which has a name -> command hash
bespin.cmd.commands.add = function(command) {
    bespin.cmd.commands.store[command.name] = command;
};

// ** {{{bespin.cmd.commands.get}}} **
//
// Return a command from the store
bespin.cmd.commands.get = function(commandname) {
    return bespin.cmd.commands.store[commandname];
};

// ** {{{Command: bespin.cmd.commands.toArgArray}}} **
// Helper for when you have a command that needs to get a hold of it's params
// as an array for processing
bespin.cmd.commands.toArgArray = function(args) {
    if (args == null) {
        return [];
    }
    else {
        var spliten = args.split(" ");
        if (spliten.length == 1 && spliten[0] == "") {
            return [];
        }
        else {
            return spliten;
        }
    }
};

// == Start adding commands to the store ==
//

bespin.cmd.displayHelp = function(commandStore, commandLine, extra) {
    var commands = [];
    var command, name;

    if (commandStore.commands[extra]) { // caught a real command
        commands.push("<u>Help for the command: <em>" + extra + "</em></u><br/>");
        command = commandStore.commands[extra];
        commands.push(command['description'] ? command.description : command.preview);
    } else {
        var showHidden = false;

        var subcmdextra = "";
        if (commandStore.subcommandFor) subcmdextra = " for " + commandStore.subcommandFor;
        commands.push("<u>Commands Available" + subcmdextra + "</u><br/>");

        if (extra) {
            if (extra == "hidden") { // sneaky, sneaky.
                extra = "";
                showHidden = true;
            }
            commands.push("<em>(starting with</em> " + extra + " <em>)</em><br/>");
        }

        var tobesorted = [];
        for (name in commandStore.commands) {
            tobesorted.push(name);
        }

        var sorted = tobesorted.sort();

        for (var i = 0; i < sorted.length; i++) {
            name = sorted[i];
            command = commandStore.commands[name];

            if (!showHidden && command.hidden) continue;
            if (extra && name.indexOf(extra) != 0) continue;

            var args = (command.takes) ? ' [' + command.takes.order.join('] [') + ']' : '';
            commands.push('<b>' + name + args + '</b>: ' + command.preview);
        }
    }
    commandLine.showInfo("<div style='font-size: 0.80em'>" + commands.join("<br/>") + "</div>");
}

// ** {{{Command: help}}} **
bespin.cmd.commands.add({
    name: 'help',
    takes: ['search'],
    preview: 'show commands',
    description: 'The <u>help</u> gives you access to the various commands in the Bespin system.<br/><br/>You can narrow the search of a command by adding an optional search params.<br/><br/>If you pass in the magic <em>hidden</em> parameter, you will find subtle hidden commands.<br/><br/>Finally, pass in the full name of a command and you can get the full description, which you just did to see this!',
    completeText: 'optionally, narrow down the search',
    execute: function(self, extra) {
        bespin.cmd.displayHelp(self.commandStore, self, extra);
    }
});

// ** {{{Command: eval}}} **
bespin.cmd.commands.add({
    name: 'eval',
    takes: ['js-code'],
    preview: 'evals given js code and show the result',
    completeText: 'evals given js code and show the result',
    execute: function(self, jscode) {
        try {
            var result = eval(jscode);
        } catch (err) {
            var result = '<b>Error: ' + err.message + '</b>';
        }

        var msg = '';
        var type = '';

        if (dojo.isFunction(result)) {
            // converts the function to a well formated string
            msg = (result + '').replace(/\n/g, '<br>').replace(/ /g, '&#160');
            type = 'function';
        } else if (dojo.isObject(result)) {
            if (dojo.isArray(result)) {
                type = 'array'
            } else {
                type = 'object';
            }

            var items = [];
            var value;

            for (x in result) {
                if (dojo.isFunction(result[x])) {
                    value = "[function]";
                } else if (dojo.isObject(result[x])) {
                    value = "[object]";
                } else {
                    value = result[x];
                }

                items.push({name: x, value: value});
            }

            items.sort(function(a,b) {
                return (a.name.toLowerCase() < b.name.toLowerCase()) ? -1 : 1
            });

            for (var x = 0; x < items.length; x++) {
                msg += '<b>' + items[x].name + '</b>: ' + items[x].value + '<br>';
            }

        } else {
            msg = result;
            type = typeof result;
        }

        self.showInfo("<div style='font-size: 0.80em'><u>Result for eval <b>\""+jscode+"\"</b> (type: "+ type+"): </u><br><br>"+ msg + "</div>");
    }
});

// ** {{{Command: set}}} **
bespin.cmd.commands.add({
        name: 'set',
        takes: ['key', 'value'],
        preview: 'define and show settings',
        completeText: 'optionally, add a key and/or a value, else you will see all settings',
        // complete: function(self, value) {
        //     console.log(self);
        //     console.log(value);
        //     return value;
        // },
        execute: function(self, setting) {
            var output;

            if (!setting.key) { // -- show all
                var settings = self.settings.list();
                output = "<u>Your Settings</u><br/><br/>";
                dojo.forEach(settings.sort(function (a, b) { // first sort the settings based on the key
                    if (a.key < b.key) {
                        return -1;
                    } else if (a.key == b.key) {
                        return 0;
                    } else {
                        return 1;
                    }
                }), function(setting) { // now add to output unless hidden settings (start with a _)
                    if (setting.key[0] != '_') {
                        output += setting.key + ": " + setting.value + "<br/>";
                    }
                });
            } else {
                var key = setting.key;
                if (setting.value === undefined) { // show it
                    var value = self.settings.get(key);
                    if (value) {
                        output = "<u>Your setting</u><br/><br/>";
                        output += key + ": " + value;
                    } else {
                        output = "You do not have a setting for <em>" + key + "</em>";
                    }
                } else {
                    output = "<u>Saving setting</u><br/><br/>";
                    output += key + ": " + setting.value;
                    self.settings.set(key, setting.value);
                }
            }
            self.showInfo(output);
        }
});

// ** {{{Command: unset}}} **
bespin.cmd.commands.add({
        name: 'unset',
        takes: ['key'],
        preview: 'unset a setting entirely',
        completeText: 'add a key for the setting to delete entirely',
        execute: function(self, key) {
            self.settings.unset(key);
            self.showInfo("Unset the setting for " + key + ".");
        }
});

// ** {{{Command: search}}} **
bespin.cmd.commands.add({
        name: 'search',
        takes: ['searchString'],
        preview: 'searches the current file for the given searchString',
        completeText: 'type in a string to search',
        execute: function(self, str) {
            bespin.get('actions').startSearch(str, 'commandLine');
        }
});

// ** {{{Command: files (ls, list)}}} **
bespin.cmd.commands.add({
    name: 'files',
    aliases: ['ls', 'list'],
    takes: ['project'],
    preview: 'show files',
    completeText: 'optionally, add the project name of your choice',
    execute: function(self, project) {
        if (!project) {
            bespin.withComponent('editSession', function(editSession) {
                project = editSession.project;
            });
        }

        if (!project) {
            self.showInfo("You need to pass in a project");
            return;
        }

        self.files.fileNames(project, function(fileNames) {
            var files = "<u>Files in project: " + project + "</u><br/><br/>";
            for (var x = 0; x < fileNames.length; x++) {
                files += fileNames[x].name + "<br/>";
            }
            self.showInfo(files);
        });
    }
});

// ** {{{Command: status}}} **
bespin.cmd.commands.add({
    name: 'status',
    preview: 'get info on the current project and file',
    execute: function(self) {
        bespin.publish("session:status");
    }
});

// ** {{{Command: project}}} **
bespin.cmd.commands.add({
    name: 'project',
    takes: ['projectname'],
    preview: 'show the current project, or set to a new one',
    completeText: 'optionally, add the project name to change to that project',
    execute: function(self, projectname) {
        if (projectname) {
            bespin.publish("project:set", { project: projectname });
        } else {
            self.executeCommand('status');
        }
    }
});

// ** {{{Command: projects}}} **
bespin.cmd.commands.add({
    name: 'projects',
    preview: 'show projects',
    execute: function(self, extra) {
      self.files.projects(function(projectNames) {
          var projects = "<u>Your projects</u><br/><br/>";
          for (var x = 0; x < projectNames.length; x++) {
            projects += projectNames[x].name + "<br/>";
          }
          self.showInfo(projects);
      });
    }
});

// ** {{{Command: createproject}}} **
bespin.cmd.commands.add({
    name: 'createproject',
    takes: ['projectname'],
    preview: 'create a new project',
    usage: '[newprojectname]',
    execute: function(self, projectname) {
        if (!projectname) {
            self.showUsage(this);
            return;
        }
        bespin.publish("project:create", { project: projectname });
    }
});

// ** {{{Command: createproject}}} **
bespin.cmd.commands.add({
    name: 'deleteproject',
    takes: ['projectname'],
    preview: 'delete a project',
    usage: '[projectname]',
    execute: function(self, projectname) {
        if (!projectname) {
            self.showUsage(this);
            return;
        }
        bespin.publish("project:delete", { project: projectname });
    }
});

// ** {{{Command: renameproject}}} **
bespin.cmd.commands.add({
    name: 'renameproject',
    takes: ['currentProject', 'newProject'],
    preview: 'rename a project',
    usage: '[currentProject], [newProject]',
    execute: function(self, args) {
        if (!args.currentProject || !args.newProject) {
            self.showUsage(this);
            return;
        }
        bespin.publish("project:rename", { currentProject: args.currentProject, newProject: args.newProject });
    }
});

// ** {{{Command: mkdir}}} **
bespin.cmd.commands.add({
    name: 'mkdir',
    takes: ['path', 'projectname'],
    preview: 'create a new directory in the given project',
    usage: '[path] [projectname]',
    execute: function(self, args) {
        if (!args.path) {
            self.showUsage(this);
            return;
        }

        var opts = { path: args.path };
        if (args.projectname) opts.project = args.projectname;

        bespin.publish("directory:create", opts);
    }
});

// ** {{{Command: save}}} **
bespin.cmd.commands.add({
    name: 'save',
    takes: ['filename'],
    preview: 'save the current contents',
    completeText: 'add the filename to save as, or use the current file',
    withKey: "CMD S",
    execute: function(self, filename) {
        bespin.publish("editor:savefile", {
            filename: filename
        });
    }
});

// ** {{{Command: load (open)}}} **
bespin.cmd.commands.add({
    name: 'load',
    aliases: ['open'],
    takes: ['filename', 'project'],
    preview: 'load up the contents of the file',
    completeText: 'add the filename to open',
    execute: function(self, opts) {
        bespin.publish("editor:openfile", opts);
    }
});

// ** {{{Command: preview}}} **
bespin.cmd.commands.add({
    name: 'preview',
    takes: ['filename'],
    preview: 'view the file in a new browser window',
    completeText: 'add the filename to view or use the current file',
    execute: function(self, filename) {
        bespin.publish("editor:preview", {
            filename: filename
        });
    }
});

// ** {{{Command: editconfig}}} **
bespin.cmd.commands.add({
    name: 'editconfig',
    aliases: ['config'],
    preview: 'load up the config file',
    execute: function(self) {
        bespin.publish("editor:config:edit");
    }
});

// ** {{{Command: runconfig}}} **
bespin.cmd.commands.add({
    name: 'runconfig',
    preview: 'run your config file',
    execute: function(self) {
        bespin.publish("editor:config:run");
    }
});

// ** {{{Command: cmdload}}} **
bespin.cmd.commands.add({
    name: 'cmdload',
    takes: ['commandname'],
    preview: 'load up a new command',
    completeText: 'command name to load (required)',
    usage: '[commandname]: Command name required.',
    execute: function(self, commandname) {
        if (!commandname) {
            self.showUsage(this);
            return;
        }
        bespin.publish("command:load", { commandname: commandname });
    }
});

// ** {{{Command: cmdedit}}} **
bespin.cmd.commands.add({
    name: 'cmdedit',
    takes: ['commandname'],
    aliases: ['cmdadd'],
    preview: 'edit the given command (force if doesn\'t exist',
    completeText: 'command name to edit (required)',
    usage: '[commandname]: Command name required.',
    execute: function(self, commandname) {
        if (!commandname) {
            self.showUsage(this);
            return;
        }

        bespin.publish("command:edit", { commandname: commandname });
    }
});

// ** {{{Command: cmdlist}}} **
bespin.cmd.commands.add({
    name: 'cmdlist',
    preview: 'list my custom commands',
    execute: function(self) {
        bespin.publish("command:list");
    }
});

// ** {{{Command: cmdrm}}} **
bespin.cmd.commands.add({
    name: 'cmdrm',
    takes: ['commandname'],
    preview: 'delete a custom command',
    completeText: 'command name to delete (required)',
    usage: '[commandname]: Command name required.',
    execute: function(self, commandname) {
        if (!commandname) {
            self.showUsage(this);
            return;
        }

        bespin.publish("command:delete", { commandname: commandname });
    }
});

// ** {{{Command: newfile}}} **
bespin.cmd.commands.add({
    name: 'newfile',
    //aliases: ['new'],
    takes: ['filename', 'project'],
    preview: 'create a new buffer for file',
    completeText: 'optionally, name the new filename first, and then the name of the project second',
    withKey: "CTRL SHIFT N",
    execute: function(self, args) {
        if (args.filename) {
            args.newfilename = args.filename;
            delete args.filename;
        }
        bespin.publish("editor:newfile", args || {});
    }
});

// ** {{{Command: rm (remove, del)}}} **
bespin.cmd.commands.add({
    name: 'rm',
    aliases: ['remove', 'del'],
    takes: ['filename', 'project'],
    preview: 'remove the file',
    completeText: 'add the filename to remove, and optionally a specific project at the end',
    execute: function(self, args) {
        var project = args.project || bespin.get('editSession').project;
        var filename = args.filename;

        if (!project) {
            self.showInfo("rm only works with the project is set.");
            return;
        }

        if (!filename) {
            self.showInfo("give me a filename or directory to delete");
            return;
        }

        self.files.removeFile(project, filename, function() {
            if (bespin.get('editSession').checkSameFile(project, filename)) self.editor.model.clear(); // only clear if deleting the same file

            self.showInfo('Removed file: ' + filename, true);
        }, function(xhr) {
            self.showInfo("Wasn't able to remove the file <b>" + filename + "</b><br/><em>Error</em> (probably doesn't exist): " + xhr.responseText);
        });
    }
});

// ** {{{Command: closefile}}} **
bespin.cmd.commands.add({
    name: 'closefile',
    takes: ['filename', 'project'],
    preview: 'close the file (may lose edits)',
    completeText: 'add the filename to close (defaults to this file).<br>also, optional project name.',
    execute: function(self, args) {
        bespin.publish("editor:closefile", args);
    }
});

// ** {{{Command: dashboard}}} **
bespin.cmd.commands.add({
    name: 'dashboard',
    preview: 'navigate to the file',
    execute: function(self) {
        bespin.util.navigate.dashboard();
    }
});

// ** {{{Command: version}}} **
bespin.cmd.commands.add({
    name: 'version',
    takes: ['command'],
    preview: 'show the version for Bespin or a command',
    completeText: 'optionally, a command name',
    execute: function(self, command) {
        var bespinVersion = 'Your Bespin is at version ' + bespin.versionNumber + ', Code name: "' + bespin.versionCodename + '"';
        var version;
        if (command) {
            var theCommand = self.commandStore.commands[command];
            if (!theCommand) {
                version = "It appears that there is no command named '" + command + "', but " + bespinVersion;
            } else {
                version = (theCommand.version)
                    ? "The command named '" + command + "' is at version " + theCommand.version
                    : "The command named '" + command + "' is a core command in Bespin version " + bespin.versionNumber;
            }
        }
        else {
            version = bespinVersion;
        }
        self.showInfo(version);
    }
});

// ** {{{Command: clear}}} **
bespin.cmd.commands.add({
    name: 'clear',
    aliases: ['cls'],
    preview: 'clear the file',
    execute: function(self) {
        self.editor.model.clear();
    }
});

// ** {{{Command: goto}}} **
(function () {
var previewFull      = 'move it! make the editor head to a line number or a function name.';
var preview          = 'move it! make the editor head to a line number.';
var completeTextFull = 'add the line number to move to, or the name of a function in the file';
var completeText     = 'add the line number to move to in the file';
var gotoCmd = {
    name: 'goto',
    takes: ['value'],
    preview: previewFull,
    completeText: completeTextFull,
    execute: function(self, value) {
        var settings = bespin.get("settings")
        if (value) {
            var linenum = parseInt(value, 10); // parse the line number as a decimal

            if (isNaN(linenum)) { // it's not a number, so for now it is a function name
                if(settings.isOn(settings.get("syntaxcheck"))) {
                    bespin.publish("parser:gotofunction", {
                        functionName: value
                    });
                } else {
                    bespin.publish("message", { msg: "Please enter a valid line number." })
                }
            } else {
                bespin.publish("editor:moveandcenter", {
                    row: linenum
                });
            }
        }
    }
}
bespin.cmd.commands.add(gotoCmd);
bespin.subscribe("settings:set:syntaxcheck", function () {
    var settings = bespin.get("settings")
    if(settings.isOn(settings.get("syntaxcheck"))) {
        gotoCmd.preview = previewFull;
        gotoCmd.completeText = completeTextFull;
    } else {
        gotoCmd.preview = preview;
        gotoCmd.completeText = completeText;
    }
})
})()

// ** {{{Command: replace}}} **
bespin.cmd.commands.add({
    name: 'replace',
    takes: ['search', 'replace'],
    preview: 's/foo/bar/g',
    completeText: 'add the search regex, and then the replacement text',
    execute: function(self, args) {
        self.editor.model.replace(args.search, args.replace);
    }
});

// ** {{{Command: login}}} **
bespin.cmd.commands.add({
    name: 'login',
    // aliases: ['user'],
    //            takes: ['username', 'password'],
    hidden: true,
    takes: {
        order: ['username', 'password'],
        username: {
            "short": 'u'
        },
        password: {
            "short": 'p',
            optional: true
        }
    },
    preview: 'login to the service',
    completeText: 'pass in your username and password',
    execute: function(self, args) {
        if (!args) { // short circuit if no username
            self.executeCommand("status");
            return;
        }
        bespin.get('editSession').username = args.user; // TODO: normalize syncing
        bespin.get('server').login(args.user, args.pass);
    }
});

// ** {{{Command: logout}}} **
bespin.cmd.commands.add({
    name: 'logout',
    preview: 'log out',
    execute: function(self) {
        delete bespin.get('editSession').username;
        bespin.get('server').logout(function() {
			window.location.href="/";
		});
    }
});

// ** {{{Command: bespin}}} **
bespin.cmd.commands.add({
    name: 'bespin',
    preview: 'has',
    hidden: true,
    messages: [
        "really wants you to trick it out in some way.",
        "is your Web editor.",
        "would love to be like Emacs on the Web.",
        "is written on the Web platform, so you can tweak it."
    ],
    execute: function(self) {
        self.showInfo("Bespin " + this.messages[Math.floor(Math.random() * this.messages.length)]);
    }
});

// ** {{{Command: action}}} **
bespin.cmd.commands.add({
    name: 'action',
    takes: ['actionname'],
    preview: 'execute any editor action',
    hidden: true,
    execute: function(self, actionname) {
        bespin.publish("editor:doaction", {
            action: actionname
        });
    }
});

// ** {{{Command: sort}}} **
bespin.cmd.commands.add({
    name: 'sort',
    takes: ['direction'],
    preview: 'sort the current buffer',
    completeText: 'optionally, sort descending',
    execute: function(self, direction) {
        var buffer = self.editor.model.getDocument().split(/\n/);
        buffer.sort();
        if (direction && /^desc/.test(direction.toLowerCase())) buffer.reverse();
        self.editor.model.insertDocument(buffer.join("\n"));
    }
});

// ** {{{Command: quota}}} **
bespin.cmd.commands.add({
    name: 'quota',
    preview: 'show your quota info',
    megabytes: function(bytes) {
        return (bytes / 1024 / 1024).toFixed(2);
    },
    execute: function(self) {
        var editSession = bespin.get('editSession');
        self.showInfo("You have " + this.megabytes(editSession.quota - editSession.amountUsed) + " MB free space to put some great code!<br><br> <em style='font-size: smaller'>Used " + this.megabytes(editSession.amountUsed) + " MB out of your " + this.megabytes(editSession.quota) + " MB quota</em>");
    }
});

// ** {{{Command: export}}} **
bespin.cmd.commands.add({
    name: 'export',
    takes: ['project', 'archivetype'],
    preview: 'export the given project with an archivetype of zip or tgz',
    completeText: 'project name, archivetype (zip | tgz, defaults to zip)',
    execute: function(self, args) {
        var project = args.project || bespin.get('editSession').project;

        var type = args.archivetype;
        if (!bespin.util.include(['zip','tgz','tar.gz'], type)) {
            type = 'zip';
        }

        bespin.get('server').exportProject(project, type); // try to do it via the iframe
    }
});

// ** {{{Command: import}}} **
bespin.cmd.commands.add({
    name: 'import',
    takes: ['url', 'project'],
    preview: 'import the given url as a project.<br>If a project name isn\'t given it will use the filename<br>If no URL is given to import, a file upload box will be shown to import.',
    completeText: 'url (to an archive zip | tgz) and/or project name',
    usage: "[url of archive] [projectname]<br><br><em>If only a URL is given, the projectname will be implied<br><br>If only a project name is given, a file upload window will be shown to upload.</em>",
    // ** {{{calculateProjectName}}}
    //
    // Given a URL, work out the project name as a default
    // For example, given http://foo.com/path/to/myproject.zip
    // return "myproject"
    calculateProjectName: function(url) {
        var split = url.split('/');
        var projectMaker = split[split.length - 1].split(".");
        projectMaker.pop();
        return projectMaker.join("_");
    },
    // ** {{{isURL}}}
    //
    // Test the given string to return if it is a URL.
    // In this context it has to be http(s) only
    isURL: function(url) {
        return (url && (/^http(:|s:)/.test(url)));
    },
    upload: function(project) {
        // use the center popup and inject a form in that points to the right place.
        var el = dojo.byId('centerpopup');

        el.innerHTML = "<div id='upload-container'><form method='POST' name='upload' id='upload' enctype='multipart/form-data'><div id='upload-header'>Import project via upload <img id='upload-close' src='images/icn_close_x.png' align='right'></div><div id='upload-content'><div id='upload-status'></div><p>Browse to find the project archive that you wish to archive<br>and then click on the <code>Upload</code> button.</p><center><input type='file' id='filedata' name='filedata' accept='application/zip,application/x-gzip'> <input type='submit' value='Upload'></center></div></form></div>";

        dojo.require("dijit._base.place");
        dojo.require("bespin.util.webpieces");

        dojo.require("dojo.io.iframe");

        dojo.connect(dojo.byId('upload'), "submit", function() {
            dojo.byId('upload-status').innerHTML = 'Importing file into new project ' + project;
            dojo.io.iframe.send({
                url: '/project/import/' + project,
                form: dojo.byId('upload'),
                method: 'POST',
                handleAs: 'text',
                preventCache: true,
                contentType: "multipart/form-data",
                load: function(data, ioArg) {
                    dojo.byId('upload-status').innerHTML = 'Thanks for uploading the file!';
                },
                error: function(error, ioArg) {
                    setTimeout(function() {
                        bespin.get('files').projects(function(projectNames) {
                            if (dojo.some(projectNames, function(testProject) { return project + '/' == testProject.name; })) {
                                dojo.byId('upload-status').innerHTML = 'Archive imported and project ' + project + ' has been created!';
                            } else {
                                dojo.byId('upload-status').innerHTML = 'Error uploading the file. Sorry, try again!';
                            }
                        });
                    }, 100);
                }
            });
        });

        bespin.util.webpieces.showCenterPopup(el, true);

        // TODO: refactor this block into webpieces if popup is modal
        // pass the uploadClose DOM element as parameter to showCenterPopup
        var uploadClose, overlay;
        var hideCenterPopup = function(){
            el.removeChild(el.firstChild);
            bespin.util.webpieces.hideCenterPopup(el);
            dojo.disconnect(uploadClose);
            dojo.disconnect(overlay);
        };
        uploadClose = dojo.connect(dojo.byId("upload-close"), "onclick", hideCenterPopup);
        overlay = dojo.connect(dojo.byId("overlay"), "onclick", hideCenterPopup);
    },

    // ** {{{execute}}}
    //
    // Can be called in three ways:
    //
    // * import http://foo.com/path/to/archive.zip
    // * import http://foo.com/path/to/archive.zip projectName
    // * import projectName http://foo.com/path/to/archive.zip
    execute: function(self, args) {
        var project, url;

        // Fail fast. Nothing given?
        if (!args.url) {
            self.showUsage(this);
            return;
        // * checking - import http://foo.com/path/to/archive.zip
        } else if (!args.project && this.isURL(args.url)) {
            args.project = this.calculateProjectName(args.url);
        // * Oops, project and url are the wrong way around. That's fine
        } else if (this.isURL(args.project)) {
            project = args.project;
            url = args.url;
            args.project = url;
            args.url = project;
        // * Make sure that a URL came along at some point, else call up an upload box
        } else if (!this.isURL(args.url)) {
            var project = args.url; // only a project has been passed in
            this.upload(project);
        } else {
        // * A project and URL are here and available to do a URL based import
            project = args.project;
            url = args.url;

            self.showInfo("About to import " + project + " from:<br><br>" + url + "<br><br><em>It can take awhile to download the project, so be patient!</em>");

            bespin.publish("project:import", { project: project, url: url });
        }
    }
});

// ** {{{Command: trim}}} **
bespin.cmd.commands.add({
    name: 'trim',
    takes: ['side'], // left, right, both
    preview: 'trim trailing or leading whitespace',
    completeText: 'optionally, give a side of left, right, or both (defaults to right)',
    execute: function(self, side) {
        self.editor.model.changeEachRow(function(row) {
            if (!side) side = "right";

            if (bespin.util.include(["left", "both"], side)) {
                while (row[0] == ' ') {
                    row.shift();
                }
            }

            if (bespin.util.include(["right", "both"], side)) {
                var i = row.length - 1;

                while (row[i] == ' ') {
                    delete row[i];
                    i--;
                }
            }
            return bespin.util.shrinkArray(row);
        });
    }
});

// ** {{{Command: bindkey}}} **
bespin.cmd.commands.add({
    name: 'bindkey',
    takes: ['modifiers', 'key', 'action'],
    preview: 'Bind a key to an action, or show bindings',
    completeText: 'With no arguments show bindings, else give modifier(s), key, and action name to set',
    execute: function(self, args) {
        if (args.key && args.action) { // bind a new key binding
            if (args.modifiers == "none") args.modifiers = '';

            bespin.publish("editor:bindkey", args);
        } else { // show me the key bindings
            var descriptions = bespin.get('editor').editorKeyListener.keyMapDescriptions;
            var output = "<u>Your Key Bindings</u><br><br><table>";

            for (var keys in descriptions) {
                var keyData = keys.split(','); // metaKey, ctrlKey, altKey, shiftKey
                var keyCode = parseInt(keyData[0]);

                var modifiers = [];
                if (keyData[1] === "true") modifiers.push("CMD");
                if (keyData[2] === "true") modifiers.push("CTRL");
                if (keyData[3] === "true") modifiers.push("ALT");
                if (keyData[4] === "true") modifiers.push("SHIFT");

                var modifierInfo = modifiers.length > 0 ? modifiers.join(', ') + " " : "";
                var keyInfo = modifierInfo + bespin.util.keys.KeyCodeToName[keyCode] || keyCode;
                output += "<tr style='font-size: x-small'><td style='color: #eee; padding-right: 20px;'>" + keyInfo + "</td><td>" + descriptions[keys] + "</td></tr>";
            }
            output += "</table>";
            self.showInfo(output);
        }
    }
});

// ** {{{Command: insert}}} **
bespin.cmd.commands.add({
    name: 'insert',
    takes: ['text'],
    preview: 'insert the given text at this point.',
    hidden: true,
    execute: function(self, text) {
        self.editor.model.insertChunk(self.editor.cursorPosition, text);
    }
});

// ** {{{Command: typingtest}}} **
bespin.cmd.commands.add({
    name: 'typingtest',
    preview: 'type in the alphabet a few times',
    hidden: true,
    execute: function(self) {
        var start = Date.now();

        for (var i = 0; i < 3; i++) {
            dojo.forEach(['a', 'b', 'c', 'd', 'e', 'f', 'g', 'h', 'i', 'j', 'k', 'l', 'm', 'n', 'o', 'p', 'q', 'r', 's', 't', 'u', 'v', 'w', 'x', 'y', 'z'], function(c) {
                var args = { pos: bespin.editor.utils.copyPos(self.editor.cursorPosition) };
                args.newchar = c;
                self.editor.ui.actions.insertCharacter(args);
            });
        }

        var stop = Date.now();

        self.showInfo("It took " + (stop - start) + " milliseconds to do this");
    }
});

// ** {{{Command: template}}} **
bespin.cmd.commands.add({
    name: 'template',
    takes: ['type'],
    preview: 'insert templates',
    completeText: 'pass in the template name',
    templates: { 'in': "for (var key in object) {\n\n}"},
    execute: function(cmdline, type) {
        cmdline.editor.model.insertChunk(cmdline.editor.cursorPosition, this.templates[type]);
    }
});

// ** {{{Command: alias}}} **
bespin.cmd.commands.add({
    name: 'alias',
    takes: ['alias', 'command'],
    preview: 'define and show aliases for commands',
    completeText: 'optionally, add your alias name, and then the command name',
    execute: function(self, args) {
      var output;
      var aliases = self.commandStore.aliases;

      if (!args.alias) { // -- show all
        output = "<u>Your Aliases</u><br/><br/>";
        for (var x in aliases) {
          output += x + ": " + aliases[x] + "<br/>";
        }
      } else {
        if (args.command === undefined) { // show it
          output = "<u>Your alias</u><br/><br/>";
          var alias = aliases[args.alias];
          if (alias) {
              output += args.alias + ": " + aliases[args.alias];
          } else {
              output += "No alias set for " + args.alias;
          }
        } else { // save a new alias
          var key = args.alias;
          var value = args.command;
          var aliascmd = value.split(' ')[0];

          output = "<u>Saving setting</u><br/><br/>";
          if (self.commandStore.commands[key]) {
              output += "Sorry, there is already a command with the name: " + key;
          } else if (self.commandStore.commands[aliascmd]) {
              output += key + ": " + value;
              aliases[key] = value;
          } else if (aliases[aliascmd]) { // TODO: have the symlink to the alias not the end point
              output += key + ": " + aliases[value] + " (" + value + " was an alias itself)";
              aliases[key] = value;
          } else {
              output += "Sorry, no command or alias with that name.";
          }
        }
      }
      self.showInfo(output);
    }
});

// ** {{{Command: history}}} **
bespin.cmd.commands.add({
    name: 'history',
    preview: 'show history of the commands',
    execute: function(self) {
        self.showInfo('<u>Command History</u><br/><br/>' + self.commandLineHistory.history.join('<br/>'));
    }
});

// ** {{{Command: use}}} **
bespin.cmd.commands.add({
    name: 'use',
    takes: ['type'],
    preview: 'use patterns to bring in code',
    completeText: '"sound" will add sound support',
    libnames: {
        'jquery': 'jquery.min.js'
    },
    execute: function(self, type) {
        if (type == 'sound') {
            self.editor.model.insertChunk({ row: 3, col: 0 },
                '  <script type="text/javascript" src="soundmanager2.js"></script>\n');
            self.editor.model.insertChunk({ row: 4, col: 0 },
                "  <script>\n  var sound; \n  soundManager.onload = function() {\n    sound =  soundManager.createSound('mySound','/path/to/mysoundfile.mp3');\n  }\n  </script>\n");
        } else if (type == 'js') {
            var jslib = 'http://ajax.googleapis.com/ajax/libs/jquery/1.2.6/jquery.min.js';
            var script = '<script type="text/javascript" src="' + jslib + '"></script>\n';
            self.editor.model.insertChunk({ row: 3, col: 0 }, script);
        }
    }
});

//** {{{Command: outline}}} **
bespin.cmd.commands.add({
    name: 'outline',
    preview: 'show outline of source code',
    withKey: "ALT SHIFT O",
    execute: function(self) {
        var settings = bespin.get("settings");
        if(settings.isOff(settings.get("syntaxcheck"))) {
            bespin.publish("message", { msg: "Please enable the syntaxcheck feature with 'set syntaxcheck on' to activate the outline view." });
        } else {
            bespin.publish("parser:showoutline");
        }
    }
});

// ** {{{Command: follow}}} **
bespin.cmd.commands.add({
    name: 'follow',
    takes: ['username ...'],
    preview: 'add to the list of users we are following, or (with no args) list the current set',
    completeText: 'username(s) of person(s) to follow',
    usage: "[username] ...<br><br><em>(username optional. Will list current followed users if not provided)</em>",
    // ** {{{execute}}}
    execute: function(self, args) {
        var usernames = bespin.cmd.commands.toArgArray(args);
        if (usernames.length == 0) {
            bespin.publish("network:followers");
        }
        else {
            bespin.publish("network:follow", [ usernames ]);
        }
    }
});

// ** {{{Command: unfollow}}} **
bespin.cmd.commands.add({
    name: 'unfollow',
    takes: ['username ...'],
    preview: 'remove from the list of users we are following',
    completeText: 'username(s) of person(s) to stop following',
    usage: "[username] ...<br><br><em>The username(s) to stop following</em>",
    // ** {{{execute}}}
    execute: function(self, args) {
        if (args.pieces.length == 0) {
            self.showInfo('Please specify the users to cease following');
        }
        else {
            bespin.publish("network:unfollow", [ args.pieces ]);
        }
    }
});

// ** {{{Command: group}}} **
bespin.cmd.commands.add({
    name: 'group',
    preview: 'Collect the people you follow into groups, and display the existing groups',
    // ** {{{execute}}}
    execute: function(self, args) {
        args = args.pieces;

        if (args.length == 0) {
            bespin.publish("groups:list:all");
        }
        else if (args.length == 1) {
            bespin.publish("groups:list", [ args[0] ]);
        }
        else if (args.length == 2) {
            if (args[1] == "-r" || args[1] == "--remove") {
                bespin.publish("groups:remove:all", [ args[0] ]);
            }
            else {
                self.showInfo('Syntax error - You must specify what you want to do with your group.');
            }
        }
        else if (args.length > 2) {
            var group = args.shift();
            var command = args.shift();
            if (command == "-a" || command == "--add") {
                bespin.publish("groups:add", [ group, args ]);
            }
            else if (command == "-r" || command == "--remove") {
                args.shift();
                bespin.publish("groups:remove", [ group, args ]);
            }
            else {
                self.showInfo('Syntax error - To manipulate a group you must use add/remove');
            }
        }
    }
});

// ** {{{Command: share}}} **
bespin.cmd.commands.add({
    name: 'share',
    takes:[ '{project}', '{user}|{group}|everyone', 'readonely|edit', 'loadany' ],
    preview: 'List and alter sharing for a project',
    // ** {{{execute}}}
    execute: function(self, args) {
        args = args.pieces;

        if (args.length == 0) {
            // i.e. 'share'
            bespin.publish("share:list:all");
        }
        else if (args.length == 1) {
            // i.e. 'share {project}'
            bespin.publish("share:list:project", [ args[0] ]);
        }
        else if (args.length == 2) {
            if (args[1] == "none") {
                // i.e. 'share {project} none'
                bespin.publish("share:remove:all", [ args[0] ]);
            }
            else {
                // i.e. 'share {project} {user}|{group}|everyone'
                bespin.publish("share:list:project:member", [ args[0], args[1] ]);
            }
        }
        else if (args.length == 3) {
            if (args[2] == "none") {
                // i.e. 'share {project} {user}|{group}|everyone none'
                bespin.publish("share:remove", [ args[0], args[1] ]);
            }
            else if (args[2] != "readonly" && args[2] != "edit") {
                this._syntaxError('Valid edit options are \'none\', \'readonly\' or \'edit\'.');
            }
            else {
                // i.e. 'share {project} {user}|{group}|everyone [readonly|edit]'
                bespin.publish("share:add", [ args[0], args[1], [ args[2] ] ]);
            }
        }
        else if (args.length == 4) {
            if (args[3] != "loadany") {
                this._syntaxError('Valid scope options are loadany or <blank>');
            }
            else if (args[2] != "readonly" && args[2] != "edit") {
                this._syntaxError('Valid edit options are \'readonly\' or \'edit\'.');
            }
            else {
                // i.e. 'share {project} {user}|{group}|everyone [readonly|edit] loadany'
                bespin.publish("share:add", [ args[0], args[1], [ args[2], args[3] ] ]);
            }
        }
        else {
            this._syntaxError('Too many arguments. Maximum 4 arguments to \'share\' command.');
        }
    },
    _syntaxError: function(message) {
        self.showInfo('Syntax error - share {project} ({user}|{group}|everyone) (none|readonly|edit) [loadany]');
    }
});

// ** {{{Command: viewme}}} **
bespin.cmd.commands.add({
    name: 'viewme',
    preview: 'List and alter user\'s ability to see what I\'m working on',
    // ** {{{execute}}}
    execute: function(self, args) {
        args = bespin.cmd.commands.toArgArray(args);

        if (args.length == 0) {
            // i.e. 'viewme'
            bespin.publish("viewme:list:all");
        }
        else if (args.length == 1) {
            // i.e. 'viewme {user|group}'
            bespin.publish("viewme:list", [ args[0] ]);
        }
        else if (args.length == 2) {
            if (args[1] != 'false' && args[1] != 'true' && args[1] != 'default') {
                this._syntaxError('Valid viewme settings are {true|false|deafult}');
            }
            else {
                bespin.publish("viewme:set", [ args[0], args[1] ]);
            }
        }
        else {
            this._syntaxError('Too many arguments. Maximum 2 arguments to \'viewme\' command.');
        }
    },
    _syntaxError: function(message) {
        self.showInfo('Syntax error - viewme ({user}|{group}|everyone) (true|false|default)');
    }
});

// ** {{{Command: test}}} **
bespin.cmd.commands.add({
    name: 'test',
    preview: 'Run some automated end to end tests',
    script: [
        { send:"echo Starting", expect:/^Starting$/ },
        { send:"follow", expect:/sds/ },
        { send:"echo Finished", expect:/^Finished$/ }
    ],
    // ** {{{_setup}}}
    _setup: function(self, onComplete) {
        this.originalShowInfo = self.showInfo;
        var that = this;
        bespin.get('server').request('POST', '/test/setup/', null, {
            onSuccess: onSuccess,
            onFailure: function(xhr) {
                that._cleanup(self, "_setup() failed. Maybe due to: " + xhr.responseText);
            }
        });
    },
    // ** {{{_cleanup}}}
    _cleanup: function(self, reason) {
        self.showInfo = this.originalShowInfo;
        self.showInfo(reason);
        bespin.get('server').request('POST', '/test/cleanup/', null, {
            onSuccess: function() {
                console.log("Server cleanup completed");
            },
            onFailure: function(xhr) {
                self.showInfo("_setup() failed. Maybe due to: " + xhr.responseText);
            }
        });
    },
    // ** {{{_runNextElement}}}
    _runNextElement: function(self, script, index) {
        console.log("_runNextElement", index);
        if (index >= script.length) {
            this._cleanup(self, "Finished running tests");
            return;
        }
        var element = script[index];
        var that = this;
        self.showInfo = function(html, autohide) {
            var info = dojo.byId('info');
            info.innerHTML = html;
            var text = info.textContent;
            if (element.expect.test(text)) {
                that._runNextElement(self, script, index + 1);
            }
            else {
                console.error("Test failure at index:", index);
                console.log("Command: ", element.send);
                console.log("Expected: ", element.expect.source);
                console.log("Received:", text);
                that._cleanup(self, "Test failure at index: " + index + "<br/>Command: '" + element.send + "'<br/>Expected: /" + element.expect.source + "/<br/>Received: '" + text + "'");
            }
        };
        self.executeCommand(element.send);
    },
    // ** {{{execute}}}
    execute: function(self) {
        var that = this;
        this._setup(self, function() {
            that._runNextElement(self, that.script, 0);
        });
    }
});

// ** {{{Command: echo}}} **
bespin.cmd.commands.add({
    name: 'echo',
    takes: ['message ...'],
    preview: 'A test echo command',
    // ** {{{execute}}}
    execute: function(self, args) {
        self.showInfo(args);
    }
});

bespin.cmd.commands.add({
	name: 'uc',
	preview: 'Change all selected text to uppercase',
	withKey: "CMD SHIFT U",
	execute: function(self) {
		var args = { stringCase: 'u' };
		self.editor.ui.actions.selectionChangeCase(args);
	}
});

bespin.cmd.commands.add({
	name: 'lc',
	preview: 'Change all selected text to lowercase',
	withKey: "CMD SHIFT L",
	execute: function(self) {
		var args = { stringCase: 'l' };
		self.editor.ui.actions.selectionChangeCase(args);
	}
});


