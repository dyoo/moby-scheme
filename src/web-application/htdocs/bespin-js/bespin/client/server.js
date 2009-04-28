dojo.provide("bespin.client.server");
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

// = Server =
//
// The Server object implements the [[https://wiki.mozilla.org/BespinServerAPI|Bespin Server API]]
// giving the client access to the backend store. The {{{FileSystem}}} object uses this to talk back.

dojo.declare("bespin.client.Server", null, {
    // ** {{{ initialize(base) }}}
    //
    // Object creation initialization
    //
    // * {{{base}}} is the base server URL to access
    constructor: function(base) {
        this.SERVER_BASE_URL = base || '.';
        this._asyncCount = 0;
    },

    // == Helpers ==

    // ** {{{ request(method, url, payload, callbackOptions) }}}
    //
    // The core way to access the backend system.
    // Similar to the Prototype Ajax.Request wrapper 
    //
    // * {{{method}}} is the HTTP method (GET|POST|PUT|DELETE)
    // * {{{url}}} is the sub url to hit (after the base url)
    // * {{{payload}}} is what to send up for POST requests
    // * {{{options}}} is how you pass in various callbacks.
    //   options['evalJSON'] = true or false to auto eval
    //   options['onSuccess'] = the main success callback
    //   options['onFailure'] = call for general failures
    //   options['on' + STATUS CODE] = call for specific failures
    //   options['log'] = just log the following
    request: function(method, url, payload, options) {
        var server = this;
        var xhr = new XMLHttpRequest();

        if (location.href.indexOf("file:") == 0){ // if developing and using this locally only!
           try {
               if (netscape.security.PrivilegeManager.enablePrivilege) {
                   netscape.security.PrivilegeManager.enablePrivilege('UniversalBrowserRead');
               }
           } catch (ex) {
           }
        }

        if (options) { // do it async (best)
            xhr.onreadystatechange = function() {
                if (xhr.readyState == 4) {
                    if (xhr.status && xhr.status != 0 && (xhr.status >= 200 && xhr.status < 300)) {
                        var response = xhr.responseText;
                        
                        if (options['evalJSON'] && response) {
                            try {
                                response = dojo.fromJson(response);
                            } catch (syntaxException) {
                                console.log("Couldn't eval the JSON: " + response + " (SyntaxError: " + syntaxException + ")");
                            }
                            
                            if (options.serverAsync && response.taskname) {
                                bespin.publish("message", 
                                    {msg: "Server is running : " + response.taskname,
                                    tag: "autohide"});
                            }
                        }
                        
                        if (dojo.isFunction(options['onSuccess'])) {
                            options['onSuccess'](response, xhr);
                        } else if (options['log']) {
                            console.log(options['log']);
                        }
                    } else {                        
                        var onStatus = 'on' + xhr.status;
                        if (options[onStatus]) {
                            options[onStatus](xhr);
                        } else if (options['onFailure']) {
                            options['onFailure'](xhr);
                        }
                    }
                }
            };
            xhr.open(method, this.SERVER_BASE_URL + url, true); // url must have leading /
            if (!server.token) {
                server.token = server._randomPassword();
                dojo.cookie("Domain-Token", server.token);
            }
            xhr.setRequestHeader("Domain-Token", server.token);
            xhr.setRequestHeader("Content-Type", 'application/x-www-form-urlencoded');
            if (options.headers) {
                for (var key in options.headers) {
                    if (options.headers.hasOwnProperty(key)) {
                        xhr.setRequestHeader(key, options.headers[key]);
                    }
                }
            }
            if (options.serverAsync) {
                server.asyncStarted();
            }
            xhr.send(payload);
        } else {
            var fullUrl = this.SERVER_BASE_URL + url;
            console.log("Are you sure you want to do a synchronous Ajax call? Really? " + fullUrl);
            xhr.open(method, fullUrl, false);
            xhr.send(payload);
            return xhr.responseText;
        }
    },
    
    _randomPassword: function() {
        chars = "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ1234567890";
        pass = "";
        for (var x = 0; x < 16; x++) {
            var charIndex = Math.floor(Math.random() * chars.length);
            pass += chars.charAt(charIndex);
        }
        return pass;
    },

    // ** {{{ asyncStarted() }}}
    //
    // Keeps track of jobs that are asynchronous on the server, so
    // that we know when we need to check for messages from the
    // server.
    asyncStarted: function() {
        console.log("Starting new server-side async job.");
        if (this._asyncCount == 0) {
            this.processMessages();
        }
        this._asyncCount++;
        console.log("Count is now " + this._asyncCount);
    },

    // ** {{{ asyncEnded() }}}
    //
    // Keeps track of the end of jobs that are asynchronous on the server, so
    // that we know when we can stop checking for messages from the
    // server.
    asyncEnded: function() {
        console.log("Server-side async job done.");
        if (this._asyncCount > 0) {
            this._asyncCount--;
        }
        console.log("Count is now " + this._asyncCount);
    },

    // == USER ==

    // ** {{{ login(user, pass, token, onSuccess, notloggedin) }}}
    //
    // Try to login to the backend system.
    // 
    // * {{{user}}} is the username
    // * {{{pass}}} is the password
    // * {{{onSuccess}}} fires when the user is logged in
    // * {{{onFailure}}} fires when the user failed to login
    login: function(user, pass, token, onSuccess, onFailure) {
        var url = "/register/login/" + user;
        this.request('POST', url, "password=" + escape(pass), { 
            onSuccess: onSuccess,
            on401: onFailure,
            log: 'Login complete.',
            headers: { 'DoubleSubmitCookie': token }
        });
    },

    // ** {{{ signup(user, pass, email, onSuccess, notloggedin, userconflict) }}}
    //
    // Signup / Register the user to the backend system
    // 
    // * {{{user}}} is the username
    // * {{{pass}}} is the password
    // * {{{email}}} is the email
    // * {{{onSuccess}}} fires when the user is logged in
    // * {{{notloggedin}}} fires when not logged in
    // * {{{userconflict}}} fires when the username exists
	signup: function(user, pass, email, onSuccess, notloggedin, userconflict) {
        var url = "/register/new/" + user;
        this.request('POST', url, 
			"password=" + escape(pass) + "&email=" + escape(email), { 
			onSuccess: onSuccess, on401: notloggedin, on409: userconflict,
			log: 'Login complete.' 
		});
	},

    // ** {{{ logout(onSuccess) }}}
    //
    // Logout from the backend
    // 
    // * {{{onSuccess}}} fires after the logout attempt
    logout: function(onSuccess) {
        var url = "/register/logout/";
        this.request('POST', url, null, { log: 'Logout complete.', onSuccess: onSuccess });
    },

    // ** {{{ currentuser(onSuccess, notloggedin) }}}
    //
    // Return info on the current logged in user
    // 
    // * {{{onSuccess}}} fires after the user attempt
    // * {{{notloggedin}}} fires if the user isn't logged in
    currentuser: function(whenLoggedIn, whenNotloggedin) {
        var url = "/register/userinfo/";
        return this.request('GET', url, null, 
                { onSuccess: whenLoggedIn, on401: whenNotloggedin, evalJSON: true });
    },

    // == FILES ==

    // ** {{{ list(project, path, onSuccess, onFailure) }}}
    //
    // List the path in the given project
    // 
    // * {{{project}}} is the project to list
    // * {{{path}}} is the path to list out
    // * {{{onSuccess}}} fires if the list returns something
    // * {{{onFailure}}} fires if there is an error getting a list from the server
    list: function(project, path, onSuccess, onFailure) {
        var project = project || '';
        var url = bespin.util.path.combine('/file/list/', project, path || '/');
        var opts = { onSuccess: onSuccess, evalJSON: true, log: "Listing files in: " + url };
        if (dojo.isFunction(onFailure)) opts.onFailure = onFailure;

        this.request('GET', url, null, opts);
    },

    // ** {{{ listAllFiles(project, onSuccess, onFailure) }}}
    //
    // List *all* files in the given project. Be *aware*: this will be a huge json-result!
    // 
    // * {{{project}}} is the project to list all files from
    // * {{{onSuccess}}} fires if the list returns something
    // * {{{onFailure}}} fires if there is an error getting a list from the server 
    listAllFiles: function(project, onSuccess, onFailure) {
        var project = project || '';
        var url = bespin.util.path.combine('/file/list_all/', project, '/');
        var opts = { onSuccess: onSuccess, evalJSON: true, log: "Listing all files in: " + url };
        if (dojo.isFunction(onFailure)) opts.onFailure = onFailure;
        
        this.request('GET', url, null, opts);
    },

    // ** {{{ projects(onSuccess) }}}
    //
    // Return the list of projects that you have access too
    // 
    // * {{{onSuccess}}} gets fired with the project list
    projects: function(onSuccess) {
        this.request('GET', '/file/list/', null, { onSuccess: onSuccess, evalJSON: true });
    },

    // ** {{{ saveFile(project, path, contents, lastOp) }}}
    //
    // Save the given file
    // 
    // * {{{project}}} is the project to save
    // * {{{path}}} is the path to save to
    // * {{{contents}}} fires after the save returns
    // * {{{lastOp}}} contains the last edit operation
    saveFile: function(project, path, contents, lastOp) {
        if (!project || !path) return;

        var url = bespin.util.path.combine('/file/at', project, (path || ''));
        if (lastOp) url += "?lastEdit=" + lastOp;

        this.request('PUT', url, contents, { log: 'Saved file "' + project + '/' + path+ '"' });
    },

    // ** {{{ loadFile(project, path, contents) }}}
    //
    // Load the given file
    // 
    // * {{{project}}} is the project to load from
    // * {{{path}}} is the path to load
    // * {{{onSuccess}}} fires after the file is loaded
    loadFile: function(project, path, onSuccess, onFailure) {
        var project = project || '';
        var path = path || '';
        var url = bespin.util.path.combine('/file/at', project, path);
        var opts = { onSuccess: onSuccess };
        if (dojo.isFunction(onFailure)) opts.onFailure = onFailure;

        this.request('GET', url, null, opts);
    },

    // ** {{{ removeFile(project, path, onSuccess, onFailure) }}}
    //
    // Remove the given file
    // 
    // * {{{project}}} is the project to remove from
    // * {{{path}}} is the path to remove
    // * {{{onSuccess}}} fires if the deletion works
    // * {{{onFailure}}} fires if the deletion failed
    removeFile: function(project, path, onSuccess, onFailure) {
        var project = project || '';
        var path = path || '';
        var url = bespin.util.path.combine('/file/at', project, path);
        var opts = { onSuccess: onSuccess };
        if (dojo.isFunction(onFailure)) opts.onFailure = onFailure;
        
        this.request('DELETE', url, null, opts);
    },

    // ** {{{ makeDirectory(project, path, onSuccess, onFailure) }}}
    //
    // Create a new directory
    // 
    // * {{{project}}} is the project to save
    // * {{{path}}} is the path to save to
    // * {{{onSuccess}}} fires if the deletion works
    // * {{{onFailure}}} fires if the deletion failed
    makeDirectory: function(project, path, onSuccess, onFailure) {
        if (!project) return;

        var url = bespin.util.path.combineAsDirectory('/file/at', project, (path || ''));
        var opts = {};
        if (dojo.isFunction(onSuccess)) {
            opts.onSuccess = onSuccess;
        } else {
            opts['log'] = "Made a directory: [project=" + project + ", path=" + path + "]";
        }
        if (dojo.isFunction(onFailure)) opts.onFailure = onFailure;

        this.request('PUT', url, null, opts);
    },
    
    // ** {{{ removeDirectory(project, path, onSuccess, onFailure) }}}
    //
    // Removed a directory
    // 
    // * {{{project}}} is the project to save
    // * {{{path}}} is the path to save to
    // * {{{onSuccess}}} fires if the deletion works
    // * {{{onFailure}}} fires if the deletion failed
    removeDirectory: function(project, path, onSuccess, onFailure) {
        if (!project) return;
        if (!path) path = '';
        
        var url = bespin.util.path.combineAsDirectory('/file/at', project, path);
        var opts = {};
        if (dojo.isFunction(onSuccess)) {
            opts.onSuccess = onSuccess;
        } else {
            opts['log'] = "Removed directory: [project=" + project + ", path=" + path + "]";
        }
        if (dojo.isFunction(onFailure)) opts.onFailure = onFailure;

        this.request('DELETE', url, null, opts);
    },

     // ** {{{ listOpen(onSuccess) }}}
     //
     // Returns JSON with the key of filename, and the value of an array of usernames:
     // { "foo.txt": ["ben"], "SomeAjaxApp/foo.txt": ["dion"] }
     // 
     // * {{{onSuccess}}} fires after listing the open files
    listOpen: function(onSuccess) {
        this.request('GET', '/file/listopen/', null, {
            onSuccess: onSuccess, evalJSON: true, log: 'List open files.' 
        });
    },

    // ** {{{ closeFile(project, path, onSuccess) }}}
    //
    // Close the given file (remove from open sessions)
    // 
    // * {{{project}}} is the project to close from
    // * {{{path}}} is the path to close
    // * {{{onSuccess}}} fires after the file is closed
    closeFile: function(project, path, onSuccess) {
        var path = path || '';
        var url = bespin.util.path.combine('/file/close', project, path);
        this.request('POST', url, null, { onSuccess: onSuccess });
    },
    
    // ** {{{ searchFiles(project, searchstring, onSuccess) }}}
    //
    // Search for files within the given project
    // 
    // * {{{project}}} is the project to look from
    // * {{{searchstring}}} to compare files with
    // * {{{onSuccess}}} fires after the file is closed
    searchFiles: function(project, searchkey, onSuccess) {
        var url = bespin.util.path.combine('/file/search', project+'?q='+escape(searchkey));
        var opts = { onSuccess: onSuccess, evalJSON: true, log: "Listing searchfiles for: " + project + ", searchkey: " + searchkey};
        this.request('GET', url, null, opts);
    },

    // == EDIT ==

    // ** {{{ editActions(project, path, onSuccess) }}}
    //
    // Get the list of edit actions
    // 
    // * {{{project}}} is the project to edit from
    // * {{{path}}} is the path to edit
    // * {{{onSuccess}}} fires after the edit is done
    editActions: function(project, path, onSuccess) {
        var path = path || '';
        var url = bespin.util.path.combine('/edit/list', project, path);
        this.request('GET', url, null, { onSuccess: onSuccess, log: "Edit Actions Complete." });
    },

    // ** {{{ editAfterActions(project, path, onSuccess) }}}
    //
    // Get the list of edit after actions
    // 
    // * {{{project}}} is the project to edit from
    // * {{{path}}} is the path to edit
    // * {{{onSuccess}}} fires after the edit is done
    editAfterActions: function(project, path, index, onSuccess) {
        var path = path || '';
        var url = bespin.util.path.combine('/edit/recent', index, project, path);
        this.request('GET', url, null, { onSuccess: onSuccess, log: "Edit After Actions Complete." });
    },

    // ** {{{ doAction(project, path, actions) }}}
    //
    // Store actions to the edit queue
    // 
    // * {{{project}}} is the project
    // * {{{path}}} is the path
    // * {{{actions}}} contain the actions to store
    doAction: function(project, path, actions) {
        var path = path || '';
        var url = bespin.util.path.combine('/edit', project, path);

        var sp = "[" + actions.join(",") + "]";

        this.request('PUT', url, sp, { onSuccess: function(){} });
    },

    // == PROJECTS ==
    //
    // still needed: owners, authorize, deauthorize
    
    // ** {{{ exportProject(project, archivetype) }}}
    //
    // Export the project as either a zip file or tar + gz
    // 
    // * {{{project}}} is the project to export
    // * {{{archivetype}}} is either zip | tgz
    exportProject: function(project, archivetype) {
        if (bespin.util.include(['zip','tgz','tar.gz'], archivetype)) {
            var iframe = document.createElement("iframe");
            iframe.src = bespin.util.path.combine('/project/export', project + "." + archivetype);
            iframe.style.display = 'none';
            iframe.style.height = iframe.style.width = "0";
            document.getElementsByTagName("body")[0].appendChild(iframe);
        }
    },

    // ** {{{ importProject(project, url, opts) }}}
    //
    // Import the given file into the given project
    // 
    // * {{{project}}} is the project to export
    // * {{{url}}} is the URL to the file to import
    // * {{{archivetype}}} is either zip | tgz
    importProject: function(project, url, opts) {
        if (opts) { // wrap the import success call in an event to say that the import is complete
            var userCall = opts.onSuccess;
            opts.onSuccess = function(text, xhr) {
                userCall(text, xhr);
                bespin.publish("project:imported", {
                    project: project,
                    url: url
                });
            };
        }
        
        this.request('POST', '/project/fromurl/' + project, url, opts || {});
    },
    
    // ** {{{ renameProject(currentProject, newProject) }}}
    //
    // Import the given file into the given project
    // 
    // * {{{currentProject}}} is the current name of the project
    // * {{{newProject}}} is the new name
    renameProject: function(currentProject, newProject, opts) {
        if (!opts) opts = { log: "Renaming project from " + currentProject + " to " + newProject };
        if (currentProject && newProject) {
            this.request('POST', '/project/rename/' + currentProject + "/", newProject, opts);
        }
    },

    // == SETTINGS ==
    //
    //
    // * GET /settings/ to list all settings for currently logged in user as json dict
    // * GET /settings/[setting] to get the value for a single setting as json string
    // * POST /settings/ with HTTP POST DATA (in standard form post syntax) to set the value for a collection of settings (all values are strings)
    // * DELETE /settings/[setting] to delete a single setting

    listSettings: function(onSuccess) {
        if (typeof onSuccess == "function") {
            this.request('GET', '/settings/', null, { onSuccess: onSuccess, evalJSON: true });
        }
    },

    getSetting: function(name, onSuccess) {
        if (typeof onSuccess == "function") {
            this.request('GET', '/settings/' + name, null, { onSuccess: onSuccess });
        }
    },
    
    setSetting: function(name, value, onSuccess) {
        var settings = {};
        settings[name] = value;
        this.setSettings(settings, (onSuccess || function(){}));
    },
    
    setSettings: function(settings, onSuccess) {
        this.request('POST', '/settings/', dojo.objectToQuery(settings), { onSuccess: (onSuccess || function(){}) });
    },
    
    unsetSetting: function(name, onSuccess) {
        this.request('DELETE', '/settings/' + name, null, { onSuccess: (onSuccess || function(){}) });
    },
    
    // ** {{{ vcs() }}}
    // Run a Version Control System (VCS) command
    // The command object should have a command attribute
    // on it that is a list of the arguments.
    // Commands that require authentication should also
    // have kcpass, which is a string containing the user's
    // keychain password.
    vcs: function(project, command, opts) {
        opts = opts || {};
        opts.serverAsync = true;
        this.request('POST', '/vcs/command/' + project + '/',
                     dojo.toJson(command),
                     opts);
    },
    
    // ** {{{ clone() }}}
    // Clone a remote repository
    clone: function(data, opts) {
        opts = opts || {};
        opts.serverAsync = true;
        this.request('POST', '/vcs/clone/',
                    data, opts);
    },
    
    // ** {{{ setauth() }}}
    // Sets authentication for a project
    setauth: function(project, form, opts) {
        this.request('POST', '/vcs/setauth/' + project + '/',
                    dojo.formToQuery(form), opts || {});
    },
    
    // ** {{{ getkey() }}}
    // Retrieves the user's SSH public key that can be used for VCS functions
    getkey: function(kcpass, opts) {
        if (kcpass == null) {
            this.request('POST', '/vcs/getkey/', null, opts || {});
        } else {
            this.request('POST', '/vcs/getkey/', "kcpass=" + escape(kcpass), opts || {});
        }
    },
    
    // ** {{{ remoteauth() }}}
    // Finds out if the given project requires remote authentication
    // the values returned are "", "both" (for read and write), "write"
    // when only writes require authentication
    // the result is published as an object with project, remoteauth
    // values to vcs:remoteauthUpdate and sent to the callback.
    remoteauth: function(project, callback) {
        this.request('GET', '/vcs/remoteauth/' + escape(project) + '/',
            null,
            {
                onSuccess: function(result) {
                    var event = {
                        project: project,
                        remoteauth: result
                    };
                    bespin.publish("vcs:remoteauthUpdate", event);
                    callback(result);
                }
            }
        );
    },
    
    // ** {{{ processMessages() }}}
    // Starts up message retrieve for this user. Call this only once.
    processMessages: function() {
        console.log("Message processing starting");
        var server = this;
        function doProcessMessages() {
            server.request('POST', '/messages/', null,
                {
                    evalJSON: true,
                    onSuccess: function(messages) {
                        for (var i=0; i < messages.length; i++) {
                            var message = messages[i];
                            var eventName = message.eventName;
                            if (eventName) {
                                bespin.publish(eventName, message);
                            }
                            if (message.asyncDone) {
                                server.asyncEnded();
                            }
                        }
                        if (server._asyncCount > 0) {
                            setTimeout(doProcessMessages, 1000);
                        }
                    },
                    onFailure: function(message) {
                        if (message.asyncDone) {
                            server.asyncEnded();
                        }
                        if (server._asyncCount > 0) {
                            setTimeout(doProcessMessages, 1000);
                        }
                    }
                });
        }
        doProcessMessages();
    }
});
