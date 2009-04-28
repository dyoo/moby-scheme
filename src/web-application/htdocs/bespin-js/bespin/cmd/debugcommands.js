dojo.provide("bespin.cmd.debugcommands");

(function() {
    var commandStore = bespin.get("commandLine").commandStore;

    // ** {{{Command: action}}} **
    commandStore.addCommand({
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

    // ** {{{Command: echo}}} **
    commandStore.addCommand({
        name: 'echo',
        takes: ['message ...'],
        preview: 'A test echo command',
        // ** {{{execute}}}
        execute: function(self, args) {
            self.showInfo(args);
        }
    });

    // ** {{{Command: login}}} **
    commandStore.addCommand({
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

    // ** {{{Command: insert}}} **
    commandStore.addCommand({
        name: 'insert',
        takes: ['text'],
        preview: 'insert the given text at this point.',
        hidden: true,
        execute: function(self, text) {
            self.editor.model.insertChunk(self.editor.getModelPos(), text);
        }
    });

    // ** {{{Command: typingtest}}} **
    commandStore.addCommand({
        name: 'typingtest',
        preview: 'type in the alphabet a few times',
        hidden: true,
        execute: function(self) {
            var start = Date.now();

            for (var i = 0; i < 3; i++) {
                dojo.forEach(['a', 'b', 'c', 'd', 'e', 'f', 'g', 'h', 'i', 'j', 'k', 'l', 'm', 'n', 'o', 'p', 'q', 'r', 's', 't', 'u', 'v', 'w', 'x', 'y', 'z'], function(c) {
                    var args = { pos: bespin.editor.utils.copyPos(self.editor.getCursorPos()) };
                    args.newchar = c;
                    self.editor.ui.actions.insertCharacter(args);            
                });
            }

            var stop = Date.now();

            self.showInfo("It took " + (stop - start) + " milliseconds to do this");        
        }
    });

    // ** {{{Command: test}}} **
    commandStore.addCommand({
        name: 'test',
        preview: 'Run some automated end to end tests',
        script: [
            { send:"echo Starting", expect:/^Starting$/ },
            { send:"follow", expect:/sds/ },
            { send:"echo Finished", expect:/^Finished$/ }
        ],
        // ** {{{_setup}}}
        _setup: function(self, onSuccess) {
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

})();