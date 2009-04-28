
dojo.provide("bespin.social");

dojo.require("bespin.cmd.dashboardcommands");

// ** {{{ Utility: displayFollowers }}} **
// Take an string array of follower names, and publish a "Following: ..."
// message as a command line response.
bespin.cmd.displayFollowers = function(followers) {
    var message = "Following: " + followers.join(", ");
    bespin.publish("message", { msg:message });
}

// ====================================================================== FOLLOW

// ** {{{ Command: follow }}} **
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

if (bespin.cmd.dashboardcommands) {
    bespin.cmd.dashboardcommands.Commands.push('follow');
}
//if (bespin.cmd.editorcommands) {
//    bespin.cmd.editorcommands.Commands.push('follow');
//}

// ** {{{ Event: network:followers }}} **
// Get a list of our followers
bespin.subscribe("network:followers", function() {
    bespin.get('server').followers({
        onSuccess: function(data) {
            bespin.cmd.displayFollowers(dojo.fromJson(data));
        },
        onFailure: function(xhr) {
            bespin.publish("message", { msg: "Failed to retrieve followers: " + xhr.responseText });
        }
    });
});

// ** {{{ Event: network:follow }}} **
// Add to the list of users that we follow
bespin.subscribe("network:follow", function(usernames) {
    bespin.get('server').follow(usernames, {
        onSuccess: function(data) {
            bespin.cmd.displayFollowers(dojo.fromJson(data));
        },
        onFailure: function(xhr) {
            bespin.publish("message", { msg: "Failed to add follower: " + xhr.responseText });
        }
    });
});

dojo.extend(bespin.client.Server, {
    // ** {{{ follows(opts) }}}
    // Get a list of the users the current user is following
    follow: function(users, opts) {
        this.request('POST', '/network/follow/', dojo.toJson(users), opts || {});
    },

    // ** {{{ follows(opts) }}}
    // Get a list of the users the current user is following
    followers: function(opts) {
        this.request('GET', '/network/followers/', null, opts || {});
    }
});

// ==================================================================== UNFOLLOW

// ** {{{Command: unfollow}}} **
bespin.cmd.commands.add({
    name: 'unfollow',
    takes: ['username ...'],
    preview: 'remove from the list of users we are following',
    completeText: 'username(s) of person(s) to stop following',
    usage: "[username] ...<br><br><em>The username(s) to stop following</em>",
    // ** {{{execute}}}
    execute: function(self, args) {
        var usernames = bespin.cmd.commands.toArgArray(args);
        if (usernames.length == 0) {
            self.showInfo('Please specify the users to cease following');
        }
        else {
            bespin.publish("network:unfollow", [ usernames ]);
        }
    }
});

if (bespin.cmd.dashboardcommands) {
    bespin.cmd.dashboardcommands.Commands.push('unfollow');
}
//if (bespin.cmd.editorcommands) {
//    bespin.cmd.editorcommands.Commands.push('unfollow');
//}

// ** {{{ Event: network:unfollow }}} **
// Remove users from the list that we follow
bespin.subscribe("network:unfollow", function(usernames) {
    bespin.get('server').unfollow(usernames, {
        onSuccess: function(data) {
            bespin.cmd.displayFollowers(dojo.fromJson(data));
        },
        onFailure: function(xhr) {
            bespin.publish("message", { msg: "Failed to remove follower: " + xhr.responseText });
        }
    });
});

dojo.extend(bespin.client.Server, {
    // ** {{{ follows(opts) }}}
    // Get a list of the users the current user is following
    unfollow: function(users, opts) {
        this.request('POST', '/network/unfollow/', dojo.toJson(users), opts || {});
    }
});

// ======================================================================= GROUP

// ** {{{Command: group}}} **
bespin.cmd.commands.add({
    name: 'group',
    takes: ['[{name}|--add|--remove] ...'],
    preview: 'Collect the people you follow into groups, and display the existing groups',
    // ** {{{execute}}}
    execute: function(self, args) {
        args = bespin.cmd.commands.toArgArray(args);

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

if (bespin.cmd.dashboardcommands) {
    bespin.cmd.dashboardcommands.Commands.push('group');
}
//if (bespin.cmd.editorcommands) {
//    bespin.cmd.editorcommands.Commands.push('group');
//}

// ** {{{ Event: groups:list:all }}} **
// Get a list of our groups
bespin.subscribe("groups:list:all", function() {
    bespin.get('server').groupListAll({
        onSuccess: function(data) {
            var groups = dojo.fromJson(data);
            if (groups.length == 0) {
                bespin.publish("message", { msg:"You have no groups" });
            }
            else {
                var message = "You have the following groups: " + bespin.cmd.formatStringArray(groups);
                bespin.publish("message", { msg:message });
            }
        },
        onFailure: function(xhr) {
            bespin.publish("message", { msg: "Failed to retrieve groups: " + xhr.responseText });
        }
    });
});

// ** {{{ Event: groups:list }}} **
// Get a list of group members
bespin.subscribe("groups:list", function(group) {
    bespin.get('server').groupList(group, {
        onSuccess: function(data) {
            var members = dojo.fromJson(data);
            if (members.length == 0) {
                console.warn("Group " + group + " has no members - it should have been auto-deleted!")
                bespin.publish("message", { msg: "" + group + " has no members." });
            }
            else {
                var message = "Members of " + group + ": " + bespin.cmd.formatStringArray(members);
                bespin.publish("message", { msg:message });
            }
        },
        onFailure: function(xhr) {
            bespin.publish("message", { msg: "Failed to retrieve group members: " + xhr.responseText });
        }
    });
});

// ** {{{ Event: groups:remove:all }}} **
// Remove a group and all its members
bespin.subscribe("groups:remove:all", function(group) {
    bespin.get('server').groupRemoveAll(group, {
        onSuccess: function(data) {
            bespin.publish("message", { msg: "Removed group " + group });
        },
        onFailure: function(xhr) {
            bespin.publish("message", { msg: "Failed to retrieve group members. Maybe due to: " + xhr.responseText });
        }
    });
});

// ** {{{ Event: groups:add }}} **
// Add to members of a group
bespin.subscribe("groups:add", function(group, users) {
    bespin.get('server').groupAdd(group, users, {
        onSuccess: function(data) {
            bespin.publish("message", { msg: "Members of " + group + ": " + data });
        },
        onFailure: function(xhr) {
            bespin.publish("message", { msg: "Failed to add to group members. Maybe due to: " + xhr.responseText });
        }
    });
});

// ** {{{ Event: groups:remove }}} **
// Add to members of a group
bespin.subscribe("groups:remove", function(group, users) {
    bespin.get('server').groupRemove(group, users, {
        onSuccess: function(data) {
            bespin.publish("message", { msg: "Members of " + group + ": " + data });
        },
        onFailure: function(xhr) {
            bespin.publish("message", { msg: "Failed to remove to group members. Maybe due to: " + xhr.responseText });
        }
    });
});

dojo.extend(bespin.client.Server, {
    // ** {{{ groupListAll() }}}
    // Get a list of the users the current user is following
    groupListAll: function(opts) {
        this.request('GET', '/group/list/all/', null, opts || {});
    },

    // ** {{{ groupList() }}}
    // Get a list of the users the current user is following
    groupList: function(group, opts) {
        this.request('GET', '/group/list/' + group + '/', null, opts || {});
    },

    // ** {{{ groupRemove() }}}
    // Get a list of the users the current user is following
    groupRemove: function(group, users, opts) {
        this.request('POST', '/group/remove/' + group + '/', dojo.toJson(users), opts || {});
    },

    // ** {{{ groupRemoveAll() }}}
    // Get a list of the users the current user is following
    groupRemoveAll: function(group, opts) {
        this.request('POST', '/group/remove/all/' + group + '/', null, opts || {});
    },

    // ** {{{ groupAdd() }}}
    // Get a list of the users the current user is following
    groupAdd: function(group, users, opts) {
        this.request('POST', '/group/add/' + group + '/', dojo.toJson(users), opts || {});
    }
});

// ======================================================================= SHARE

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

if (bespin.cmd.dashboardcommands) {
    bespin.cmd.dashboardcommands.Commands.push('share');
}
//if (bespin.cmd.editorcommands) {
//    bespin.cmd.editorcommands.Commands.push('share');
//}

// ** {{{ Event: share:list:all }}} **
// List all project shares
bespin.subscribe("share:list:all", function() {
    bespin.get('server').shareListAll({
        onSuccess: function(data) {
            bespin.publish("message", { msg: "Project sharing: " + data });

            var shares = dojo.fromJson(data);
            if (shares.length == 0) {
                bespin.publish("message", { msg:"You are not sharing any projects" });
            }
            else {
                var message = "You have the following shares:<ul>\n";
                dojo.forEach(shares, function(share) {
                    // loadany needs adding here
                    var edit = share.edit ? "<strong>Editable</strong>" : "Read-only";
                    if (share.type == "everyone") {
                        message += "<li><strong>" + share.project + "</strong> with <strong>everyone</strong>: " + edit + ".</li>\n";
                    }
                    else if (share.type == "group") {
                        message += "<li><strong>" + share.project + "</strong> with the group <strong>" + share.recipient + "</strong>: " + edit + ".</li>\n";
                    }
                    else {
                        message += "<li><strong>" + share.project + "</strong> with <strong>" + share.recipient + "</strong>: " + edit + ".</li>\n";
                    }
                });
                message += "</ul>";
                bespin.publish("message", { msg:message });
            }
        },
        onFailure: function(xhr) {
            bespin.publish("message", { msg: "Failed to list project shares: " + xhr.responseText });
        }
    });
});

// ** {{{ Event: share:list:project }}} **
// List sharing for a given project
bespin.subscribe("share:list:project", function(project) {
    bespin.get('server').shareListProject(project, {
        onSuccess: function(data) {
            bespin.publish("message", { msg: "Project sharing for " + project + ": " + data });
        },
        onFailure: function(xhr) {
            bespin.publish("message", { msg: "Failed to list project sharing. Maybe due to: " + xhr.responseText });
        }
    });
});

// ** {{{ Event: share:list:project:member }}} **
// List sharing for a given project and member
bespin.subscribe("share:list:project:member", function(project, member) {
    bespin.get('server').shareListProjectMember(project, member, {
        onSuccess: function(data) {
            bespin.publish("message", { msg: "Project sharing for " + project + ", " + member + ": " + data });
        },
        onFailure: function(xhr) {
            bespin.publish("message", { msg: "Failed to list project sharing. Maybe due to: " + xhr.responseText });
        }
    });
});

// ** {{{ Event: share:remove:all }}} **
// Remove all sharing from a project
bespin.subscribe("share:remove:all", function(project) {
    bespin.get('server').shareRemoveAll(project, {
        onSuccess: function(data) {
            bespin.publish("message", { msg: "All sharing removed from " + project });
        },
        onFailure: function(xhr) {
            bespin.publish("message", { msg: "Failed to remove sharing permissions. Maybe due to: " + xhr.responseText });
        }
    });
});

// ** {{{ Event: share:remove }}} **
// Remove project sharing from a given member
bespin.subscribe("share:remove", function(project, member) {
    bespin.get('server').shareRemove(project, member, {
        onSuccess: function(data) {
            bespin.publish("message", { msg: "Removed sharing permission from " + member + " to " + project });
        },
        onFailure: function(xhr) {
            bespin.publish("message", { msg: "Failed to remove sharing permission. Maybe due to: " + xhr.responseText });
        }
    });
});

// ** {{{ Event: share:add }}} **
// Add a member to the sharing list for a project
bespin.subscribe("share:add", function(project, member, options) {
    bespin.get('server').shareAdd(project, member, options, {
        onSuccess: function(data) {
            bespin.publish("message", { msg: "Adding sharing permission for " + member + " to " + project });
        },
        onFailure: function(xhr) {
            bespin.publish("message", { msg: "Failed to add sharing permission. Maybe due to: " + xhr.responseText });
        }
    });
});

dojo.extend(bespin.client.Server, {
    // ** {{{ shareListAll() }}}
    // List all project shares
    shareListAll: function(opts) {
        this.request('GET', '/share/list/all/', null, opts || {});
    },

    // ** {{{ shareListProject() }}}
    // List sharing for a given project
    shareListProject: function(project, opts) {
        this.request('GET', '/share/list/' + project + '/', null, opts || {});
    },

    // ** {{{ shareListProjectMember() }}}
    // List sharing for a given project and member
    shareListProjectMember: function(project, member, opts) {
        this.request('GET', '/share/list/' + project + '/' + member + '/', null, opts || {});
    },

    // ** {{{ shareRemoveAll() }}}
    // Remove all sharing from a project
    shareRemoveAll: function(project, opts) {
        this.request('POST', '/share/remove/' + project + '/all/', null, opts || {});
    },

    // ** {{{ shareRemove() }}}
    // Remove project sharing from a given member
    shareRemove: function(project, member, opts) {
        this.request('POST', '/share/remove/' + project + '/' + member + '/', null, opts || {});
    },

    // ** {{{ shareAdd() }}}
    // Add a member to the sharing list for a project
    shareAdd: function(project, member, options, opts) {
        this.request('POST', '/share/add/' + project + '/' + member + '/', dojo.toJson(options), opts || {});
    }
});

// ====================================================================== VIEWME

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

if (bespin.cmd.dashboardcommands) {
    bespin.cmd.dashboardcommands.Commands.push('viewme');
}
//if (bespin.cmd.editorcommands) {
//    bespin.cmd.editorcommands.Commands.push('viewme');
//}

// ** {{{ Event: viewme:list:all }}} **
// List all the members with view settings on me
bespin.subscribe("viewme:list:all", function() {
    bespin.get('server').viewmeListAll({
        onSuccess: function(data) {
            bespin.publish("message", { msg: "All view settings: " + data });
        },
        onFailure: function(xhr) {
            bespin.publish("message", { msg: "Failed to retrieve view settings. Maybe due to: " + xhr.responseText });
        }
    });
});

// ** {{{ Event: viewme:list }}} **
// List the view settings for a given member
bespin.subscribe("viewme:list", function(member) {
    bespin.get('server').viewmeList(member, {
        onSuccess: function(data) {
            bespin.publish("message", { msg: "View settings for " + member + ": " + data });
        },
        onFailure: function(xhr) {
            bespin.publish("message", { msg: "Failed to retrieve view settings. Maybe due to: " + xhr.responseText });
        }
    });
});

// ** {{{ Event: viewme:set }}} **
// Alter the view setting for a given member
bespin.subscribe("viewme:set", function(member, value) {
    bespin.get('server').viewmeSet(member, value, {
        onSuccess: function(data) {
            bespin.publish("message", { msg: "Changed view settings for " + member });
        },
        onFailure: function(xhr) {
            bespin.publish("message", { msg: "Failed to change view setttings. Maybe due to: " + xhr.responseText });
        }
    });
});

dojo.extend(bespin.client.Server, {
    // ** {{{ viewmeListAll() }}}
    // List all the members with view settings on me
    viewmeListAll: function(opts) {
        this.request('GET', '/viewme/list/all/', null, opts || {});
    },

    // ** {{{ viewmeList() }}}
    // List the view settings for a given member
    viewmeList: function(member, opts) {
        this.request('GET', '/viewme/list/' + member + '/', null, opts || {});
    },

    // ** {{{ viewmeSet() }}}
    // Alter the view setting for a given member
    viewmeSet: function(member, value, opts) {
        this.request('POST', '/viewme/set/' + member + '/' + value + '/', null, opts || {});
    }
});
