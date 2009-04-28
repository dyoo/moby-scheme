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

dojo.provide("bespin.jetpack");

dojo.require("bespin.util.webpieces");
dojo.require("bespin.cmd.commands");
dojo.require("bespin.cmd.commandline");

// Command store for the Jetpack commands
// (which are subcommands of the main 'jetpack' command)
bespin.jetpack.commands = new bespin.cmd.commandline.CommandStore({ subCommand: {
    name: 'jetpack',
    preview: 'play with jetpack features',
    completeText: 'subcommands: create [featurename], install [featurename], list',
    subcommanddefault: 'help'
}});

// = Commands =
// Jetpack related commands

// ** {{{Command: help}}} **
bespin.jetpack.commands.addCommand({
    name: 'help',
    takes: ['search'],
    preview: 'show commands for jetpack subcommand',
    description: 'The <u>help</u> gives you access to the various commands in the Bespin system.<br/><br/>You can narrow the search of a command by adding an optional search params.<br/><br/>Finally, pass in the full name of a command and you can get the full description, which you just did to see this!',
    completeText: 'optionally, narrow down the search',
    execute: function(self, extra) {
        bespin.cmd.displayHelp(bespin.jetpack.commands, self, extra);
    }
});

// ** {{{Command: create}}} **
bespin.jetpack.commands.addCommand({
    name: 'create',
    takes: ['name'],
    preview: 'create a new jetpack feature',
    description: 'Create a new jetpack feature that you can install into Firefox with the new Jetpack goodness.',
    completeText: 'name of the feature',
    template: "<feature id='TEMPLATE_NAME' name='TEMPLATE_NAME Sidebar Extension' version='0.1' description='TEMPLATE_NAME Sidebar Test Extension. Opens a sidebar with some HTML code.'>\n  <script>\n    let sidebar;\n\n    function install() {\n      sidebar =\n        Jetpack.UI.Sidebars.create(\n          { id: 'test', name: 'TEMPLATE_NAME Test Sidebar',\n            content: $('#sidebar-content')[0] });\n      window.setInterval(function() { updateTime(); }, 1000);\n    }\n\n    function updateTime() {\n      $('#time')[0].textContent = (new Date()).toString();\n      Jetpack.UI.Sidebars.update({ sidebar : sidebar });\n    }\n\n    function uninstall() {\n    }\n  </script>\n  <div id='sidebar-content'>\n    <h1>Success!</h1>\n    <p>Your sidebar extension was installed correctly!</p>\n    <p>The current <strong>date and time</strong> is:</p>\n    <ul>\n    <li id='time'></li>\n    </ul>\n    <p>And the code:</p>\n    <code><pre>\n    let sidebar;\n\n    function install() {\n      sidebar =\n        Jetpack.UI.Sidebars.create(\n          { id: 'test', name: 'Test Sidebar',\n            content: $('#sidebar-content')[0] });\n      window.setInterval(function() { updateTime(); }, 1000);\n    }\n\n    function updateTime() {\n      $('#time')[0].textContent = (new Date()).toString();\n      Jetpack.UI.Sidebars.update({sidebar : sidebar});\n    }\n    </pre></code>\n  </div>\n</feature>",
    execute: function(self, name) {
        name = name || 'newjetpack';
        // create a new file in BespinSettings/jetpack/{name}.html
        bespin.publish("editor:newfile", {
            project: bespin.userSettingsProject,
            newfilename: 'jetpack/' + name + '.html',
            content: this.template.replace(/TEMPLATE_NAME/g, name)
        });
    }
});

// ** {{{Command: install}}} **
bespin.jetpack.commands.addCommand({
    name: 'install',
    takes: ['name'],
    preview: 'install a jetpack feature',
    description: 'Install a Jetpack feature, either the current file, or the named feature',
    completeText: 'optionally, the name of the feature to install',
    execute: function(self, name) {
        // Use the given name, or default to the current jetpack
        name = name || (function() {
            var editSession = bespin.get('editSession');
            if (editSession.project != bespin.userSettingsProject || editSession.path.indexOf('jetpack') < 0) return; // jump out if not in the jetpack project
            var bits = editSession.path.split('.');
            return bits[bits.length - 2];
        })();

        if (!name) {
            bespin.publish("message", { msg: "Please pass in the name of the Jetpack feature you would like to install" });
        } else {
            // add the link tag to the body
            // <link rel="jetpack" href="1.0/main.html" name="testExtension">
            var link = dojo.create("link", {
                rel: 'jetpack',
                href: bespin.util.path.combine("preview/at", bespin.userSettingsProject, name + ".html"),
                name: name
            }, dojo.body());
        }
    }
});

// ** {{{Command: list}}} **
bespin.jetpack.commands.addCommand({
    name: 'list',
    preview: 'list out the Jetpacks that you have written',
    description: 'Which Jetpacks have you written and have available in BespinSettings/jetpacks. NOTE: This is not the same as which Jetpacks you have installed in Firefox!',
    execute: function(self, extra) {
        bespin.get('server').list(bespin.userSettingsProject, 'jetpack/', function(jetpacks) {
            var output;

            if (!jetpacks || jetpacks.length < 1) {
                output = "You haven't installed any Jetpacks. Run '> jetpack create' to get going.";
            } else {
                output = "<u>Your Jetpacks</u><br/><br/>";

                output += dojo.map(dojo.filter(jetpacks, function(file) {
                    return bespin.util.endsWith(file.name, '\\.html');
                }), function(c) {
                    return "<a href=\"javascript:bespin.get('commandLine').executeCommand('open jetpack/" + c.name + " BespinSettings');\">" + c.name.replace(/\.html$/, '') + "</a>";
                }).join("<br>");
            }

            bespin.publish("message", { msg: output });
        });
    }
});
