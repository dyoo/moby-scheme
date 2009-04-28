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

dojo.provide("bespin.page.dashboard.components");

if (!bespin.page.dashboard) bespin.page.dashboard = {};

th.BespinSessionPanel = Class.define({
    type: "BespinSessionPanel",

    superclass: th.Panel,

    members: {
        init: function(parms) {
            this._super(parms);

            this.filename = new th.Label({ className: "filename" });
            this.path = new th.Label({ className: "path" });
            this.opened = new th.Label({ className: "opened" });
            this.details = new th.Label({ className: "details" });
            this.editTime = new th.Label({ className: "editTime" });

            var labels = [ this.filename, this.path, this.opened, this.details, this.editTime ];

            this.add(labels);

            var panel = this;
            for (var i = 0; i < labels.length; i++) {
                this.bus.bind("dblclick", labels[i], function(e) {
                    panel.bus.fire("dblclick", e, panel);
                });
            }

            this.preferredSizes = [ 13, 9, 8, 8, 8 ];
            this.minimumSizes = [ 9, 8, 7, 7, 7 ];

            this.filename.text = parms.filename;
            this.path.text = parms.project + ": /" + parms.path;

            // dummy data
            this.opened.text = "(opened info)";
            this.details.text = "(edit details info)";
            this.editTime.text = "(editing time)";

            this.session = { filename: parms.filename, path: parms.path, project: parms.project };
        },

        layout: function() {
            var d = this.d();
            var w = d.b.w - d.i.w;
            var labels = 5;
            var sizes = this.preferredSizes.slice();
            var y;

            while (labels > 0) {
                y = d.i.t;

                // set the fonts and clear the bounds
                for (var i = 0; i < this.children.length; i++) {
                    var font = sizes[i] + "pt Tahoma";
                    this.children[i].addCss("font", font);

                    delete this.children[i].bounds;
                }

                var current = 0;

                var h = this.filename.getPreferredSize().height;
                h = Math.floor(h * 0.95); // pull in the line height a bit
                this.filename.bounds = { x: d.i.l, y: y, width: w, height: h };
                y += h;

                if (++current < labels) {
                    h = this.path.getPreferredSize().height;
                    h = Math.floor(h * 1.2); // add a bit of margin to separate from subsequent labels
                    this.path.bounds = { x: d.i.l, y: y, width: w, height: h };
                    y += h;
                }

                if (++current < labels) {
                    h = this.opened.getPreferredSize().height;
                    this.opened.bounds = { x: d.i.l, y: y, width: w, height: h };
                    y += h;
                }

                if (++current < labels) {
                    h = this.details.getPreferredSize().height;
                    this.details.bounds = { x: d.i.l, y: y, width: w, height: h };
                    y += h;
                }

                if (++current < labels) {
                    h = this.editTime.getPreferredSize().height;
                    this.editTime.bounds = { x: d.i.l, y: y, width: w, height: h };
                    y += h;
                }

                y += d.i.b;
                if (y <= d.b.h) break;

                // we're too tall, make adjustments

                var changeMade = false;
                for (var z = 2; z < sizes.length; z++) {
                    if (sizes[z] > this.minimumSizes[z]) {
                        sizes[z]--;
                        changeMade = true;
                    }
                }
                if (changeMade) continue;

                if (labels > 2) {
                    labels--;
                    continue;
                }

                changeMade = false;
                for (y = 0; y < 2; y++) {
                    if (sizes[y] > this.minimumSizes[y]) {
                        sizes[y]--;
                        changeMade = true;
                    }
                }
                if (changeMade) continue;

                labels--;
            }
        }
    }
});

th.BespinProjectPanelFooter = Class.define({
    type: "BespinProjectPanelFooter",

    superclass: th.Panel,

    members: {
        init: function(parms) {
            this._super(parms);
            this.add = new th.Button();
        },

        getPreferredHeight: function(width) {
            return 17;
        },

        paintSelf: function(ctx) {
            // not ready to display this yet - bg

    //        var d = this.d();
    //
    //        ctx.fillStyle = "rgb(85, 80, 72)";
    //        ctx.fillRect(0, 0, d.b.w, 1);
    //
    //        ctx.fillStyle = "rgb(35, 31, 28)";
    //        ctx.fillRect(0, d.b.h - 1, d.b.w, 1);
    //
    //        var gradient = ctx.createLinearGradient(0, 1, 1, d.b.h - 2);
    //        gradient.addColorStop(0, "rgb(71, 66, 57)");
    //        gradient.addColorStop(1, "rgb(65, 61, 53)");
    //        ctx.fillStyle = gradient;
    //        ctx.fillRect(0, 1, d.b.w, d.b.h - 2);
        }
    }
});

th.BespinProjectPanel = Class.define({
    type: "BespinProjectPanel",

    superclass: th.Panel,

    member: {
        init: function(parms) {
            if (!parms) parms = {};
            this._super(parms);

            this.projectLabel = new th.Label({ text: "Projects", className: "projectLabel" });

            this.list = new th.List();

            this.splitter = new th.Splitter({ orientation: th.HORIZONTAL });

            this.footer = new th.BespinProjectPanelFooter();

            this.add([ this.projectLabel, this.list, this.splitter, this.footer ]);

            this.bus.bind("dragstart", this.splitter, this.ondragstart, this);
            this.bus.bind("drag", this.splitter, this.ondrag, this);
            this.bus.bind("dragstop", this.splitter, this.ondragstop, this);

            // this is a closed container
            delete this.add;
            delete this.remove;
        },

        ondragstart: function(e) {
            this.startWidth = this.bounds.width;
        },

        ondrag: function(e) {
            var delta = e.currentPos.x - e.startPos.x;
            this.prefWidth = this.startWidth + delta;
            this.getScene().render();
        },

        ondragstop: function(e) {
            delete this.startWidth;
        },

        getPreferredSize: function() {
            return { width: this.prefWidth || 150, height: 0 };
        },

        layout: function() {
            var d = this.d();

            var y = d.i.t;

            // todo: when I have a better way to do uni-dimensional preferred sizing, restore this
            //var lh = this.projectLabel.getPreferredHeight(d.b.w);
            var lh = this.projectLabel.getPreferredSize().height;

            this.projectLabel.bounds = { y: y, x: d.i.l, height: lh, width: d.b.w };
            y += lh;

            var sw = this.splitter.getPreferredSize().width;
            this.splitter.bounds = { x: d.b.w - d.i.r - sw, height: d.b.h - d.i.b - y, y: y, width: sw };

            var innerWidth = d.b.w - d.i.w - sw;

            // todo: when I have a better way to do uni-dimensional preferred sizing, restore this
//            var ph = this.footer.getPreferredHeight(innerWidth);
            var ph = this.footer.getPreferredSize().height;

            this.footer.bounds = { x: d.i.l, y: d.b.h - ph, width: innerWidth, height: ph };

            this.list.bounds = { x: d.i.l, y: y, width: innerWidth, height: this.splitter.bounds.height };
        }
    }
});