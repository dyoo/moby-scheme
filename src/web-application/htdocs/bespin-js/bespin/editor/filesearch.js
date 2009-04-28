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

dojo.provide("bespin.editor.filesearch");

dojo.declare("bespin.editor.filesearch.Panel", th.components.Panel, {
    constructor: function(parms) {
        if (!parms) parms = {};

        this.findLabel = new th.components.Label({ style: { color: "black", font: "8pt Tahoma" }, text: "Find:" });
        this.replaceLabel = new th.components.Label({ style: { color: "black", font: "8pt Tahoma" }, text: "Replace:" });
        this.add([ this.findLabel, this.replaceLabel ]);

        // this is a closed container
        delete this.add;
        delete this.remove;
    },

    layout: function() {
        var d = this.d();

        this.findLabel.bounds =    { x: 10, y: 12, width: 50, height: 15};
        this.replaceLabel.bounds = { x: 10, y: 36, width: 100, height: 15};
        /*var y = d.i.t;

        lh = this.pathLabel.getPreferredHeight(d.b.w);

        y = 28;
        var sw = 14;
        var sh = d.b.h - d.i.b - y - lh;
        var innerWidth = d.b.w - d.i.w - sw;
        
        this.list.bounds = { x: d.i.l, y: y, width: innerWidth + sw, height: sh - 1 };
        this.pathLabel.bounds = { x: d.i.l, y: y + sh, width: innerWidth + sw, height: lh };*/
    },
    
    paintSelf: function(ctx) {      
        var d = this.d();
        ctx.fillStyle = "#D5D0C0";
        ctx.fillRect(0, 0, d.b.w, d.b.h);
        
/*      ctx.lineWidth = 1;
        ctx.strokeStyle = "black";
        ctx.beginPath()
        ctx.moveTo(0, 50.5);
        ctx.lineTo(d.b.w, 50.5);
        
        ctx.stroke();*/
    }
});

dojo.declare("bespin.editor.filesearch.API", null, {
    constructor: function() {                
        
        this.serachTimeout = null;
        
        // create the Window!
        this.panel = new bespin.editor.filesearch.Panel();
        this.window = new th.Window({
            title: 'Find & Replace', 
            top: 100, 
            left: 200, 
            width: 290, 
            height: 115, 
            userPanel: this.panel,
            containerId: 'filesearch',
            closeOnClickOutside: true
        });
        this.window.centerUp(); // center the window, but more a "human" center

        // add the input field to the window
        this.searchInput = dojo.byId('search_text');
        this.replaceInput = dojo.byId('replace_text');
        this.nextButton = dojo.byId('bu_search_next');
        this.prevButton = dojo.byId('bu_search_prev');
        
        // listen to the bus, whether the window got toggled
        this.window.scene.bus.bind("toggle", this.window, dojo.hitch(this, function(e) {            
            if (e.isVisible) {
                this.searchInput.focus();
                // the editor has no longer the focus
                bespin.get('editor').setFocus(false);
            } else {
                bespin.get('editor').setFocus(true);
            } 
        }));
        
        dojo.connect(this.searchInput, 'keyup', dojo.hitch(this, function(e) {
            if (!this.window.isVisible) return; // short circuit if the popup isn't up
            
            if (this.serachTimeout) {
                clearTimeout(this.serachTimeout);
            }
            this.serachTimeout = setTimeout(dojo.hitch(this, function () {
                bespin.get('editor').ui.setSearchString(this.searchInput.value);
            }), 100);
        }));
        
        dojo.connect(this.searchInput, 'keydown', dojo.hitch(this, function(e) {
            if (!this.window.isVisible) return; // short circuit if the popup isn't up
            
            var key = bespin.util.keys.Key;
            
            if (e.keyCode == key.ENTER) {
                bespin.get('actions').findNext();
            }
        }));
        
        dojo.connect(this.nextButton, 'click', dojo.hitch(this, function(e) {
            if (!this.window.isVisible) return; // short circuit if the popup isn't up
            
            bespin.get('actions').findNext();
        }));
        
        dojo.connect(this.prevButton, 'click', dojo.hitch(this, function(e) {
            if (!this.window.isVisible) return; // short circuit if the popup isn't up
            
            bespin.get('actions').findPrev();
        }));
    },
});