//  ***** BEGIN LICENSE BLOCK *****
// Version: MPL 1.1
// 
// The contents of this file are subject to the Mozilla Public License  
// Version
// 1.1 (the "License"); you may not use this file except in compliance  
// with
// the License. You may obtain a copy of the License at
// http://www.mozilla.org/MPL/
// 
// Software distributed under the License is distributed on an "AS IS"  
// basis,
// WITHOUT WARRANTY OF ANY KIND, either express or implied. See the  
// License
// for the specific language governing rights and limitations under the
// License.
// 
// The Original Code is Bespin.
// 
// The Initial Developer of the Original Code is Mozilla.
// Portions created by the Initial Developer are Copyright (C) 2009
// the Initial Developer. All Rights Reserved.
// 
// Contributor(s):
// 
// ***** END LICENSE BLOCK *****
//  

dojo.provide("th.helpers");

dojo.mixin(th.helpers, {
    isPercentage: function(str){  // TODO: make more robust 
        return (str.indexOf && str.indexOf("%") != -1);
    },

    isCssPixel: function(str) {
        str = dojo.trim(str).toLowerCase();
        return (str.indexOf("px") == str.length - 2);
    }
});

dojo.declare("th.helpers.EventHelpers", null, { 
    // only works on a Scene at the moment as it uses  
    wrapEvent: function(e, root) {
        // compute the canvas-local coordinates
        var coords = dojo.coords(this.canvas, true);
        var x = e.clientX - coords.x;
        var y = e.clientY - coords.y;
        
        var component = root.getComponentForPosition(x, y, true);
        e.thComponent = component;

        this.addComponentXY(e, root, component);
    },
    
    // changes clientX and clientY from space of source to space of dest; no change wraught if dest incompatible with source
    addComponentXY: function(e, source, dest) {
        if (!dest.bounds) {
            console.log("No dest bounds - " + dest.declaredClass);
            console.log(dest.bounds);
            console.log(dest);
            return;
        }

        // compute the canvas-local coordinates
        var coords = dojo.coords(this.canvas, true);
        var x = e.clientX - coords.x;
        var y = e.clientY - coords.y;
   
        var nxy = { x: x, y: y };

        var c = dest;
        while (c) {
            nxy.x -= c.bounds.x;
            nxy.y -= c.bounds.y;
            c = c.parent;

            if (c == source) {
                e.componentX = nxy.x;
                e.componentY = nxy.y;
                return;
            }
        }
    }
}); 
  
dojo.declare("th.helpers.ComponentHelpers", null, {  	
    // returns hash with some handy short-cuts for painting
    d: function() {  
        return {
           b: (this.bounds) ? { x: this.bounds.x, y: this.bounds.y, w: this.bounds.width, h: this.bounds.height,
                                iw: this.bounds.width - this.getInsets().left - this.getInsets().right,
                                ih: this.bounds.height - this.getInsets().top - this.getInsets().bottom } : {},
           i: { l: this.getInsets().left, r: this.getInsets().right, t: this.getInsets().top, b: this.getInsets().bottom,
                w: this.getInsets().left + this.getInsets().right, h: this.getInsets().top + this.getInsets().bottom }
        }
    },

    shouldPaint: function() {
        return (this.shouldLayout() && this.style.visibility != "hidden");
    },

    shouldLayout: function() {
        return (this.style.display != "none");
    },

    emptyInsets: function() {
        return { left: 0, right: 0, bottom: 0, top: 0 };
    },

    resolveCss: function() {
        // right now, all components tie into the global resources bucket; this is fine for now but may need to be loaded from the scene
        var resources = th.global_resources;

        // build a map of declarations
        var declarations = {};

        // process the user agent styles first
        var propertyName;
        var sheetTypes = [ "userAgentCss", "userCss", "authorCss" ];
        for (var i = 0; i < sheetTypes.length; i++) {
            // css splits sheets into user agent, user, and author categories, each of which has different priority
            // we'll implement this by having the same code take three passes, dynamically grabbing the appropriate CSS array
            // from the Resources.
            //
            // this will have to change if we support !important as the user gets a final crack at overridding author sheets
            var currentSheet = sheetTypes[i];
            dojo.forEach(resources[currentSheet], function(css) {
                for (var selector in css) {
                    // a selector may be compound (e.g., foo, bar, car {}) so we split it out by comma to treat each piece of
                    // the selector independently
                    var selectorPieces = selector.split(",");
                    for (var s = 0; s < selectorPieces.length; s++) {
                        var selectorPiece = dojo.trim(selectorPieces[s]);

                        // if this selector selects this component, let's add the rules to the declarations bucket
                        if (this.matchesSelector(selectorPiece)) {
                            var properties = css[selector];

                            for (propertyName in properties) {
                                declarations[propertyName] = properties[propertyName];
                            }
                        }
                    }
                }
            }, this);
        }

        this.styles = declarations;
    },

    // only id and class selectors are supported at the moment. it is assumed the passed in value has already been trimmed.
    matchesSelector: function(selector) {
        var s = selector.toLowerCase();

        // universal selector
        if (s == "*") return true;

        // class selector
        if (s.indexOf(".") == 0) {
            if (!this.className) return false;
            if (this.className.toLowerCase() == s.substring(1)) return true;
        }

        // id selector
        if (s.indexOf("#") == 0) {
            if (!this.id) return false;
            return ("#" + this.id) == s;
        }

        // type selector
        var classPieces = this.declaredClass.split(".");
        var clazz = classPieces[classPieces.length - 1].toLowerCase();
        if (clazz == s) return true;

        // type selector / id hybrid
        if (this.id && (s == (clazz + "#" + this.id))) return true;

        // type selector / class hybrid
        if (this.className && (s == (clazz + "." + this.className.toLowerCase()))) return true;

        // simple child selector support, must be "SEL1 > SEL2"
        if (s.indexOf(">") != -1) {
            var ss = s.split(">");

            if (ss.length != 2) {
                console.log("unsupported child selector syntax; must be SEL1 > SEL2, was '" + selector + "'");
                return false;
            }

            if (this.matchesSelector(dojo.trim(ss[1]))) {
                if (!this.parent) return false;
                return (this.parent.matchesSelector(dojo.trim(ss[0])));
            }

            return false;
        }

        // simple ancestor selector support, must be "SEL1 SEL2"
        if (s.indexOf(" ") != -1) {
            var ss = s.split(" ");

            if (ss.length != 2) {
                console.log("unsupported ancestor selector syntax; must be SEL1 SEL2, was '" + selector + "'");
                return false;
            }

            if (this.matchesSelector(ss[1].trim())) {
                var ancestor = this.parent;
                while (ancestor) {
                    if (ancestor.matchesSelector(ss[0].trim())) return true;
                    ancestor = ancestor.parent;
                }

                return false;
            }
        }

        return false;
    },

    // returns the "specificity" index of the selector. -1 means the first is less specific; 0 means they are equal, 1 means the first
    // is more specific
    getSpecificityIndex: function(selector, otherSelector) {
        if (selector == otherSelector) return 0;

        // if one of them is an id match, they win
        if (selector.indexOf("#") == 0) return 1;
        if (otherSelector.indexOf("#") == 0) return -1;

        // for now, we only match on id and type, so 0 is the only option left
        return 0;
    },

    // paints the background of the component using the optionally passed coordinates using CSS properties; if no coordinates are
    // passed, will default to painting the background on the entire component, which is generally the default and expected behavior
    paintBackground: function(ctx, x, y, w, h) {
        if (!x) x = 0;
        if (!y) y = 0;
        if (!w) w = this.bounds.width;
        if (!h) h = this.bounds.height;

        if (this.styles["background-color"]) {
            ctx.fillStyle = this.styles["background-color"];
            ctx.fillRect(x, y, w, h);
        }

        if (this.styles["background-image"]) {
            var img = this.styles["background-image"];
            this.paintImage(ctx, img, this.styles["background-repeat"], this.styles["background-position"], x, y, w, h);
        }
    },

    // paints the image on the ctx using the optional repeat, position, x, y, w, h values, any of which may be undefined
    paintImage: function(ctx, img, repeat, position, x, y, w, h) {
        if (!repeat) repeat = "repeat";
        if (!position) position = "0% 0%";

        if (!x) x = 0;
        if (!y) y = 0;
        if (!w) w = this.bounds.width;
        if (!h) h = this.bounds.height;

        ctx.save();
        try {
            if ((x != 0) || (y != 0)) ctx.translate(x, y);

            // convert the position string into two different numbers
            var pos = position.toLowerCase().split(" ");
            if (pos.length == 1) pos.push("50%");
            if (pos.length != 2) {
                console.log("Unsupported position syntax; only \"X\" or \"X Y\" supported, you passed in \" + position + \"");
                return;
            }

            // order is x, y *unless* one they are keywords, in which case they *might* be reversed
            var xy = [];
            xy[0] = (pos[0] == "top" || pos[0] == "bottom") ? pos[1] : pos[0];
            xy[1] = (pos[0] == "top" || pos[0] == "bottom") ? pos[0] : pos[1];

            // convert positions to percentages if they are keywords
            dojo.forEach(xy, function(p, index) {
                if (p == "top") xy[index] = "0%";
                if (p == "right") xy[index] = "100%";
                if (p == "bottom") xy[index] = "100%";
                if (p == "left") xy[index] = "0%";
                if (p == "center") xy[index] = "50%";
            });

            // convert positions to pixels; if the positions are lengths, the image's origin is drawn at the specified position.
            // if the positions are percentages, the percentage represents both the position at which the image is to be drawn
            // and the amount the origin should be translated before image is drawn (a touch confusing)
            var txy = [0, 0];
            for (var i = 0; i < xy.length; i++) {
                var percentage = th.helpers.isPercentage(xy[i]);
                var pixelPosition = this.convertPositionToPixel(xy[i], (i == 0) ? w : h);
                if (percentage) txy[i] = this.convertPositionToPixel(xy[i], (i == 0) ? img.width : img.height);
                xy[i] = pixelPosition;
            }

            // now we can draw the frickin' picture
            ctx.drawImage(img, xy[0], xy[1]);
        } finally {
            ctx.restore();
        }
    },

    convertPositionToPixel: function(position, totalLength) {
        if (th.helpers.isPercentage(pos)) {
            var per = pos.substring(0, pos.length - 1) / 100;
            return totalLength * per;
        } else if (th.helpers.isCssPixel(pos)) {
            return pos.substring(0, pos.length - 2);
        }
    }
});

dojo.declare("th.helpers.ContainerHelpers", null, {
    getScene: function() {
        var container = this;
        while (!container.scene && container.parent) container = container.parent;
        return container.scene;
    },

    getChildById: function(id) {
        for (var i = 0; i < this.children.length; i++) {
            if (this.children[i].id == id) return this.children[i];
        }
    },

    getComponentForPosition: function(x, y, recurse) {
        for (var i = 0; i < this.children.length; i++) {
            if (!this.children[i].bounds) continue;
            
            if (this.children[i].bounds.x <= x && this.children[i].bounds.y <= y
                    && (this.children[i].bounds.x + this.children[i].bounds.width) >= x
                    && (this.children[i].bounds.y + this.children[i].bounds.height) >= y) {
                if (!recurse) return this.children[i];
                return (this.children[i].getComponentForPosition) ?
                       this.children[i].getComponentForPosition(x - this.children[i].bounds.x, y - this.children[i].bounds.y, recurse) :
                       this.children[i];
            }
        }
        return this;
    },

    removeAll: function() {
        this.remove(this.children);
    }
});