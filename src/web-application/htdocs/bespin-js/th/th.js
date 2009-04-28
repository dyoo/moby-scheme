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

dojo.provide("th.th");  

/*
    Constants
 */ 
dojo.mixin(th, {
    VERTICAL: "v",
    HORIZONTAL: "h"    
});

// ** {{{ Resources }}} **
//
// Loads those resources that are shared with all Scenes on the same page, like CSS and layout information.
// Typically there is no need to instantiate this class; a Scene will do it automatically. However, for performance, you may
// wish to eagerly instantiate an instance to request resources earlier in the page realization process.
//
// property cssLoaded
dojo.declare("th.Resources", null, {
    constructor: function() {
        this.loading = false;

        // an array containing objects that correspond to each matching stylesheet in the order specified
        this.userAgentCss = [];
        this.authorCss = [];
        this.userCss = [];

        this.blockUntilImagesLoaded = true;

        // images
        this.images = {};

        // used during the CSS loading process; due to callbacks, this is pushed top-level so it can be shared across functions
        this.sheetCount = 0;
        this.currentSheet = 0;

        // used during image loading process
        this.imageCount = 0;
        this.currentImage = 0;

        this.loaded = false;    // are all the resources loaded, including CSS and other stuff?
        this.cssLoaded = false; // are all the CSS sheets loaded?
        this.imagesLoaded = false;  // are all the images references by the CSS loaded?

        this.callbacks = [];
    },

    load: function() {
        if (this.loaded) return;    // no re-loading
        
        this.loading = true;
        this.parseCSS();
    },

    processImage: function() {
        this.currentImage++;
        if (this.imageCount == this.currentImage) {
            this.imagesLoaded = true;
            this.onLoaded();
        }
    },

    onLoaded: function() {
        if (this.cssLoaded && ((this.blockUntilImagesLoaded && this.imagesLoaded) || !this.blockUntilImagesLoaded)) {
            this.loaded = true;
            this.loading = false;
            if (this.callbacks) {
                dojo.forEach(this.callbacks, function(item) {
                    // check if there is context; if so, execute the callback using the context
                    if (item.context) {
                        item.callback.apply(item.context);
                    } else {
                        item.callback();
                    }
                });
            }
        }
    },

    registerOnLoadCallback: function(callback, context) {
        this.callbacks.push({ callback: callback, context: context });
    },

    parseCSS: function() {
        var links = [];

        // add default stylesheet; cheesy path at the moment, need to come up with a better way to approach this TODO
        links.push({ url: "/js/th/default.css", array: this.userAgentCss, index: 0 });

        var s, l = document.getElementsByTagName('link'), counter = 0;
		for (var i=0; i < l.length; i++){
		    s = l[i];
			if (s.rel.toLowerCase().indexOf('thstylesheet') >= 0 && s.href) {
			    links.push({ url: s.href, array: this.authorCss, index: counter++ });
			}
		}

        // this shouldn't happen; we should always have at least one userAgentCss otherwise things are going to be mighty sparse
        if (links.length == 0) {
            this.cssLoaded = true;
            return this.onLoaded();
        }

        this.sheetCount = links.length;
        dojo.forEach(links, function(link) {
            dojo.xhrGet({
                url: link.url,
                load: dojo.hitch(this, function(response) {
                    this.processCSS(response, link.array, link.index );
                })
            });
        }, this);
    },

    processCSS: function(stylesheet, array, index) {
        array[index] = new th.css.CSSParser().parse(stylesheet);

        // load the images
        for (var rule in array[index]) {
            for (var property in array[index][rule]) {
                var value = array[index][rule][property];
                if (value.indexOf("url(") == 0 && value.indexOf(")") == value.length - 1) {
                    var url = value.substring(4, value.length - 1);

                    this.imageCount++;
                    var image = new Image();

                    if (this.blockUntilImagesLoaded) {
                        this.imagesLoaded = false;
                        dojo.connect(image, "onload", this, this.processImage);
                    }

                    image.src = "." + url;
                    this.images[value] = image;

                    // swap out the value in the CSS with an image; not sure this is the right way to go
                    array[index][rule][property] = image;
                }
            }
        }

        if (++this.currentSheet == this.sheetCount) {
            this.cssLoaded = true;
            this.onLoaded();
        }
    }
});

/*
    Event bus; all listeners and events pass through a single global instance of this class.
 */ 
dojo.declare("th.Bus", null, {
    constructor: function() {
        // map of event name to listener; listener contains a selector, function, and optional context object
        this.events = {};
    },

    // register a listener with an event
    // - event: string name of the event
    // - selector: 
    bind: function(event, selector, listenerFn, listenerContext) {
        var listeners = this.events[event];
        if (!listeners) {
            listeners = [];
            this.events[event] = listeners;
        }
        selector = dojo.isArray(selector) ? selector : [ selector ];
        for (var z = 0; z < selector.length; z++) {
            for (var i = 0; i < listeners.length; i++) {
                if (listeners[i].selector == selector[z] && listeners[i].listenerFn == listenerFn) return;
            }
            listeners.push({ selector: selector[z], listenerFn: listenerFn, context: listenerContext });
        }
    },

    // removes any listeners whose selectors have the *same identity* as the passed selector
    unbind: function(selector) {
        for (var event in this.events) {
            var listeners = this.events[event];

            for (var i = 0; i < listeners.length; i++) {
                if (listeners[i].selector === selector) {
                    this.events[event] = dojo.filter(listeners, function(item){ return item != listeners[i]; });                    
                    listeners = this.events[event];
                    i--;
                }
            }
        }
    },

    // notify all listeners of an event
    fire: function(eventName, eventDetails, component) {
        var listeners = this.events[eventName];
        if (!listeners || listeners.length == 0) return;

        // go through each listener registered for the fired event and check if the selector matches the component for whom
        // the event was fired; if there is a match, dispatch the event
        for (var i = 0; i < listeners.length; i++) {
            // if the listener selector is a string...
            if (listeners[i].selector.constructor == String) {
                // check if the string starts with a hash, indicating that it should match by id
                if (listeners[i].selector.charAt(0) == "#") {
                    if (component.id == listeners[i].selector.substring(1)) {
                        this.dispatchEvent(eventName, eventDetails, component, listeners[i]);
                    }
                // otherwise check if it's the name of the component class
                } else if (listeners[i].selector == component.declaredClass) {
                    this.dispatchEvent(eventName, eventDetails, component, listeners[i]);
                }
            // otherwise check if the selector is the current component
            } else if (listeners[i].selector == component) {
                this.dispatchEvent(eventName, eventDetails, component, listeners[i]);
            }
        }
    },

    // invokes the listener function
    dispatchEvent: function(eventName, eventDetails, component, listener) {
        eventDetails.thComponent = component;

        // check if there is listener context; if so, execute the listener function using that as the context
        if (listener.context) {
            listener.listenerFn.apply(listener.context, [ eventDetails ]);
        } else {
            listener.listenerFn(eventDetails);
        }
    }
});

// create the global event bus
th.global_event_bus = new th.Bus();

// create the global resource loader loader
th.global_resources = new th.Resources();

dojo.declare("th.Scene", th.helpers.EventHelpers, { 
    bus: th.global_event_bus,

    constructor: function(canvas) {
        this.canvas = canvas;

        // whether this scene completely repaints on each render or does something smarter. this is experimental.
        this.smartRedraw = false;

        // aliasing global resources to be a member; not yet clear how components will typically get access to resources, whether
        // through scene or the global
        this.resources = th.global_resources;

        // has this scene registered a render callback? this is done if render() invoked before resources are all loaded
        this.resourceCallbackRegistered = false;

        // if the resource loading process hasn't started, start it!
        if (!this.resources.loaded && !this.resources.loading) this.resources.load();

        dojo.connect(window, "resize", dojo.hitch(this, function() {
            this.render();
        })); 

        this.root = new th.components.Panel({ id: "root" });
        this.root.scene = this; 

        var testCanvas = document.createElement("canvas");
        this.scratchContext = testCanvas.getContext("2d");
        bespin.util.canvas.fix(this.scratchContext);

        dojo.connect(window, "mousedown", dojo.hitch(this, function(e) {
            this.wrapEvent(e, this.root);

            this.mouseDownComponent = e.thComponent;

            th.global_event_bus.fire("mousedown", e, e.thComponent);
        }));

        dojo.connect(window, "dblclick", dojo.hitch(this, function(e) {
            this.wrapEvent(e, this.root);

            th.global_event_bus.fire("dblclick", e, e.thComponent);
        }));

        dojo.connect(window, "click", dojo.hitch(this, function(e) {
            this.wrapEvent(e, this.root);

            th.global_event_bus.fire("click", e, e.thComponent);
        }));

        dojo.connect(window, "mousemove", dojo.hitch(this, function(e) {
            this.wrapEvent(e, this.root);

            th.global_event_bus.fire("mousemove", e, e.thComponent);

            if (this.mouseDownComponent) {
                this.addComponentXY(e, this.root, this.mouseDownComponent);
                th.global_event_bus.fire("mousedrag", e, this.mouseDownComponent);
            }
        }));

        dojo.connect(window, "mouseup", dojo.hitch(this, function(e) {
            if (!this.mouseDownComponent) return;

            this.addComponentXY(e, this.root, this.mouseDownComponent);
            th.global_event_bus.fire("mouseup", e, this.mouseDownComponent);

            delete this.mouseDownComponent;
        }));
    },

    render: function() {
        if (!this.resources.loaded) {
            if (!this.resourceCallbackRegistered) {
                this.resources.registerOnLoadCallback(this.render, this);
                this.resourceCallbackRegistered = true;
            }

            return;
        }

        this.layout();
        this.paint();
    },

    layout: function() {
        if (this.root) {
            this.root.bounds = { x: 0, y: 0, width: this.canvas.width, height: this.canvas.height };
            this.root.layoutTree();
        }
    },

    paint: function(component) {
        if (!this.resources.loaded) {
            if (!this.resourceCallbackRegistered) {
                this.resources.registerOnLoadCallback(this.render, this);
                this.resourceCallbackRegistered = true;
            }

            return;
        }

        if (!component) component = this.root;

        if (component) {
            if (!component.opaque && component.parent) {
                return this.paint(component.parent);
            }

            var ctx = this.canvas.getContext("2d");
            bespin.util.canvas.fix(ctx);

            ctx.save();

            var parent = component.parent;
            var child = component;
            while (parent) {
                try {
                    ctx.translate(child.bounds.x, child.bounds.y);
                } catch (e) {
                    console.log("translate error (" + child.type + ")");
                    console.log(child.bounds);
                    return;
                }
                child = parent;
                parent = parent.parent;
            }
            
            if (!this.smartRedraw) {
                ctx.clearRect(0, 0, component.bounds.width, component.bounds.height);
            }
            ctx.beginPath();
            ctx.rect(0, 0, component.bounds.width, component.bounds.height);
            ctx.closePath();
            ctx.clip(); 
            component.paint(ctx);  
            
            ctx.restore();
        }
    }
});

dojo.declare("th.Border", th.helpers.ComponentHelpers, {
    constructor: function(parms) {
        if (!parms) parms = {};
        this.style = parms.style || {};
        this.attributes = parms.attributes || {};
    },

    getInsets: function() {
        return this.emptyInsets();
    },

    paint: function(ctx) {}
});   
    
dojo.declare("th.Component", th.helpers.ComponentHelpers, {
    constructor: function(parms) { 
        if (!parms) parms = {};
        this.bounds = parms.bounds || {};
        this.style = parms.style || {};
        this.styles = parms.styles || {};
        this.className = parms.className;
        this.attributes = parms.attributes || {};
        this.id = parms.id;
        this.border = parms.border;
        this.opaque = parms.opaque || true;
    
        this.bus = th.global_event_bus; 
    },
    
    // used to obtain a throw-away canvas context for performing measurements, etc.; may or may not be the same canvas as that used to draw the component
    getScratchContext: function() {
        var scene = this.getScene();
        if (scene) return scene.scratchContext;
    },
    
    getPreferredHeight: function(width) {},
    
    getPreferredWidth: function(height) {},
    
    getInsets: function() {
        return (this.border) ? this.border.getInsets() : this.emptyInsets();
    },
    
    paint: function(ctx) {},
    
    repaint: function() {
        // todo: at present, there are some race conditions that cause painting to be invoked before a scene is ready, so this
        // check is necessary to bail. We need to work out better rules for scenes and components, etc.
        if (!this.getScene()) return;
        
        this.getScene().paint(this);
    }
});

dojo.declare("th.Container", [th.Component, th.helpers.ContainerHelpers], {
    constructor: function() {       
        this.children = [];
    },
    
    add: function() {
        for (var z = 0; z < arguments.length; z++) {
            component = dojo.isArray(arguments[z]) ? arguments[z] : [ arguments[z] ]; 
            this.children = this.children.concat(component);
            for (var i = 0; i < component.length; i++) {
                component[i].parent = this;
            }
        }
    },

    remove: function() {
        for (var z = 0; z < arguments.length; z++) {
            component = dojo.isArray(arguments[z]) ? arguments[z] : [ arguments[z] ];
            for (var i = 0; i < component.length; i++) {
                var old_length = this.children.length;
                this.children = dojo.filter(this.children, function(item){ return item != component[i]; });

                // if the length of the array has changed since I tried to remove the current component, assume it was removed and clear the parent
                if (old_length != this.children.length) delete component[i].parent;
            }
        }
    },
    
    replace: function(component, index) {
        this.bus.unbind(this.children[index]);
        component.parent = this;
        this.children[index] = component;
    },

    paint: function(ctx) {
        if (this.shouldPaint()) {
            this.paintSelf(ctx);
            this.paintChildren(ctx);
        }
    },

    paintSelf: function(ctx) {},

    paintChildren: function(ctx) {
        for (var i = 0; i < this.children.length; i++ ) {
            if (!this.children[i].shouldPaint()) continue;

            if (!this.children[i].bounds) {
                // console.log("WARNING: child " + i + " (type: " + this.children[i].declaredClass + ", id: " + this.children[i].id + ") of parent with id " + this.id + " of type " + this.declaredClass + " has no bounds and could not be painted");
                continue;
            }

            ctx.save();
            try {
                ctx.translate(this.children[i].bounds.x, this.children[i].bounds.y);
            } catch (error) {
                // console.log("WARNING: child " + i + " (type: " + this.children[i].declaredClass + ", id: " + this.children[i].id + ") of parent with id " + this.id + " of type " + this.declaredClass + " has malformed bounds and could not be painted");
                // console.log(this.children[i].bounds);
                ctx.restore();
                continue;
            }

            try {
                if (!this.children[i].style["noClip"]) {
                    ctx.beginPath();
                    ctx.rect(0, 0, this.children[i].bounds.width, this.children[i].bounds.height);
                    ctx.closePath();
                    ctx.clip();
                }
            } catch(ex) {
                // console.log("Bounds problem");
                // console.log(this.children[i].declaredClass);
                // console.log(this.children[i].bounds);
            }

            ctx.save();
            this.children[i].resolveCss();
            this.children[i].paint(ctx);
            ctx.restore();

            if (this.children[i].style.border) {
                this.children[i].style.border.component = this.children[i];
                ctx.save();
                this.children[i].style.border.paint(ctx);
                ctx.restore();
            }

            ctx.restore();
        }
    },

    // lays out this container and any sub-containers
    layoutTree: function() {
        this.layout();
        for (var i = 0; i < this.children.length; i++) {  
            if (this.children[i].layoutTree) this.children[i].layoutTree();
        }
    },

    layout: function() {
        var d = this.d();
        if (this.children.length > 0) {
            var totalWidth = this.bounds.width - d.i.w;
            var individualWidth = totalWidth / this.children.length;
            for (var i = 0; i < this.children.length; i++) {
                this.children[i].bounds = { x: (i * individualWidth) + d.i.l, y: d.i.t, width: individualWidth, height: this.bounds.height - d.i.h };
            }
        }
    },

    render: function() {
        if (!th.global_resources.loaded) return;

        this.layoutTree();
        this.repaint();
    }
});

dojo.declare("th.Window", null, {
    constructor: function(parms) {        
        parms = parms || {};
        
        this.containerId = parms.containerId || false;
        this.width = parms.width || 200;
        this.height = parms.height || 300;
        this.title = parms.title || 'NO TITLE GIVEN!';
        this.y = parms.top || 50;
        this.x = parms.left || 50;
        this.isVisible = false;
        this.closeOnClickOutside = !!parms.closeOnClickOutside;

        // some things must be given
        if(!parms.containerId) {
            console.error('The "containerId" must be given!');
            return;            
        }
        
        // for the moment, this is done by hand!
        // if (dojo.byId(this.containerId)) {
        //             console.error('There is already a element with the id "'+this.containerId+'"!');
        //             return;
        //         }
                
        if (!parms.userPanel) {
            console.error('The "userPanel" must be given!');
            return;
        }
        
        /*if (!dojo.byId('popup_insert_point')) {
            // there is no place to add the popups => create one
            for (var x = 0; x < document.childNodes.length; x++) {
                if (document.childNodes[x].nodeType == 1) {
                    // thats the place to add the pop_insert_point
                    var popupParent = document.createElement("div");
                    popupParent.id = 'popup_insert_point';
                    document.childNodes[x].appendChild(popupParent);
                    break;
                }
            }
        }*/
        
        // insert the HTML to the document for the new window and create the scene
        // dojo.byId('popup_insert_point').innerHTML += '<div id="'+this.containerId+'" class="popupWindow"></div>';
        this.container = dojo.byId(this.containerId);
        dojo.attr(this.container, { width: this.width, height: this.height, tabindex: '-1' });

        this.container.innerHTML += "<canvas id='"+this.containerId+"_canvas'></canvas>";
        this.canvas = dojo.byId(this.containerId + '_canvas');
        dojo.attr(this.canvas, { width: this.width, height: this.height, tabindex: '-1' });
        
        this.scene = new th.Scene(this.canvas);
        this.windowPanel = new th.components.WindowPanel(parms.title, parms.userPanel);
        this.windowPanel.windowBar.parentWindow = this;  
        this.scene.root.add(this.windowPanel);
        
        this.move(this.x, this.y);
        
        // add some listeners for closing the window
        
        // close the window, if the user clicks outside the window
        dojo.connect(window, "mousedown", dojo.hitch(this, function(e) {
            if (!this.isVisible || !this.closeOnClickOutside) return;

            var d = dojo.coords(this.container);
            if (e.clientX < d.l || e.clientX > (d.l + d.w) || e.clientY < d.t || e.clientY > (d.t + d.h)) {
                this.toggle();
            }
        }));
        
        // close the window, if the user pressed ESCAPE
        dojo.connect(window, "keydown", dojo.hitch(this, function(e) {
            if (!this.isVisible) return;
            
            if(e.keyCode == bespin.util.keys.Key.ESCAPE) {
                this.toggle();
                dojo.stopEvent(e);
            }
        }));
    }, 
         
    toggle: function() {
        this.isVisible = !this.isVisible;
                
        if (this.isVisible) {
            this.container.style.display = 'block';
            this.layoutAndRender();
        } else {
            this.container.style.display = 'none';
        }
        
        this.scene.bus.fire("toggle", {isVisible: this.isVisible}, this);
    },
    
    layoutAndRender: function() {
        this.scene.layout();
        this.scene.render();
    },
    
    centerUp: function() {
        this.move(Math.round((window.innerWidth - this.width) * 0.5), Math.round((window.innerHeight - this.height) * 0.25));
    },
    
    center: function() {
        this.move(Math.round((window.innerWidth - this.width) * 0.5), Math.round((window.innerHeight - this.height) * 0.5));
    },
    
    move: function(x, y) {
        this.y = y;
        this.x = x;
        this.container.style.top = y + 'px';
        this.container.style.left = x + 'px';
    },
    
    getPosition: function() {
        return { x: this.x, y: this.y };
    }
});
