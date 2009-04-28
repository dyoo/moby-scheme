dojo.provide("thx.test");

dojo.require("bespin.util.canvas");
dojo.require("bespin.util.keys");
dojo.require("bespin.util.navigate");
dojo.require("bespin.util.path");
dojo.require("bespin.util.tokenobject");
dojo.require("bespin.util.util");
dojo.require("bespin.util.mousewheelevent");
dojo.require("bespin.util.urlbar");

dojo.require("bespin.editor.actions");
dojo.require("bespin.editor.editor");
dojo.require("bespin.editor.model");
dojo.require("bespin.editor.toolbar");
dojo.require("bespin.editor.undo");

dojo.require("bespin.themes.default");

dojo.require("th.helpers");
dojo.require("th.css");
dojo.require("th.th");
dojo.require("th.models");
dojo.require("th.borders");
dojo.require("th.components"); 

dojo.require("thx.textarea"); 

dojo.addOnLoad(function(){                                                       
    var scene;
    dojo.connect(window, 'resize', doResize);
    
    scene = new th.Scene(dojo.byId("textarea"));
    scene.smartRedraw = true;
    var testText = new thx.textarea.TextArea({ style: {
        backgroundColor: "rgb(48, 33, 28)",
        backgroundColorOdd: "rgb(82, 80, 71)",
        font: "10pt Monaco, Lucida Console, monospace",
        color: "white",
        scrollTopImage: dojo.byId("vscroll_track_top"),
        scrollMiddleImage: dojo.byId("vscroll_track_middle"),
        scrollBottomImage: dojo.byId("vscroll_track_bottom"),
        scrollHandleTopImage: dojo.byId("vscroll_top"),
        scrollHandleMiddleImage: dojo.byId("vscroll_middle"),
        scrollHandleBottomImage: dojo.byId("vscroll_bottom"),
        scrollUpArrow: dojo.byId("vscroll_up_arrow"),
        scrollDownArrow: dojo.byId("vscroll_down_arrow")
    }});
    var doc = new thx.textarea.DocumentModel();
    doc.insertDocument("dojo.declare(\"th.components.Scrollbar2\", th.Container, {\n    constructor: function(parms) {\n        if (!parms) parms = {};\n        this.orientation = parms.orientation || th.VERTICAL;\n        this.value = parms.value || 0;\n        this.min = parms.min || 0;\n        this.max = parms.max || 100;\n        this.increment = parms.increment || 2;\n\n        this.up = new th.components.Button();\n        this.down = new th.components.Button();\n        this.bar = new th.components.Button();\n        this.add([ this.up, this.down, this.bar ]);\n\n        this.bus.bind(\"click\", this.up, this.scrollup, this);\n        this.bus.bind(\"click\", this.down, this.scrolldown, this);\n        this.bus.bind(\"mousedrag\", this.bar, this.onmousedrag, this);\n        this.bus.bind(\"mouseup\", this.bar, this.onmouseup, this);\n    },\n\n    onmousedrag: function(e) {\n        var currentPosition = (this.orientation == th.VERTICAL) ? e.clientY : e.clientX;\n\n        if (this.dragstart_mouse === undefined) {\n            this.dragstart_mouse = currentPosition;\n            return;\n        }\n\n        // difference in pixels; needs to be translated to a difference in value\n        var diff = currentPosition - this.dragstart_mouse;\n        this.dragstart_mouse = currentPosition;\n        \n        // the difference in the value\n        var delta;\n        // Math.floor works differently for negative and positive numbers\n        // (it rounds towards -infinity), so if diff is negative, it will\n        // scroll \"slower\" than it would if delta is positive.\n        // To correct it, I seperate handling according to the sign of diff.\n//        if (diff > 0)\n//            delta = Math.floor(diff / this.ratio);\n//        else\n//            delta = -Math.floor(-diff / this.ratio);\n        delta = diff / this.ratio;\n        this.value += delta;\n        if (this.value < this.min) this.value = this.min;\n        if (this.value > this.max) this.value = this.max;\n        this.layout();\n        if (this.scrollable) \n            if (delta > 0)\n                this.scrollable.scrollDown(delta);\n            else\n                this.scrollable.scrollUp(-delta);\n        this.repaint();\n        if (this.scrollable) this.scrollable.repaint();\n    },\n\n    onmouseup: function(e) {\n        delete this.dragstart_value;\n        delete this.dragstart_mouse;\n    },\n\n    scrollup: function(e) {\n        if (this.value > this.min) {\n            this.value = Math.min(this.min, this.value - this.increment);\n            if (this.scrollable) this.scrollable.scrollUp(this.increment);\n            this.render();\n            if (this.scrollable) this.scrollable.repaint();\n        }\n    },\n\n    scrolldown: function(e) {\n        if (this.value < this.max) {\n            this.value = Math.min(this.max, this.value + this.increment);\n            if (this.scrollable) this.scrollable.scrollDown(this.increment)");
    scene.root.add(testText);
    doResize();
    scene.layout();
    scene.render();
    testText.assignModel(doc);
});

function doResize() {
    dojo.attr('textarea', {width: window.innerWidth, height: window.innerHeight});
}
