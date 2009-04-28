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

dojo.provide("bespin.editor.editor");

dojo.require("bespin.editor.clipboard");

// = Editor =
//
// This is the guts. The metal. The core editor has most of its classes living in here:
//
// * {{{bespin.editor.API}}} : The editor API itself
// * {{{bespin.editor.UI}}} : Knowledge of the UI pieces of the editor is here, bulk of the code. paints!
// * {{{bespin.editor.Scrollbar}}} : The custom scrollbar (to be factored out and use TH scrollbar instead)
// * {{{bespin.editor.SelectionHelper}}} : Handle text selection
// * {{{bespin.editor.DefaultEditorKeyListener}}} : Key listener operations
// * {{{bespin.editor.Rect}}} : Helper to hold a rectangle
// * {{{bespin.editor.Events}}} : Helper to hold a rectangle
// * {{{bespin.editor.Utils}}} : Blobby utility object to do common things
//
// * {{{bespin.editor.Actions}}} : The actions that the editor can do (can be added too) are are in actions.js

// ** {{{ bespin.editor.Scrollbar }}} **
//
// some state mgmt. for scrollbars; not a true component
dojo.declare("bespin.editor.Scrollbar", null, {
    HORIZONTAL: "horizontal",
    VERTICAL: "vertical",
    MINIMUM_HANDLE_SIZE: 20,

    constructor: function(ui, orientation, rect, value, min, max, extent) {
        this.ui = ui;
        this.orientation = orientation; // "horizontal" or "vertical"
        this.rect = rect;       // position/size of the scrollbar track
        this.value = value;     // current offset value
        this.min = min;         // minimum offset value
        this.max = max;         // maximum offset value
        this.extent = extent;   // size of the current visible subset

        this.mousedownScreenPoint;    // used for scroll bar dragging tracking; point at which the mousedown first occurred
        this.mousedownValue;          // value at time of scroll drag start
    },

    // return a Rect for the scrollbar handle
    getHandleBounds: function() {
        var sx = (this.isH()) ? this.rect.x : this.rect.y;
        var sw = (this.isH()) ? this.rect.w : this.rect.h;

        var smultiple = this.extent / (this.max + this.extent);
        var asw = smultiple * sw;
        if (asw < this.MINIMUM_HANDLE_SIZE) asw = this.MINIMUM_HANDLE_SIZE;

        sx += (sw - asw) * (this.value / (this.max - this.min));

        return (this.isH()) ? new bespin.editor.Rect(Math.floor(sx), this.rect.y, asw, this.rect.h) : new bespin.editor.Rect(this.rect.x, sx, this.rect.w, asw);
    },

    isH: function() {
        return (!(this.orientation == this.VERTICAL));
    },

    fixValue: function(value) {
        if (value < this.min) value = this.min;
        if (value > this.max) value = this.max;
        return value;
    },

    onmousewheel: function(e) {
        var wheel = bespin.util.mousewheelevent.wheel(e);
        var axis = bespin.util.mousewheelevent.axis(e);

        if (this.orientation == this.VERTICAL && axis == this.VERTICAL) {
            this.setValue(this.value + (wheel * this.ui.lineHeight));
        } else if (this.orientation == this.HORIZONTAL && axis == this.HORIZONTAL) {
            this.setValue(this.value + (wheel * this.ui.charWidth));
        }
    },

    onmousedown: function(e) {
        var clientY = e.clientY - this.ui.getTopOffset();
        var clientX = e.clientX - this.ui.getLeftOffset();

        var bar = this.getHandleBounds();
        if (bar.contains({ x: clientX, y: clientY })) {
            this.mousedownScreenPoint = (this.isH()) ? e.screenX : e.screenY;
            this.mousedownValue = this.value;
        } else {
            var p = (this.isH()) ? clientX : clientY;
            var b1 = (this.isH()) ? bar.x : bar.y;
            var b2 = (this.isH()) ? bar.x2 : bar.y2;

            if (p < b1) {
                this.setValue(this.value -= this.extent);
            } else if (p > b2) {
                this.setValue(this.value += this.extent);
            }
        }
    },

    onmouseup: function(e) {
        this.mousedownScreenPoint = null;
        this.mousedownValue = null;
        if (this.valueChanged) this.valueChanged(); // make the UI responsive when the user releases the mouse button (in case arrow no longer hovers over scrollbar)
    },

    onmousemove: function(e) {
        if (this.mousedownScreenPoint) {
            var diff = ((this.isH()) ? e.screenX : e.screenY) - this.mousedownScreenPoint;
            var multiplier = diff / (this.isH() ? this.rect.w : this.rect.h);
            this.setValue(this.mousedownValue + Math.floor(((this.max + this.extent) - this.min) * multiplier));
        }
    },

    setValue: function(value) {
        this.value = this.fixValue(value);
        if (this.valueChanged) this.valueChanged();
    }
});

// ** {{{ bespin.editor.Rect }}} **
//
// treat as immutable (pretty please)
dojo.declare("bespin.editor.Rect", null, {
    constructor: function(x, y, w, h) {
        this.x = x;
        this.y = y;
        this.w = w;
        this.h = h;
        this.x2 = x + w;
        this.y2 = y + h;
    },

    // inclusive of bounding lines
    contains: function(point) {
        if (!this.x) return false;
        return ((this.x <= point.x) && ((this.x + this.w) >= point.x) && (this.y <= point.y) && ((this.y + this.h) >= point.y));
    }
});

// ** {{{ bespin.editor.SelectionHelper }}} **
dojo.declare("bespin.editor.SelectionHelper", null, {
    constructor: function(editor) {
        this.editor = editor;
    },

    // returns an object with the startCol and endCol of the selection. If the col is -1 on the endPos, the selection goes for the entire line
    // returns undefined if the row has no selection
    getRowSelectionPositions: function(rowIndex) {
        var startCol;
        var endCol;

        var selection = this.editor.getSelection();
        if (!selection) return undefined;
        if ((selection.endPos.row < rowIndex) || (selection.startPos.row > rowIndex)) return undefined;

        startCol = (selection.startPos.row < rowIndex) ? 0 : selection.startPos.col;
        endCol = (selection.endPos.row > rowIndex) ? -1 : selection.endPos.col;

        return { startCol: startCol, endCol: endCol };
    }
});

// ** {{{ bespin.editor.utils }}} **
//
// Mess with positions mainly
dojo.mixin(bespin.editor, { utils: {
    buildArgs: function(oldPos) {
        return { pos: bespin.editor.utils.copyPos(oldPos || bespin.get('editor').getCursorPos()) };
    },

    changePos: function(args, pos) {
        return { pos: bespin.editor.utils.copyPos(oldPos || bespin.get('editor').getCursorPos()) };
    },

    copyPos: function(oldPos) {
        return { row: oldPos.row, col: oldPos.col };
    },

    posEquals: function(pos1, pos2) {
        if (pos1 == pos2) return true;
        if (!pos1 || !pos2) return false;
        return (pos1.col == pos2.col) && (pos1.row == pos2.row);
    },

    diffObjects: function(o1, o2) {
        var diffs = {};

        if (!o1 || !o2) return undefined;

        for (var key in o1) {
            if (o2[key]) {
                if (o1[key] != o2[key]) {
                    diffs[key] = o1[key] + " => " + o2[key];
                }
            } else {
                diffs[key] = "o1: " + key + " = " + o1[key];
            }
        }

        for (var key2 in o2) {
            if (!o1[key2]) {
                diffs[key2] = "o2: " + key2 + " = " + o2[key2];
            }
        }
        return diffs;
    }
}});

// ** {{{ bespin.editor.DefaultEditorKeyListener }}} **
//
// Core key listener to decide which actions to run
dojo.declare("bespin.editor.DefaultEditorKeyListener", null, {
    constructor: function(editor) {
        this.editor = editor;
        this.actions = editor.ui.actions;
        this.skipKeypress = false;

        this.defaultKeyMap = {};

        // Allow for multiple key maps to be defined
        this.keyMap = this.defaultKeyMap;
        this.keyMapDescriptions = {};
    },

    bindKey: function(keyCode, metaKey, ctrlKey, altKey, shiftKey, action, name) {
        this.defaultKeyMap[[keyCode, metaKey, ctrlKey, altKey, shiftKey]] =
            (typeof action == "string") ?
                function() {
                    var toFire = bespin.events.toFire(action);
                    bespin.publish(toFire.name, toFire.args);
                } : dojo.hitch(this.actions, action);

        if (name) this.keyMapDescriptions[[keyCode, metaKey, ctrlKey, altKey, shiftKey]] = name;
    },

    bindKeyString: function(modifiers, keyCode, action, name) {
        var ctrlKey = (modifiers.toUpperCase().indexOf("CTRL") != -1);
        var altKey = (modifiers.toUpperCase().indexOf("ALT") != -1);
        var metaKey = (modifiers.toUpperCase().indexOf("META") != -1) || (modifiers.toUpperCase().indexOf("APPLE") != -1);
        var shiftKey = (modifiers.toUpperCase().indexOf("SHIFT") != -1);

        // Check for the platform specific key type
        // The magic "CMD" means metaKey for Mac (the APPLE or COMMAND key)
        // and ctrlKey for Windows (CONTROL)
        if (modifiers.toUpperCase().indexOf("CMD") != -1) {
            if (bespin.util.isMac()) {
                metaKey = true;
            } else {
                ctrlKey = true;
            }
        }
        return this.bindKey(keyCode, metaKey, ctrlKey, altKey, shiftKey, action, name);
    },

    bindKeyStringSelectable: function(modifiers, keyCode, action, name) {
        this.bindKeyString(modifiers, keyCode, action, name);
        this.bindKeyString("SHIFT " + modifiers, keyCode, action);
    },

    onkeydown: function(e) {
        // handle keys only if editor has the focus!
        if (!this.editor.focus) return false;

        var args = { event: e,
                     pos: bespin.editor.utils.copyPos(this.editor.cursorManager.getCursorPosition()) };
        this.skipKeypress = false;
        this.returnValue = false;

        var action = this.keyMap[[e.keyCode, e.metaKey, e.ctrlKey, e.altKey, e.shiftKey]];

        var hasAction = false;

        if (dojo.isFunction(action)) {
            hasAction = true;
            action(args);
            this.lastAction = action;
        }

        // If a special key is pressed OR if an action is assigned to a given key (e.g. TAB or BACKSPACE)
        if (e.metaKey || e.ctrlKey || e.altKey) {
            this.skipKeypress = true;
            this.returnValue = true;
        }

        // stop going, but allow special strokes to get to the browser
        if (hasAction || !bespin.util.keys.passThroughToBrowser(e)) dojo.stopEvent(e);
    },

    onkeypress: function(e) {
        // handle keys only if editor has the focus!
        if (!this.editor.focus) return false;

        // This is to get around the Firefox bug that happens the first time of jumping between command line and editor
        // Bug https://bugzilla.mozilla.org/show_bug.cgi?id=478686
        var commandLine = bespin.get('commandLine');
        if (commandLine && e.charCode == 'j'.charCodeAt() && e.ctrlKey) {
            dojo.stopEvent(e);
            return false;
        }
        // -- End of commandLine short cut

        // If key should be skipped, BUT there are some chars like "@|{}[]\" that NEED the ALT- or CTRL-key to be accessable
        // on some platforms and keyboardlayouts (german?). This is not working for "^"
        if ([64 /*@*/, 91/*[*/, 92/*\*/, 93/*]*/, 94/*^*/, 123/*{*/, 124/*|*/, 125/*}*/, 126/*~*/ ].indexOf(e.charCode) != -1) {
            this.skipKeypress = false;
        } else if (this.skipKeypress) {
            if (!bespin.util.keys.passThroughToBrowser(e)) dojo.stopEvent(e);
            return this.returnValue;
        }

        var args = { event: e,
                     pos: bespin.editor.utils.copyPos(this.editor.cursorManager.getCursorPosition()) };
        var actions = this.editor.ui.actions;

        // Only allow ascii through
        if ((e.charCode >= 32) && (e.charCode <= 126) || e.charCode >= 160) {
            args.newchar = String.fromCharCode(e.charCode);
            actions.insertCharacter(args);
        } else { // Allow user to move with the arrow continuously
            var action = this.keyMap[[e.keyCode, e.metaKey, e.ctrlKey, e.altKey, e.shiftKey]];

            if (this.lastAction == action) {
                delete this.lastAction;
            } else if (typeof action == "function") {
               action(args);
            }
        }

        dojo.stopEvent(e);
    }
});

// ** {{{ bespin.editor.UI }}} **
//
// Holds the UI. The editor itself, the syntax highlighter, the actions, and more
dojo.declare("bespin.editor.UI", null, {
    constructor: function(editor) {
        this.editor = editor;
        this.model = this.editor.model;

        var settings = bespin.get("settings");
        this.syntaxModel = bespin.syntax.Resolver.setEngine("simple").getModel();

        this.selectionHelper = new bespin.editor.SelectionHelper(editor);
        this.actions = new bespin.editor.Actions(editor);

        this.rowLengthCache = [];
        this.searchString = null;

        this.toggleCursorFullRepaintCounter = 0; // tracks how many cursor toggles since the last full repaint
        this.toggleCursorFrequency = 250;        // number of milliseconds between cursor blink
        this.toggleCursorAllowed = true;         // is the cursor allowed to toggle? (used by cursorManager.moveCursor)

        // these two canvases are used as buffers for the scrollbar images, which are then composited onto the
        // main code view. we could have saved ourselves some misery by just prerendering slices of the scrollbars and
        // combining them like sane people, but... meh
        this.horizontalScrollCanvas = dojo.create("canvas");
        this.verticalScrollCanvas   = dojo.create("canvas");

        // gutterWidth used to be a constant, but is now dynamically calculated in each paint() invocation. I set it to a silly
        // default value here in case some of the code expects it to be populated before the first paint loop kicks in. the
        // default value ought to eventually become 0
        this.gutterWidth = 54;

        this.LINE_HEIGHT = 23;
        this.GUTTER_INSETS = { top: 0, left: 6, right: 10, bottom: 6 };
        this.LINE_INSETS = { top: 0, left: 5, right: 0, bottom: 6 };
        this.FALLBACK_CHARACTER_WIDTH = 10;
        this.NIB_WIDTH = 15;
        this.NIB_INSETS = { top: Math.floor(this.NIB_WIDTH / 2),
                            left: Math.floor(this.NIB_WIDTH / 2),
                            right: Math.floor(this.NIB_WIDTH / 2),
                            bottom: Math.floor(this.NIB_WIDTH / 2) };
        this.NIB_ARROW_INSETS = { top: 3, left: 3, right: 3, bottom: 5 };

        this.DEBUG_GUTTER_WIDTH = 18;
        this.DEBUG_GUTTER_INSETS = { top: 2, left: 2, right: 2, bottom: 2 };

        //this.lineHeight;        // reserved for when line height is calculated dynamically instead of with a constant; set first time a paint occurs
        //this.charWidth;         // set first time a paint occurs
        //this.visibleRows;       // the number of rows visible in the editor; set each time a paint occurs
        //this.firstVisibleRow;   // first row that is visible in the editor; set each time a paint occurs

        //this.nibup;             // rect
        //this.nibdown;           // rect
        //this.nibleft;           // rect
        //this.nibright;          // rect

        //this.selectMouseDownPos;        // position when the user moused down

        this.xoffset = 0;       // number of pixels to translate the canvas for scrolling
        this.yoffset = 0;

        this.showCursor = true;

        this.overXScrollBar = false;
        this.overYScrollBar = false;
        this.hasFocus = false;

        var source = this.editor.container;
        dojo.connect(source, "mousemove", this, "handleScrollBars");
        dojo.connect(source, "mouseout", this, "handleScrollBars");
        dojo.connect(source, "click", this, "handleScrollBars");
        dojo.connect(source, "mousedown", this, "handleScrollBars");

        dojo.connect(source, "mousedown", this, "mouseDownSelect");
        dojo.connect(source, "mousemove", this, "mouseMoveSelect");
        dojo.connect(source, "mouseup", this, "mouseUpSelect");

        // painting optimization state
        this.lastLineCount = 0;
        this.lastCursorPos = null;
        this.lastxoffset = 0;
        this.lastyoffset = 0;

        this.xscrollbar = new bespin.editor.Scrollbar(this, "horizontal");
        this.xscrollbar.valueChanged = dojo.hitch(this, function() {
            this.xoffset = -this.xscrollbar.value;
            this.editor.paint();
        });
        dojo.connect(window, "mousemove", this.xscrollbar, "onmousemove");
        dojo.connect(window, "mouseup", this.xscrollbar, "onmouseup");
        dojo.connect(window, (!dojo.isMozilla ? "onmousewheel" : "DOMMouseScroll"), this.xscrollbar, "onmousewheel");

        this.yscrollbar = new bespin.editor.Scrollbar(this, "vertical");
        this.yscrollbar.valueChanged = dojo.hitch(this, function() {
            this.yoffset = -this.yscrollbar.value;
            this.editor.paint();
        });

        var scope = editor.opts.actsAsComponent ? editor.canvas : window;
        dojo.connect(scope, "mousemove", this.yscrollbar, "onmousemove");
        dojo.connect(scope, "mouseup", this.yscrollbar, "onmouseup");
        dojo.connect(scope, (!dojo.isMozilla ? "onmousewheel" : "DOMMouseScroll"), this.yscrollbar, "onmousewheel");

        setTimeout(dojo.hitch(this, function() { this.toggleCursor(this); }), this.toggleCursorFrequency);
    },

    // col is -1 if user clicked in gutter; clicking below last line maps to last line
    convertClientPointToCursorPoint: function(pos) {
        var settings = bespin.get("settings");
        var x, y;

        if (pos.y > (this.lineHeight * this.editor.model.getRowCount())) {
            y = this.editor.model.getRowCount() - 1;
        } else {
            var ty = pos.y;
            y = Math.floor(ty / this.lineHeight);
        }

        if (pos.x <= (this.gutterWidth + this.LINE_INSETS.left)) {
            x = -1;
        } else {
            var tx = pos.x - this.gutterWidth - this.LINE_INSETS.left;
            x = Math.floor(tx / this.charWidth);

            // With strictlines turned on, don't select past the end of the line
            if ((settings && settings.isSettingOn('strictlines'))) {
                var maxcol = this.getRowScreenLength(y);

                if (x >= maxcol) {
                    x = this.getRowScreenLength(y);
                }
            }
        }
        return { row: y, col: x };
    },

    mouseDownSelect: function(e) {
        // only select if the editor has the focus!
        if (!this.editor.focus) return;
        
        var clientY = e.clientY - this.getTopOffset();
        var clientX = e.clientX - this.getLeftOffset();

        if (this.overXScrollBar || this.overYScrollBar) return;

        if (this.editor.debugMode) {
            if (clientX < this.DEBUG_GUTTER_WIDTH) {
                var point = { x: clientX, y: clientY };
                point.y += Math.abs(this.yoffset);
                var p = this.convertClientPointToCursorPoint(point);

                var editSession = bespin.get("editSession");
                if (p && editSession) {
                    bespin.debug.toggleBreakpoint({ project: editSession.project, path: editSession.path, lineNumber: p.row });
                    this.editor.paint(true);
                    return;
                }
            }
        }

        if (e.shiftKey) {
            this.selectMouseDownPos = (this.editor.selection) ? this.editor.selection.startPos : this.editor.getCursorPos();
            this.setSelection(e);
        } else {
            var point = { x: clientX, y: clientY };
            point.x += Math.abs(this.xoffset);
            point.y += Math.abs(this.yoffset);

            if ((this.xscrollbar.rect.contains(point)) || (this.yscrollbar.rect.contains(point))) return;
            this.selectMouseDownPos = this.convertClientPointToCursorPoint(point);
        }
    },

    mouseMoveSelect: function(e) {
        // only select if the editor has the focus!
        if (!this.editor.focus) return;
        
        this.setSelection(e);
    },

    mouseUpSelect: function(e) {
        // only select if the editor has the focus!
        if (!this.editor.focus) return;
        
        this.setSelection(e);
        this.selectMouseDownPos = undefined;
    },

    setSelection: function(e) {
        var clientY = e.clientY - this.getTopOffset();
        var clientX = e.clientX - this.getLeftOffset();

        if (!this.selectMouseDownPos) return;

        var down = bespin.editor.utils.copyPos(this.selectMouseDownPos);

        var point = { x: clientX, y: clientY };
        point.x += Math.abs(this.xoffset);
        point.y += Math.abs(this.yoffset);
        var up = this.convertClientPointToCursorPoint(point);

        if (down.col == -1) {
            down.col = 0;
            // clicked in gutter; show appropriate lineMarker message
            var lineMarkers = bespin.get("parser").getLineMarkers();
            for (var i = 0; i < lineMarkers.length; i++) {
                if (lineMarkers[i].row === down.row + 1) {
                    bespin.publish("message", {
                        msg: 'Syntax error: ' + lineMarkers[i].message + ' on line ' + lineMarkers[i].row,
                        tag: 'autohide'
                    });
                }
            }
        }
        if (up.col == -1) up.col = 0;

        if (!bespin.editor.utils.posEquals(down, up)) {
            this.editor.setSelection({ startPos: down, endPos: up });
        } else {
            if (e.detail == 1) {
                this.editor.setSelection(undefined);
            } else if (e.detail == 2) {
                var row = this.editor.model.rows[down.row];
                var cursorAt = row[down.col];
                if (!cursorAt || cursorAt.charAt(0) == ' ') { // empty space
                    // For now, don't select anything, but think about copying Textmate and grabbing around it
                } else {
                    var startPos = (up = this.editor.model.findBefore(down.row, down.col));

                    var endPos = this.editor.model.findAfter(down.row, down.col);

                    this.editor.setSelection({ startPos: startPos, endPos: endPos });
                }
            } else if (e.detail > 2) {
                // select the line
                this.editor.setSelection({ startPos: { row: down.row, col: 0 }, endPos: { row: down.row + 1, col: 0 } });
            }
        }

        this.editor.cursorManager.moveCursor(up);
        this.editor.paint();
    },

    toggleCursor: function(ui) {
        if (ui.toggleCursorAllowed) {
            ui.showCursor = !ui.showCursor;
        } else {
            ui.toggleCursorAllowed = true;
        }

        if (++this.toggleCursorFullRepaintCounter > 0) {
            this.toggleCursorFullRepaintCounter = 0;
            ui.editor.paint(true);
        } else {
            ui.editor.paint();
        }

        setTimeout(function() { ui.toggleCursor(ui); }, ui.toggleCursorFrequency);
    },

    ensureCursorVisible: function() {
        if ((!this.lineHeight) || (!this.charWidth)) return;    // can't do much without these

        var y = this.lineHeight * this.editor.cursorManager.getCursorPosition().row;
        var x = this.charWidth * this.editor.cursorManager.getCursorPosition().col;

        var cheight = this.getHeight();
        var cwidth = this.getWidth() - this.gutterWidth;

        if (Math.abs(this.yoffset) > y) {               // current row before top-most visible row
            this.yoffset = Math.min(-y + cheight * 0.25, 0);
        } else if ((Math.abs(this.yoffset) + cheight) < (y + this.lineHeight)) {       // current row after bottom-most visible row
            this.yoffset = -((y + this.lineHeight) - cheight * 0.75);
        }

        if (Math.abs(this.xoffset) > x) {               // current col before left-most visible col
            this.xoffset = -x;
        } else if ((Math.abs(this.xoffset) + cwidth) < (x + (this.charWidth * 2))) { // current col after right-most visible col
            this.xoffset = -((x + (this.charWidth * 2)) - cwidth);
        }
    },

    handleFocus: function(e) {
        this.editor.model.clear();
        this.editor.model.insertCharacters({ row: 0, col: 0 }, e.type);
    },

    handleScrollBars: function(e) {
        var clientY = e.clientY - this.getTopOffset();
        var clientX = e.clientX - this.getLeftOffset();

        var oldX = this.overXScrollBar;
        var oldY = this.overYScrollBar;
        var scrolled = false;

        var w = this.editor.container.clientWidth;
        var h = this.editor.container.clientHeight;
        var sx = w - this.NIB_WIDTH - this.NIB_INSETS.right;    // x start of the vert. scroll bar
        var sy = h - this.NIB_WIDTH - this.NIB_INSETS.bottom;   // y start of the hor. scroll bar

        var p = { x: clientX, y:clientY };

        if (e.type == "mousedown") {
            // dispatch to the scrollbars
            if ((this.xscrollbar) && (this.xscrollbar.rect.contains(p))) {
                this.xscrollbar.onmousedown(e);
            } else if ((this.yscrollbar) && (this.yscrollbar.rect.contains(p))) {
                this.yscrollbar.onmousedown(e);
            }
        }

        if (e.type == "mouseout") {
            this.overXScrollBar = false;
            this.overYScrollBar = false;
        }

        if ((e.type == "mousemove") || (e.type == "click")) {
            this.overYScrollBar = p.x > sx;
            this.overXScrollBar = p.y > sy;
        }

        if (e.type == "click") {
            if ((typeof e.button != "undefined") && (e.button == 0)) {
                var button;
                if (this.nibup.contains(p)) {
                    button = "up";
                } else if (this.nibdown.contains(p)) {
                    button = "down";
                } else if (this.nibleft.contains(p)) {
                    button = "left";
                } else if (this.nibright.contains(p)) {
                    button = "right";
                }

                if (button == "up") {
                    this.yoffset += this.lineHeight;
                    scrolled = true;
                } else if (button == "down") {
                    this.yoffset -= this.lineHeight;
                    scrolled = true;
                } else if (button == "left") {
                    this.xoffset += this.charWidth * 2;
                    scrolled = true;
                } else if (button == "right") {
                    this.xoffset -= this.charWidth * 2;
                    scrolled = true;
                }
            }
        }

        if ((oldX != this.overXScrollBar) || (oldY != this.overYScrollBar) || scrolled) this.editor.paint();
    },

    installKeyListener: function(listener) {
        var Key = bespin.util.keys.Key; // alias

        if (this.oldkeydown) dojo.disconnect(this.oldkeydown);
        if (this.oldkeypress) dojo.disconnect(this.oldkeypress);

        this.oldkeydown  = dojo.hitch(listener, "onkeydown");
        this.oldkeypress = dojo.hitch(listener, "onkeypress");

        var scope = this.editor.opts.actsAsComponent ? this.editor.canvas : document;

        dojo.connect(scope, "keydown", this, "oldkeydown");
        dojo.connect(scope, "keypress", this, "oldkeypress");

        // Modifiers, Key, Action

        listener.bindKeyStringSelectable("", Key.LEFT_ARROW, this.actions.moveCursorLeft, "Move Cursor Left");
        listener.bindKeyStringSelectable("", Key.RIGHT_ARROW, this.actions.moveCursorRight, "Move Cursor Right");
        listener.bindKeyStringSelectable("", Key.UP_ARROW, this.actions.moveCursorUp, "Move Cursor Up");
        listener.bindKeyStringSelectable("", Key.DOWN_ARROW, this.actions.moveCursorDown, "Move Cursor Down");

        listener.bindKeyStringSelectable("ALT", Key.LEFT_ARROW, this.actions.moveWordLeft, "Move Word Left");
        listener.bindKeyStringSelectable("ALT", Key.RIGHT_ARROW, this.actions.moveWordRight, "Move Word Right");

        listener.bindKeyStringSelectable("", Key.HOME, this.actions.moveToLineStart, "Move to start of line");
        listener.bindKeyStringSelectable("CMD", Key.LEFT_ARROW, this.actions.moveToLineStart, "Move to start of line");
        listener.bindKeyStringSelectable("", Key.END, this.actions.moveToLineEnd, "Move to end of line");
        listener.bindKeyStringSelectable("CMD", Key.RIGHT_ARROW, this.actions.moveToLineEnd, "Move to end of line");

        listener.bindKeyString("CTRL", Key.K, this.actions.killLine, "Kill entire line");
        listener.bindKeyString("CTRL", Key.L, this.actions.moveCursorRowToCenter, "Move cursor to center of page");

        listener.bindKeyString("", Key.BACKSPACE, this.actions.backspace, "Backspace");
        listener.bindKeyString("CTRL", Key.BACKSPACE, this.actions.deleteWordLeft, "Delete a word to the left");

        listener.bindKeyString("", Key.DELETE, this.actions.deleteKey, "Delete");
        listener.bindKeyString("CTRL", Key.DELETE, this.actions.deleteWordRight, "Delete a word to the right");

        listener.bindKeyString("", Key.ENTER, this.actions.newline, "Insert newline");
        listener.bindKeyString("", Key.TAB, this.actions.insertTab, "Indent / insert tab");
        listener.bindKeyString("SHIFT", Key.TAB, this.actions.unindent, "Unindent");

        // SEARCH / FIND
        listener.bindKeyString("", Key.ESCAPE, this.actions.escape, "Clear fields and dialogs");
        // This is at the moment done by a observe(window) within init.js
        // listener.bindKeyString("CMD", Key.F, this.actions.findSelectInputField, "Show find dialog");
        // listener.bindKeyString("SHIFT CMD", Key.G, this.actions.findPrev, "Find the previous match");
        // listener.bindKeyString("CMD", Key.G, this.actions.findNext, "Go on to the next match");

        listener.bindKeyString("CMD", Key.A, this.actions.selectAll, "Select All");
        
        // handle key to jump between editor and other windows / commandline
        listener.bindKeyString("ALT", Key.O, this.actions.toggleQuickopen, "Toggle Quickopen");
        listener.bindKeyString("CTRL", Key.J, this.actions.focusCommandline, "Focus Commandline");

        listener.bindKeyString("CMD", Key.Z, this.actions.undo, "Undo");
        listener.bindKeyString("SHIFT CMD", Key.Z, this.actions.redo, "Redo");
        listener.bindKeyString("CMD", Key.Y, this.actions.redo, "Redo");

        listener.bindKeyStringSelectable("CMD", Key.UP_ARROW, this.actions.moveToFileTop, "Move to top of file");
        listener.bindKeyStringSelectable("CMD", Key.DOWN_ARROW, this.actions.moveToFileBottom, "Move to bottom of file");
        listener.bindKeyStringSelectable("CMD", Key.HOME, this.actions.moveToFileTop, "Move to top of file");
        listener.bindKeyStringSelectable("CMD", Key.END, this.actions.moveToFileBottom, "Move to bottom of file");

        listener.bindKeyStringSelectable("", Key.PAGE_UP, this.actions.movePageUp, "Move a page up");
        listener.bindKeyStringSelectable("", Key.PAGE_DOWN, this.actions.movePageDown, "Move a page down");

        // Other key bindings can be found in commands themselves.
        // For example, this:
        // listener.bindKeyString("CTRL SHIFT", Key.N, "editor:newfile", "Create a new file");
        // has been moved to the 'newfile' command withKey
        // Also, the clipboard.js handles C, V, and X
    },

    getWidth: function() {
        return parseInt(dojo.style(this.editor.canvas.parentNode, "width"));
    },

    getHeight: function() {
        return parseInt(dojo.style(this.editor.canvas.parentNode, "height"));
    },

    getTopOffset: function() {
        return dojo.coords(this.editor.canvas.parentNode).y || this.editor.canvas.parentNode.offsetTop;
    },

    getLeftOffset: function() {
        return dojo.coords(this.editor.canvas.parentNode).x || this.editor.canvas.parentNode.offsetLeft;
    },

    getCharWidth: function(ctx) {
        if (ctx.measureText) {
            return ctx.measureText("M").width;
        } else {
            return this.FALLBACK_CHARACTER_WIDTH;
        }
    },

    getLineHeight: function(ctx) {
        var lh = -1;
        if (ctx.measureText) {
            var t = ctx.measureText("M");
            if (t.ascent) lh = Math.floor(t.ascent * 2.8);
        }
        if (lh == -1) lh = this.LINE_HEIGHT;
        return lh;
    },

    // ** {{{ paint }}} **
    //
    // This is where the editor is painted from head to toe. The optional "fullRefresh" argument triggers a complete repaint
    // of the editor canvas; otherwise, pitiful tricks are used to draw as little as possible.

    paint: function(ctx, fullRefresh) {
        // DECLARE VARIABLES

        // these are convenience references so we don't have to type so much
        var c = dojo.byId(this.editor.canvas);
        var theme = this.editor.theme;
        var ed = this.editor;

        // these are commonly used throughout the rendering process so are defined up here to make it clear they are shared
        var x, y;
        var cy;
        var currentLine;
        var lastLineToRender;

        var Rect = bespin.editor.Rect;

        // SETUP STATE

        var refreshCanvas = fullRefresh;        // if the user explicitly requests a full refresh, give it to 'em

        if (!refreshCanvas) refreshCanvas = (this.selectMouseDownPos);

        if (!refreshCanvas) refreshCanvas = (this.lastLineCount != ed.model.getRowCount());  // if the line count has changed, full refresh

        this.lastLineCount = ed.model.getRowCount();        // save the number of lines for the next time paint

        // get the line and character metrics; calculated for each paint because this value can change at run-time
        ctx.font = theme.editorTextFont;
        this.charWidth = this.getCharWidth(ctx);
        this.lineHeight = this.getLineHeight(ctx);

        // cwidth and cheight are set to the dimensions of the parent node of the canvas element; we'll resize the canvas element
        // itself a little bit later in this function
        var cwidth = this.getWidth();
        var cheight = this.getHeight();

        // adjust the scrolling offsets if necessary; negative values are good, indicate scrolling down or to the right (we look for overflows on these later on)
        // positive values are bad; they indicate scrolling up past the first line or to the left past the first column
        if (this.xoffset > 0) this.xoffset = 0;
        if (this.yoffset > 0) this.yoffset = 0;

        // only paint those lines that can be visible
        this.visibleRows = Math.ceil(cheight / this.lineHeight);
        this.firstVisibleRow = Math.floor(Math.abs(this.yoffset / this.lineHeight));
        lastLineToRender = this.firstVisibleRow + this.visibleRows;
        if (lastLineToRender > (ed.model.getRowCount() - 1)) lastLineToRender = ed.model.getRowCount() - 1;

        var virtualheight = this.lineHeight * ed.model.getRowCount();    // full height based on content

        // virtual width *should* be based on every line in the model; however, with the introduction of tab support, calculating
        // the width of a line is now expensive, so for the moment we will only calculate the width of the visible rows
        //var virtualwidth = this.charWidth * (Math.max(this.getMaxCols(), ed.cursorManager.getCursorPosition().col) + 2);       // full width based on content plus a little padding
        var virtualwidth = this.charWidth * (Math.max(this.getMaxCols(this.firstVisibleRow, lastLineToRender), ed.cursorManager.getCursorPosition().col) + 2);

        // calculate the gutter width; for now, we'll make it fun and dynamic based on the lines visible in the editor.
        this.gutterWidth = this.GUTTER_INSETS.left + this.GUTTER_INSETS.right;  // first, add the padding space
        this.gutterWidth += ("" + lastLineToRender).length * this.charWidth;    // make it wide enough to display biggest line number visible
        if (this.editor.debugMode) this.gutterWidth += this.DEBUG_GUTTER_WIDTH;

        // these next two blocks make sure we don't scroll too far in either the x or y axis
        if (this.xoffset < 0) {
            if ((Math.abs(this.xoffset)) > (virtualwidth - (cwidth - this.gutterWidth))) this.xoffset = (cwidth - this.gutterWidth) - virtualwidth;
        }
        if (this.yoffset < 0) {
            if ((Math.abs(this.yoffset)) > (virtualheight - cheight)) this.yoffset = cheight - virtualheight;
        }

        // if the current scrolled positions are different than the scroll positions we used for the last paint, refresh the entire canvas
        if ((this.xoffset != this.lastxoffset) || (this.yoffset != this.lastyoffset)) {
            refreshCanvas = true;
            this.lastxoffset = this.xoffset;
            this.lastyoffset = this.yoffset;
        }

        // these are boolean values indicating whether the x and y (i.e., horizontal or vertical) scroll bars are visible
        var xscroll = ((cwidth - this.gutterWidth) < virtualwidth);
        var yscroll = (cheight < virtualheight);

        // the scroll bars are rendered off-screen into their own canvas instances; these values are used in two ways as part of
        // this process:
        //   1. the x position of the vertical scroll bar image when painted onto the canvas and the y position of the horizontal
        //      scroll bar image (both images span 100% of the width/height in the other dimension)
        //   2. the amount * -1 to translate the off-screen canvases used by the scrollbars; this lets us flip back to rendering
        //      the scroll bars directly on the canvas with relative ease (by omitted the translations and passing in the main context
        //      reference instead of the off-screen canvas context)
        var verticalx = cwidth - this.NIB_WIDTH - this.NIB_INSETS.right - 2;
        var horizontaly = cheight - this.NIB_WIDTH - this.NIB_INSETS.bottom - 2;

        // these are boolean values that indicate whether special little "nibs" should be displayed indicating more content to the
        // left, right, top, or bottom
        var showLeftScrollNib = (xscroll && (this.xoffset != 0));
        var showRightScrollNib = (xscroll && (this.xoffset > ((cwidth - this.gutterWidth) - virtualwidth)));
        var showUpScrollNib = (yscroll && (this.yoffset != 0));
        var showDownScrollNib = (yscroll && (this.yoffset > (cheight - virtualheight)));

        // check and see if the canvas is the same size as its immediate parent in the DOM; if not, resize the canvas
        if (((dojo.attr(c, "width")) != cwidth) || (dojo.attr(c, "height") != cheight)) {
            refreshCanvas = true;   // if the canvas changes size, we'll need a full repaint
            dojo.attr(c, { width: cwidth, height: cheight });
        }

        // get debug metadata
        var breakpoints = {};

        if (this.editor.debugMode && bespin.get("editSession")) {
            var points = bespin.debug.getBreakpoints(bespin.get('editSession').project, bespin.get('editSession').path);
            dojo.forEach(points, function(point) {
                breakpoints[point.lineNumber] = point;
            });
            delete points;
        }

        // IF YOU WANT TO FORCE A COMPLETE REPAINT OF THE CANVAS ON EVERY PAINT, UNCOMMENT THE FOLLOWING LINE:
        //refreshCanvas = true;

        // START RENDERING

        // if we're not doing a full repaint, work out which rows are "dirty" and need to be repainted
        if (!refreshCanvas) {
            var dirty = ed.model.getDirtyRows();

            // if the cursor has changed rows since the last paint, consider the previous row dirty
            if ((this.lastCursorPos) && (this.lastCursorPos.row != ed.cursorManager.getCursorPosition().row)) dirty[this.lastCursorPos.row] = true;

            // we always repaint the current line
            dirty[ed.cursorManager.getCursorPosition().row] = true;
        }

        // save this state for the next paint attempt (see above for usage)
        this.lastCursorPos = bespin.editor.utils.copyPos(ed.cursorManager.getCursorPosition());

        // if we're doing a full repaint...
        if (refreshCanvas) {
            // ...paint the background color over the whole canvas and...
            ctx.fillStyle = theme.backgroundStyle;
            ctx.fillRect(0, 0, c.width, c.height);

            // ...paint the gutter
            ctx.fillStyle = theme.gutterStyle;
            ctx.fillRect(0, 0, this.gutterWidth, c.height);
        }

        // translate the canvas based on the scrollbar position; for now, just translate the vertical axis
        ctx.save(); // take snapshot of current context state so we can roll back later on

        // the Math.round(this.yoffset) makes the painting nice and not to go over 2 pixels
        // see for more informations:
        //  - https://developer.mozilla.org/en/Canvas_tutorial/Applying_styles_and_colors, section "Line styles"
        //  - https://developer.mozilla.org/@api/deki/files/601/=Canvas-grid.png
        ctx.translate(0, Math.round(this.yoffset));

        // paint the line numbers
        if (refreshCanvas) {
            //line markers first
            if (bespin.get("parser")) {
                var lineMarkers = bespin.get("parser").getLineMarkers();
                for (var i = 0; i < lineMarkers.length; i++) {
                    if (lineMarkers[i].row >= this.firstVisibleRow && lineMarkers[i].row <= lastLineToRender + 1) {
                        y = this.lineHeight * (lineMarkers[i].row - 1);
                        cy = y + (this.lineHeight - this.LINE_INSETS.bottom);
                        ctx.fillStyle = this.editor.theme.lineMarkerGutterColor;
                        ctx.fillRect(0, y, this.gutterWidth, this.lineHeight);
                    }
                 }
            }
            y = (this.lineHeight * this.firstVisibleRow);
            for (currentLine = this.firstVisibleRow; currentLine <= lastLineToRender; currentLine++) {
                x = 0;

                // if we're in debug mode...
                if (this.editor.debugMode) {
                    // ...check if the current line has a breakpoint
                    if (breakpoints[currentLine]) {
                        var bpx = x + this.DEBUG_GUTTER_INSETS.left;
                        var bpy = y + this.DEBUG_GUTTER_INSETS.top;
                        var bpw = this.DEBUG_GUTTER_WIDTH - this.DEBUG_GUTTER_INSETS.left - this.DEBUG_GUTTER_INSETS.right;
                        var bph = this.lineHeight - this.DEBUG_GUTTER_INSETS.top - this.DEBUG_GUTTER_INSETS.bottom;

                        var bpmidpointx = bpx + parseInt(bpw / 2);
                        var bpmidpointy = bpy + parseInt(bph / 2);

                        ctx.strokeStyle = "rgb(128, 0, 0)";
                        ctx.fillStyle = "rgb(255, 102, 102)";
                        ctx.beginPath();
                        ctx.arc(bpmidpointx, bpmidpointy, bpw / 2, 0, Math.PI*2, true);
                        ctx.closePath();
                        ctx.fill();
                        ctx.stroke();
                    }

                    // ...and push the line number to the right, leaving a space for breakpoint stuff
                    x += this.DEBUG_GUTTER_WIDTH;
                }

                x += this.GUTTER_INSETS.left;

                cy = y + (this.lineHeight - this.LINE_INSETS.bottom);

                ctx.fillStyle = theme.lineNumberColor;
                ctx.font = this.editor.theme.lineNumberFont;
                ctx.fillText(currentLine + 1, x, cy);

                y += this.lineHeight;
            }
         }

        // and now we're ready to translate the horizontal axis; while we're at it, we'll setup a clip to prevent any drawing outside
        // of code editor region itself (protecting the gutter). this clip is important to prevent text from bleeding into the gutter.
        ctx.save();
        ctx.beginPath();
        ctx.rect(this.gutterWidth, -this.yoffset, cwidth - this.gutterWidth, cheight);
        ctx.closePath();
        ctx.translate(this.xoffset, 0);
        ctx.clip();

        // calculate the first and last visible columns on the screen; these values will be used to try and avoid painting text
        // that the user can't actually see
        var firstColumn = Math.floor(Math.abs(this.xoffset / this.charWidth));
        var lastColumn = firstColumn + (Math.ceil((cwidth - this.gutterWidth) / this.charWidth));

        // create the state necessary to render each line of text
        y = (this.lineHeight * this.firstVisibleRow);
        var cc; // the starting column of the current region in the region render loop below
        var ce; // the ending column in the same loop
        var ri; // counter variable used for the same loop
        var regionlen;  // length of the text in the region; used in the same loop
        var tx, tw, tsel;
        var settings = bespin.get("settings");
        var searchStringLength = (this.searchString ? this.searchString.length : -1);

        // paint each line
        for (currentLine = this.firstVisibleRow; currentLine <= lastLineToRender; currentLine++) {
            x = this.gutterWidth;

            // if we aren't repainting the entire canvas...
            if (!refreshCanvas) {
                // ...don't bother painting the line unless it is "dirty" (see above for dirty checking)
                if (!dirty[currentLine]) {
                    y += this.lineHeight;
                    continue;
                }

                // setup a clip for the current line only; this makes drawing just that piece of the scrollbar easy
                ctx.save(); // this is restore()'d in another if (!refreshCanvas) block at the end of the loop
                ctx.beginPath();
                ctx.rect(x + (Math.abs(this.xoffset)), y, cwidth, this.lineHeight);
                ctx.closePath();
                ctx.clip();

                if ((currentLine % 2) == 1) { // only repaint the line background if the zebra stripe won't be painted into it
                    ctx.fillStyle = theme.backgroundStyle;
                    ctx.fillRect(x + (Math.abs(this.xoffset)), y, cwidth, this.lineHeight);
                }
            }

            // if highlight line is on, paint the highlight color
            if ((settings && settings.isSettingOn('highlightline')) &&
                    (currentLine == ed.cursorManager.getCursorPosition().row)) {
                ctx.fillStyle = theme.highlightCurrentLineColor;
                ctx.fillRect(x + (Math.abs(this.xoffset)), y, cwidth, this.lineHeight);
            // if not on highlight, see if we need to paint the zebra
            } else if ((currentLine % 2) == 0) {
                ctx.fillStyle = theme.zebraStripeColor;
                ctx.fillRect(x + (Math.abs(this.xoffset)), y, cwidth, this.lineHeight);
            }

            x += this.LINE_INSETS.left;
            cy = y + (this.lineHeight - this.LINE_INSETS.bottom);

            // paint the selection bar if the line has selections
            var selections = this.selectionHelper.getRowSelectionPositions(currentLine);
            if (selections) {
                tx = x + (selections.startCol * this.charWidth);
                tw = (selections.endCol == -1) ? (lastColumn - firstColumn) * this.charWidth : (selections.endCol - selections.startCol) * this.charWidth;
                ctx.fillStyle = theme.editorSelectedTextBackground;
                ctx.fillRect(tx, y, tw, this.lineHeight);
            }

            var lineMetadata = this.model.getRowMetadata(currentLine);
            var lineText = lineMetadata.lineText;
            var searchIndices = lineMetadata.searchIndices;

            // the following two chunks of code do the same thing; only one should be uncommented at a time

            // CHUNK 1: this code just renders the line with white text and is for testing
//            ctx.fillStyle = "white";
//            ctx.fillText(this.editor.model.getRowArray(currentLine).join(""), x, cy);

            // CHUNK 2: this code uses the SyntaxModel API to render the line
            // syntax highlighting
            var lineInfo = this.syntaxModel.getSyntaxStylesPerLine(lineText, currentLine, this.editor.language);

            for (ri = 0; ri < lineInfo.regions.length; ri++) {
                var styleInfo = lineInfo.regions[ri];

                for (var style in styleInfo) {
                    if (!styleInfo.hasOwnProperty(style)) continue;

                    var thisLine = "";

                    var styleArray = styleInfo[style];
                    var currentColumn = 0; // current column, inclusive
                    for (var si = 0; si < styleArray.length; si++) {
                        var range = styleArray[si];

                        for ( ; currentColumn < range.start; currentColumn++) thisLine += " ";
                        thisLine += lineInfo.text.substring(range.start, range.stop);
                        currentColumn = range.stop;
                    }

                    ctx.fillStyle = this.editor.theme[style] || "white";
                    ctx.font = this.editor.theme.editorTextFont;
                    ctx.fillText(thisLine, x, cy);
                }
            }

            // highlight search string
            if (searchIndices) {
                // in some cases the selections are -1 => set them to a more "realistic" number
                if (selections) {
                    tsel = { startCol: 0, endCol: lineText.length };
                    if (selections.startCol != -1) tsel.startCol = selections.startCol;
                    if (selections.endCol   != -1) tsel.endCol = selections.endCol;
                } else {
                    tsel = false;
                }

                for (var i = 0; i < searchIndices.length; i++) {
                    var index = ed.cursorManager.getCursorPosition({col: searchIndices[i], row: currentLine}).col;
                    tx = x + index * this.charWidth;

                    // highlight the area
                    ctx.fillStyle = this.editor.theme.searchHighlight;
                    ctx.fillRect(tx, y, searchStringLength * this.charWidth, this.lineHeight);

                    // figure out, whether the selection is in this area. If so, colour it different
                    if (tsel) {
                        var indexStart = index;
                        var indexEnd = index + searchStringLength;

                        if (tsel.startCol < indexEnd && tsel.endCol > indexStart) {
                            indexStart = Math.max(indexStart, tsel.startCol);
                            indexEnd = Math.min(indexEnd, tsel.endCol);

                            ctx.fillStyle = this.editor.theme.searchHighlightSelected;
                            ctx.fillRect(x + indexStart * this.charWidth, y, (indexEnd - indexStart) * this.charWidth, this.lineHeight);
                        }
                    }

                    // print the overpainted text again
                    ctx.fillStyle = this.editor.theme.editorTextColor || "white";
                    ctx.fillText(lineText.substring(index, index + searchStringLength), tx, cy);
                }

            }

            // paint tab information, if applicable and the information should be displayed
            if (settings && (settings.isSettingOn("tabarrow") || settings.isSettingOn("tabshowspace"))) {
                if (lineMetadata.tabExpansions.length > 0) {
                    for (var i = 0; i < lineMetadata.tabExpansions.length; i++) {
                        var expansion = lineMetadata.tabExpansions[i];

                        // the starting x position of the tab character; the existing value of y is fine
                        var lx = x + (expansion.start * this.charWidth);

                        // check if the user wants us to highlight tabs; useful if you need to mix tabs and spaces
                        var showTabSpace = settings && settings.isSettingOn("tabshowspace");
                        if (showTabSpace) {
                            var sw = (expansion.end - expansion.start) * this.charWidth;
                            ctx.fillStyle = this.editor.theme["tabSpace"] || "white";
                            ctx.fillRect(lx, y, sw, this.lineHeight);
                        }

                        var showTabNib = settings && settings.isSettingOn("tabarrow");
                        if (showTabNib) {
                            // the center of the current character position's bounding rectangle
                            var cy = y + (this.lineHeight / 2);
                            var cx = lx + (this.charWidth / 2);

                            // the width and height of the triangle to draw representing the tab
                            var tw = 4;
                            var th = 6;

                            // the origin of the triangle
                            var tx = parseInt(cx - (tw / 2));
                            var ty = parseInt(cy - (th / 2));

                            // draw the rectangle
                            ctx.beginPath();
                            ctx.fillStyle = this.editor.theme["plain"] || "white";
                            ctx.moveTo(tx, ty);
                            ctx.lineTo(tx, ty + th);
                            ctx.lineTo(tx + tw, ty + parseInt(th / 2));
                            ctx.closePath();
                            ctx.fill();
                        }
                    }
                }
            }


            if (!refreshCanvas) {
                ctx.drawImage(this.verticalScrollCanvas, verticalx + Math.abs(this.xoffset), Math.abs(this.yoffset));
                ctx.restore();
            }

            y += this.lineHeight;
        }


        // paint the cursor
        if (this.editor.focus) {
            if (this.showCursor) {
                if (ed.theme.cursorType == "underline") {
                    x = this.gutterWidth + this.LINE_INSETS.left + ed.cursorManager.getCursorPosition().col * this.charWidth;
                    y = (ed.getCursorPos().row * this.lineHeight) + (this.lineHeight - 5);
                    ctx.fillStyle = ed.theme.cursorStyle;
                    ctx.fillRect(x, y, this.charWidth, 3);
                } else {
                    x = this.gutterWidth + this.LINE_INSETS.left + ed.cursorManager.getCursorPosition().col * this.charWidth;
                    y = (ed.cursorManager.getCursorPosition().row * this.lineHeight);
                    ctx.fillStyle = ed.theme.cursorStyle;
                    ctx.fillRect(x, y, 1, this.lineHeight);
                }
            }
        } else {
            x = this.gutterWidth + this.LINE_INSETS.left + ed.cursorManager.getCursorPosition().col * this.charWidth;
            y = (ed.cursorManager.getCursorPosition().row * this.lineHeight);

            ctx.fillStyle = ed.theme.unfocusedCursorFillStyle;
            ctx.strokeStyle = ed.theme.unfocusedCursorStrokeStyle;
            ctx.fillRect(x, y, this.charWidth, this.lineHeight);
            ctx.strokeRect(x, y, this.charWidth, this.lineHeight);
        }

        // scroll bars - x axis
        ctx.restore();

        // scrollbars - y axis
        ctx.restore();

        // paint scroll bars unless we don't need to :-)
        if (!refreshCanvas) return;

        // temporary disable of scrollbars
        //if (this.xscrollbar.rect) return;

        if (this.horizontalScrollCanvas.width != cwidth) this.horizontalScrollCanvas.width = cwidth;
        if (this.horizontalScrollCanvas.height != this.NIB_WIDTH + 4) this.horizontalScrollCanvas.height = this.NIB_WIDTH + 4;

        if (this.verticalScrollCanvas.height != cheight) this.verticalScrollCanvas.height = cheight;
        if (this.verticalScrollCanvas.width != this.NIB_WIDTH + 4) this.verticalScrollCanvas.width = this.NIB_WIDTH + 4;

        var hctx = this.horizontalScrollCanvas.getContext("2d");
        hctx.clearRect(0, 0, this.horizontalScrollCanvas.width, this.horizontalScrollCanvas.height);
        hctx.save();

        var vctx = this.verticalScrollCanvas.getContext("2d");
        vctx.clearRect(0, 0, this.verticalScrollCanvas.width, this.verticalScrollCanvas.height);
        vctx.save();

        var ythemes = (this.overYScrollBar) || (this.yscrollbar.mousedownValue != null) ?
                      { n: ed.theme.fullNibStyle, a: ed.theme.fullNibArrowStyle, s: ed.theme.fullNibStrokeStyle } :
                      { n: ed.theme.partialNibStyle, a: ed.theme.partialNibArrowStyle, s: ed.theme.partialNibStrokeStyle };
        var xthemes = (this.overXScrollBar) || (this.xscrollbar.mousedownValue != null) ?
                      { n: ed.theme.fullNibStyle, a: ed.theme.fullNibArrowStyle, s: ed.theme.fullNibStrokeStyle } :
                      { n: ed.theme.partialNibStyle, a: ed.theme.partialNibArrowStyle, s: ed.theme.partialNibStrokeStyle };

        var midpoint = Math.floor(this.NIB_WIDTH / 2);

        this.nibup = new Rect(cwidth - this.NIB_INSETS.right - this.NIB_WIDTH,
                this.NIB_INSETS.top, this.NIB_WIDTH, this.NIB_WIDTH);

        this.nibdown = new Rect(cwidth - this.NIB_INSETS.right - this.NIB_WIDTH,
                cheight - (this.NIB_WIDTH * 2) - (this.NIB_INSETS.bottom * 2),
                this.NIB_INSETS.top,
                this.NIB_WIDTH, this.NIB_WIDTH);

        this.nibleft = new Rect(this.gutterWidth + this.NIB_INSETS.left, cheight - this.NIB_INSETS.bottom - this.NIB_WIDTH,
                this.NIB_WIDTH, this.NIB_WIDTH);

        this.nibright = new Rect(cwidth - (this.NIB_INSETS.right * 2) - (this.NIB_WIDTH * 2),
                cheight - this.NIB_INSETS.bottom - this.NIB_WIDTH,
                this.NIB_WIDTH, this.NIB_WIDTH);

        vctx.translate(-verticalx, 0);
        hctx.translate(0, -horizontaly);

        if (xscroll && ((this.overXScrollBar) || (this.xscrollbar.mousedownValue != null))) {
            hctx.save();

            hctx.beginPath();
            hctx.rect(this.nibleft.x + midpoint + 2, 0, this.nibright.x - this.nibleft.x - 1, cheight); // y points don't matter
            hctx.closePath();
            hctx.clip();

            hctx.fillStyle = ed.theme.scrollTrackFillStyle;
            hctx.fillRect(this.nibleft.x, this.nibleft.y - 1, this.nibright.x2 - this.nibleft.x, this.nibleft.h + 1);

            hctx.strokeStyle = ed.theme.scrollTrackStrokeStyle;
            hctx.strokeRect(this.nibleft.x, this.nibleft.y - 1, this.nibright.x2 - this.nibleft.x, this.nibleft.h + 1);

            hctx.restore();
        }

        if (yscroll && ((this.overYScrollBar) || (this.yscrollbar.mousedownValue != null))) {
            vctx.save();

            vctx.beginPath();
            vctx.rect(0, this.nibup.y + midpoint + 2, cwidth, this.nibdown.y - this.nibup.y - 1); // x points don't matter
            vctx.closePath();
            vctx.clip();

            vctx.fillStyle = ed.theme.scrollTrackFillStyle;
            vctx.fillRect(this.nibup.x - 1, this.nibup.y, this.nibup.w + 1, this.nibdown.y2 - this.nibup.y);

            vctx.strokeStyle = ed.theme.scrollTrackStrokeStyle;
            vctx.strokeRect(this.nibup.x - 1, this.nibup.y, this.nibup.w + 1, this.nibdown.y2 - this.nibup.y);

            vctx.restore();
        }

        if (yscroll) {
            // up arrow
            if ((showUpScrollNib) || (this.overYScrollBar) || (this.yscrollbar.mousedownValue != null)) {
                vctx.save();
                vctx.translate(this.nibup.x + midpoint, this.nibup.y + midpoint);
                this.paintNib(vctx, ythemes.n, ythemes.a, ythemes.s);
                vctx.restore();
            }

            // down arrow
            if ((showDownScrollNib) || (this.overYScrollBar) || (this.yscrollbar.mousedownValue != null)) {
                vctx.save();
                vctx.translate(this.nibdown.x + midpoint, this.nibdown.y + midpoint);
                vctx.rotate(Math.PI);
                this.paintNib(vctx, ythemes.n, ythemes.a, ythemes.s);
                vctx.restore();
            }
        }

        if (xscroll) {
            // left arrow
            if ((showLeftScrollNib) || (this.overXScrollBar) || (this.xscrollbar.mousedownValue != null)) {
                hctx.save();
                hctx.translate(this.nibleft.x + midpoint, this.nibleft.y + midpoint);
                hctx.rotate(Math.PI * 1.5);
                this.paintNib(hctx, xthemes.n, xthemes.a, xthemes.s);
                hctx.restore();
            }

            // right arrow
            if ((showRightScrollNib) || (this.overXScrollBar) || (this.xscrollbar.mousedownValue != null)) {
                hctx.save();
                hctx.translate(this.nibright.x + midpoint, this.nibright.y + midpoint);
                hctx.rotate(Math.PI * 0.5);
                this.paintNib(hctx, xthemes.n, xthemes.a, xthemes.s);
                hctx.restore();
            }
        }

        // the bar
        var sx = this.nibleft.x2 + 4;
        var sw = this.nibright.x - this.nibleft.x2 - 9;
        this.xscrollbar.rect = new Rect(sx, this.nibleft.y - 1, sw, this.nibleft.h + 1);
        this.xscrollbar.value = -this.xoffset;
        this.xscrollbar.min = 0;
        this.xscrollbar.max = virtualwidth - (cwidth - this.gutterWidth);
        this.xscrollbar.extent = cwidth - this.gutterWidth;

        if (xscroll) {
            var fullonxbar = (((this.overXScrollBar) && (virtualwidth > cwidth)) || ((this.xscrollbar) && (this.xscrollbar.mousedownValue != null)));
            if (!fullonxbar) hctx.globalAlpha = 0.3;
            this.paintScrollbar(hctx, this.xscrollbar);
            hctx.globalAlpha = 1.0;
        }

        var sy = this.nibup.y2 + 4;
        var sh = this.nibdown.y - this.nibup.y2 - 9;
        this.yscrollbar.rect = new Rect(this.nibup.x - 1, sy, this.nibup.w + 1, sh);
        this.yscrollbar.value = -this.yoffset;
        this.yscrollbar.min = 0;
        this.yscrollbar.max = virtualheight - cheight;
        this.yscrollbar.extent = cheight;

        if (yscroll) {
            var fullonybar = ((this.overYScrollBar) && (virtualheight > cheight)) || ((this.yscrollbar) && (this.yscrollbar.mousedownValue != null));
            if (!fullonybar) vctx.globalAlpha = 0.3;
            this.paintScrollbar(vctx, this.yscrollbar);
            vctx.globalAlpha = 1;
        }

        // composite the scrollbars
        ctx.drawImage(this.verticalScrollCanvas, verticalx, 0);
        ctx.drawImage(this.horizontalScrollCanvas, 0, horizontaly);
        hctx.restore();
        vctx.restore();

        // clear the unusued nibs
        if (!showUpScrollNib) this.nibup = new Rect();
        if (!showDownScrollNib) this.nibdown = new Rect();
        if (!showLeftScrollNib) this.nibleft = new Rect();
        if (!showRightScrollNib) this.nibright = new Rect();
    },

    paintScrollbar: function(ctx, scrollbar) {
        var bar = scrollbar.getHandleBounds();
        var alpha = (ctx.globalAlpha) ? ctx.globalAlpha : 1;

        if (!scrollbar.isH()) {
            ctx.save();     // restored in another if (!scrollbar.isH()) block at end of function
            ctx.translate(bar.x + Math.floor(bar.w / 2), bar.y + Math.floor(bar.h / 2));
            ctx.rotate(Math.PI * 1.5);
            ctx.translate(-(bar.x + Math.floor(bar.w / 2)), -(bar.y + Math.floor(bar.h / 2)));

            // if we're vertical, the bar needs to be re-worked a bit
            bar = new bespin.editor.Rect(bar.x - Math.floor(bar.h / 2) + Math.floor(bar.w / 2),
                    bar.y + Math.floor(bar.h / 2) - Math.floor(bar.w / 2), bar.h, bar.w);
        }

        var halfheight = bar.h / 2;

        ctx.beginPath();
        ctx.arc(bar.x + halfheight, bar.y + halfheight, halfheight, Math.PI / 2, 3 * (Math.PI / 2), false);
        ctx.arc(bar.x2 - halfheight, bar.y + halfheight, halfheight, 3 * (Math.PI / 2), Math.PI / 2, false);
        ctx.lineTo(bar.x + halfheight, bar.y + bar.h);
        ctx.closePath();

        var gradient = ctx.createLinearGradient(bar.x, bar.y, bar.x, bar.y + bar.h);
        gradient.addColorStop(0, this.editor.theme.scrollBarFillGradientTopStart.replace(/%a/, alpha));
        gradient.addColorStop(0.4, this.editor.theme.scrollBarFillGradientTopStop.replace(/%a/, alpha));
        gradient.addColorStop(0.41, this.editor.theme.scrollBarFillStyle.replace(/%a/, alpha));
        gradient.addColorStop(0.8, this.editor.theme.scrollBarFillGradientBottomStart.replace(/%a/, alpha));
        gradient.addColorStop(1, this.editor.theme.scrollBarFillGradientBottomStop.replace(/%a/, alpha));
        ctx.fillStyle = gradient;
        ctx.fill();

        ctx.save();
        ctx.clip();

        ctx.fillStyle = this.editor.theme.scrollBarFillStyle.replace(/%a/, alpha);
        ctx.beginPath();
        ctx.moveTo(bar.x + (halfheight * 0.4), bar.y + (halfheight * 0.6));
        ctx.lineTo(bar.x + (halfheight * 0.9), bar.y + (bar.h * 0.4));
        ctx.lineTo(bar.x, bar.y + (bar.h * 0.4));
        ctx.closePath();
        ctx.fill();
        ctx.beginPath();
        ctx.moveTo(bar.x + bar.w - (halfheight * 0.4), bar.y + (halfheight * 0.6));
        ctx.lineTo(bar.x + bar.w - (halfheight * 0.9), bar.y + (bar.h * 0.4));
        ctx.lineTo(bar.x + bar.w, bar.y + (bar.h * 0.4));
        ctx.closePath();
        ctx.fill();

        ctx.restore();

        ctx.beginPath();
        ctx.arc(bar.x + halfheight, bar.y + halfheight, halfheight, Math.PI / 2, 3 * (Math.PI / 2), false);
        ctx.arc(bar.x2 - halfheight, bar.y + halfheight, halfheight, 3 * (Math.PI / 2), Math.PI / 2, false);
        ctx.lineTo(bar.x + halfheight, bar.y + bar.h);
        ctx.closePath();

        ctx.strokeStyle = this.editor.theme.scrollTrackStrokeStyle;
        ctx.stroke();

        if (!scrollbar.isH()) {
            ctx.restore();
        }
    },

    paintNib: function(ctx, nibStyle, arrowStyle, strokeStyle) {
        var midpoint = Math.floor(this.NIB_WIDTH / 2);

        ctx.fillStyle = nibStyle;
        ctx.beginPath();
        ctx.arc(0, 0, Math.floor(this.NIB_WIDTH / 2), 0, Math.PI * 2, true);
        ctx.closePath();
        ctx.fill();
        ctx.strokeStyle = strokeStyle;
        ctx.stroke();

        ctx.fillStyle = arrowStyle;
        ctx.beginPath();
        ctx.moveTo(0, -midpoint + this.NIB_ARROW_INSETS.top);
        ctx.lineTo(-midpoint + this.NIB_ARROW_INSETS.left, midpoint - this.NIB_ARROW_INSETS.bottom);
        ctx.lineTo(midpoint - this.NIB_ARROW_INSETS.right, midpoint - this.NIB_ARROW_INSETS.bottom);
        ctx.closePath();
        ctx.fill();
    },

    // returns metadata bout the a string that represents the row; converts tab characters to spaces
    getRowString: function(row) {
        return this.model.getRowMetadata(row).lineText;
    },

    getRowScreenLength: function(row) {
        return this.getRowString(row).length;
    },

    // returns the maximum number of display columns across all rows
    getMaxCols: function(firstRow, lastRow) {
        var cols = 0;
        for (var i = firstRow; i <= lastRow; i++) {
            cols = Math.max(cols, this.getRowScreenLength(i));
        }
        return cols;
    },

    setSearchString: function(str) {
        if (str && str != '') {
            this.searchString = str;
        } else {
            delete this.searchString;
        }
        
        this.model.searchStringChanged(this.searchString);
        
        this.editor.paint(true);
    }
});

// ** {{{ bespin.editor.API }}} **
//
// The root object. This is the API that others should be able to use
dojo.declare("bespin.editor.API", null, {
    constructor: function(container, opts) {
        this.opts = opts || {};

        // fixme: this stuff may not belong here
        this.debugMode = false;

        this.container = dojo.byId(container);
        this.model = new bespin.editor.DocumentModel(this);

        dojo.byId(container).innerHTML = '<canvas id="canvas" moz-opaque="true" tabindex="-1"></canvas>';
        this.canvas = dojo.byId(container).firstChild;
        while (this.canvas && this.canvas.nodeType != 1) this.canvas = this.canvas.nextSibling;

        this.cursorManager = new bespin.editor.CursorManager(this);
        this.ui = new bespin.editor.UI(this);
        this.theme = bespin.themes['default'];

        this.editorKeyListener = new bespin.editor.DefaultEditorKeyListener(this);
        this.undoManager = new bespin.editor.UndoManager(this);
        this.customEvents = new bespin.editor.Events(this);

        this.ui.installKeyListener(this.editorKeyListener);

        this.model.insertCharacters({ row: 0, col: 0 }, " ");

        dojo.connect(this.canvas, "blur",  dojo.hitch(this, function(e) { this.setFocus(false); }));
        dojo.connect(this.canvas, "focus", dojo.hitch(this, function(e) { this.setFocus(true); }));

        bespin.editor.clipboard.setup(this); // setup the clipboard

        this.paint();

        if (!this.opts.dontfocus) { this.setFocus(true); }
    },

    // ensures that the start position is before the end position; reading directly from the selection property makes no such guarantee
    getSelection: function(selection) {
        selection = (selection != undefined) ? selection : this.selection;
        if (!selection) return undefined;

        var startPos = selection.startPos;
        var endPos = selection.endPos;

        // ensure that the start position is always before than the end position
        if ((endPos.row < startPos.row) || ((endPos.row == startPos.row) && (endPos.col < startPos.col))) {
            var foo = startPos;
            startPos = endPos;
            endPos = foo;
        }

        return {
            startPos: bespin.editor.utils.copyPos(startPos),
            endPos: bespin.editor.utils.copyPos(endPos),
            startModelPos: this.getModelPos(startPos),
            endModelPos: this.getModelPos(endPos)
        };
    },

    // helper
    getCursorPos: function(modelPos) {
        return this.cursorManager.getCursorPosition(modelPos);
    },

    // helper
    getModelPos: function(pos) {
        return this.cursorManager.getModelPosition(pos);
    },

    // restore the state of the editor
    resetView: function(data) {
        this.cursorManager.moveCursor(data.cursor);
        this.setSelection(data.selection);
        this.ui.yoffset = data.offset.y;
        this.ui.xoffset = data.offset.x;
        this.paint();
    },

    basicView: function() {
        this.cursorManager.moveCursor({row: 0, col: 0});
        this.setSelection(undefined);
        this.ui.yoffset = 0;
        this.ui.xoffset = 0;
        this.paint();
    },

    getCurrentView: function() {
        return { cursor: this.getCursorPos(), offset: { x: this.ui.xoffset, y: this.ui.yoffset }, selection: this.selection };
    },

    // be gentle trying to get the tabstop from settings
    getTabSize: function() {
        var settings = bespin.get("settings");
        var size = bespin.defaultTabSize; // default
        if (settings) {
            var tabsize = parseInt(settings.get("tabsize"));
            if (tabsize > 0) size = tabsize;
        }
        return size;
    },

    // helper to get text
    getSelectionAsText: function() {
        var selectionText = '';
        var selectionObject = this.getSelection();
        if (selectionObject) {
            selectionText = this.model.getChunk(selectionObject);
        }
        return selectionText;
    },

    setSelection: function(selection) {
        this.selection = selection;
        if (this.undoManager.syncHelper) this.undoManager.syncHelper.queueSelect(selection);
    },

    paint: function(fullRefresh) {
        var ctx = bespin.util.canvas.fix(this.canvas.getContext("2d"));
        this.ui.paint(ctx, fullRefresh);
    },

    changeKeyListener: function(newKeyListener) {
        this.ui.installKeyListener(newKeyListener);
        this.editorKeyListener = newKeyListener;
    },

    // this does not set focus to the editor; it indicates that focus has been set to the underlying canvas
    setFocus: function(focus) {
        this.focus = focus;
    }
});
