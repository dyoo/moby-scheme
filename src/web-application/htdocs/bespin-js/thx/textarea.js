dojo.provide("thx.textarea");
dojo.require("bespin.util.keys");

dojo.declare("thx.textarea.TextArea", th.Container, {
    /*
     * Functions that need overriding (th.Container)
     */
    constructor: function (params) {
        this.style = params.style || {};

        this.scrollbar = new thx.textarea.Scrollbar();
        this.scrollbar.style = this.style;
        this.scrollbar.scrollable = this;
        this.scrollbar.opaque = false;
        this.add(this.scrollbar);

        this.cursor = {row: 0, col: 0};
        this.cursor_prev = {row: 0, col: 0};

        this.fvl = 0;   // FVL = First Visible Line
        this.fvl_prev = 0;  // this one lags one step behind this.fvl in order to detect and measure changes
        this.cfvl = 0;  // a continuous version of fvl (in order for the scrollbar motion to be smooth)

        this.nol = 0; // total Number Of Lines (wrapped) in the editor

        this.selection = undefined; // Defined as undefined just for the sake of making it clear that this is a member

        // TODO: Both paddings, I think, ought to be read from CSS
        this.leftPadding = 10;
        this.rightPadding = 10 + 16;

        this.recalcCharSize = true;
        // Mock up section, TODO: these have to be done externally
        this.actions = new thx.textarea.Actions(this);
        this.listener = new thx.textarea.KeyListener(this, this.actions);
        dojo.connect(document, "keypress", this, dojo.hitch(this.listener, "onkeypress"));
        dojo.connect(document, "keydown", this, dojo.hitch(this.listener, "onkeydown"));
        var Key = bespin.util.keys.Key; // alias
        this.listener.bindKeyStringSelectable("", Key.LEFT_ARROW, this.actions.moveCursorLeft);
        this.listener.bindKeyStringSelectable("", Key.ARROW_RIGHT, this.actions.moveCursorRight);
        this.listener.bindKeyStringSelectable("", Key.ARROW_UP, this.actions.moveCursorUp);
        this.listener.bindKeyStringSelectable("", Key.ARROW_DOWN, this.actions.moveCursorDown);
        this.listener.bindKeyString("", Key.BACKSPACE, this.actions.backspace);
        this.listener.bindKeyString("", Key.DELETE, this.actions.deleteKey);
        this.listener.bindKeyString("", Key.ENTER, this.actions.newline);
        this.listener.bindKeyStringSelectable("", Key.HOME, this.actions.moveToLineStart);
        this.listener.bindKeyStringSelectable("", Key.END, this.actions.moveToLineEnd);
        this.listener.bindKeyStringSelectable("", Key.PAGE_UP, this.actions.movePageUp);
        this.listener.bindKeyStringSelectable("", Key.PAGE_DOWN, this.actions.movePageDown);
        this.listener.bindKeyStringSelectable("ALT", Key.LEFT_ARROW, this.actions.moveWordLeft);
        this.listener.bindKeyStringSelectable("ALT", Key.ARROW_RIGHT, this.actions.moveWordRight);
        this.listener.bindKeyString("CTRL", Key.A, this.actions.selectAll);
    },

    layout: function () {
        var d = this.d();
        this.h = d.b.h;
        this.w = d.b.w;

        // set the scrollbar bounds
        // TODO: the "16" should not be hardcoded
        this.scrollbar.bounds = { x: this.w - 16, y: 0, height: d.b.h, width: 16};
        this.scrollbar.increment = 1;

        // Calculate line-height and char-width using the Scene's scratch context
        if (this.recalcCharSize) {
            var tmpctx = this.getScratchContext();
            tmpctx.font = this.style.font;
            this.charSize = tmpctx.measureText("a");
            // I have no idea what ascent means, but in the old editor
            // implementation, the height is 2.8 times the ascent
            this.charSize.height = Math.floor(this.charSize.ascent * 2.8);
            this.recalcCharSize = false;
        }
        // effw := Effective Width (textarea width sans the left&right padding)
        this.effw = this.w - this.leftPadding - this.rightPadding;
        // vll := Visual Line Length
        this.vll = Math.floor(this.effw / this.charSize.width);
        // vlc := Visible Lines Count
        this.vlc = Math.floor((this.h - 0.75 * this.charSize.height) / this.charSize.height);

        this.refreshMetaData(0);
    },

    paintSelf: function (ctx) {
        if (this.lines.length === 0) // Nothing to draw
            return;
        ctx.save();
        
        this.shiftCanvasOnScroll(ctx);
        
        ctx.font = this.style.font;
        ctx.translate(this.leftPadding, this.charSize.height * 0.75);
        this.paintText(ctx);
        
        ctx.restore();
        
        // TODO: make it so that the near region of the curson is saved, so that it can be rapainted after it moves
        if (this.cursor_prev !== this.cursor) {
            this.paintCursor(ctx, this.cursor_prev, true);
            ctx.fillStyle = this.style.color;
            this.paintCursor(ctx, this.cursor, false);
        }
        this.paintSelection(ctx);
    },

    /*
     * "Virtual" functions
     */

    paintTextLine: function (ctx, row) {
        var text = this.model.getRowArray(row).join('');
        var line = this.lines[row];
        for (var k = 0; k < line.nowl; k++) {
            ctx.fillText(text.substr(k*this.vll, this.vll), 0, 0);
            ctx.translate(0, this.charSize.height);
        }
        ctx.translate(0, -this.charSize.height);
    },

    /*
     * "Public" functions
     */

    setCursor: function (cursor) {
        this.cursor_prev = dojo.clone(this.cursor);
        this.cursor = dojo.clone(cursor);
        if (cursor.row > this.nol) this.cursor.row = this.nol;
        this.handleViewShift();
    },

    setSelection: function (selection) {
        this.selection = dojo.clone(selection);
    },

    getCursor: function () {
        return dojo.clone(this.cursor); // it is important to return a clone in order to keep this.cursor for being modified
    },

    getSelection: function () {
        return dojo.clone(this.selection);
    },

    getPageRows: function () {
        return this.vlc;
    },

    getPageCols: function () {
        return this.vll;
    },

    assignModel: function (model) {
        if (model.type === "Document Model") // make sure it's the right sort of model. maybe istead check for the existance of key functions such as: getRowArray(), setRowDirty(), setRowClean(), getRowCount(), getRowLength() ...
            this.model = model;
        this.refreshMetaData(0);
    },

    assignListener: function (listener) {
        // TODO
    },

    // Required by the scrollbar child widget
    getScrollInfo: function () {
        return {offset: this.cfvl, span: this.nol, scope: this.vlc};
    },

    scrollUp: function (delta) {
        this.fvl_prev = this.fvl;
        if (delta > this.cfvl) {
            this.cfvl = 0;
        } else {
            this.cfvl -= delta;
        }
        this.fvl = Math.floor(this.cfvl);
        this.selectionChanged = true;
    },

    scrollDown: function (delta) {
        this.fvl_prev = this.fvl;
        if (this.cfvl + this.vlc + delta > this.nol) {
            this.cfvl = this.nol - this.vlc;
        } else {
            this.cfvl += delta;
        }
        this.fvl = Math.floor(this.cfvl);
        this.selectionChanged = true;
    },

    /*
     * "Private" functions
     */

    refreshMetaData: function (rowFrom) {
        if (this.model === undefined)
            return;
        if (this.vll === undefined || this.vlc === undefined) {
            this.layout();
            return; // the layout calls refreshMetaData already, so there's no use in going over it again
        }
        var row_offset
        if (this.lines === undefined || this.lines === [])
            row_offset = 0;
        else
            row_offset = this.lines[rowFrom].row_offset;
        var nowl;
        var k;
        var row_length;
        this.lines = [];
        for (k = rowFrom; k < this.model.getRowCount(); k++) {
            row_length = this.model.getRowLength(k);
            nowl = (row_length === 0) ? 1 : Math.ceil(row_length / this.vll);
            if (row_length !== 0 && row_length % this.vll === 0) // when the line fills the screen width, the cursor is on the next line, so effectively, the line wrapped
                nowl++;
            this.lines.push({nowl: nowl, row_offset: row_offset});
            this.model.setRowDirty(k);
            row_offset += nowl;
        }
        this.nol = row_offset;
        this.scrollbar.layout();
    },

    handleViewShift: function () {
        this.fvl_prev = this.fvl;
        var row = this.lines[this.cursor.row].row_offset + Math.floor(this.cursor.col / this.vll);
        var cfvl_residue = this.cfvl - Math.floor(this.cfvl);
        // if the row has left the visible screen bounds, the first visible line needs to be updated
        if (row < this.fvl) {
            this.cfvl = row + cfvl_residue;
        } else if (row > (this.fvl + this.vlc - 1)) {
            this.cfvl = row - this.vlc + cfvl_residue;
        }
        this.fvl = Math.floor(this.cfvl);
        this.scrollbar.layout();
    },

    getTextLine: function (row) {
        // For a row in page coordinated, find the text line to which it belongs
        var k = 0;
        while (this.lines[k].row_offset <= row) {
            k++;
            if (k === this.lines.length) {
                return {index: this.lines.length - 1, offset: this.lines[this.lines.length - 1].nowl};
            }
        }
        if (k === 0) k = 1;
        return {index: k - 1, offset: row - this.lines[k - 1].row_offset};
    },

    getScreenCursor: function (cursor) {
        // return the cursor in "screen coordinates", i.e. relative to the view rather then to the page
        return {row: this.lines[cursor.row].row_offset + Math.floor(cursor.col / this.vll) - this.fvl,
                col: cursor.col % this.vll};
    },

    paintCursor: function (ctx, cursor, clear) { // cursor is a row/col hash, and clear is a flag to indicate whether the cursor is painted or erased (to enable smart redraw)
        var vcursor = this.getScreenCursor(cursor);
        var cvpx = this.leftPadding + this.charSize.width * vcursor.col;
        var cvpy = this.charSize.height * vcursor.row;
        if (clear === false) {
            ctx.fillRect(cvpx, cvpy, 1, this.charSize.height);
        } else {
            ctx.clearRect(cvpx, cvpy, 1, this.charSize.height);
        }
    },

    highlight :function (ctx, r, c1, c2) { // helper function for paintSelection()
        ctx.fillRect(c1 * this.charSize.width,
                r * this.charSize.height,
                (c2-c1) * this.charSize.width,
                this.charSize.height);
    },

    paintSelection: function (ctx) {
        if (this.selection === undefined) // no selection to paint
            return;
        if (this.selectionChanged === false) // no need to repaint
            return;
        var start = dojo.clone(this.selection.start);
        var end = dojo.clone(this.selection.end);
        // swap if the selection "end" is before the selection "start"
        if (start.row > end.row)
            [start, end] = [end, start];
        // calculate the position of the selection start and end "cursors"
        var vstart = this.getScreenCursor(start);
        var vend = this.getScreenCursor(end);
        if (vstart.row > this.vlc || vend.row < 0) // selection is outside the visibe view, no need to paint it
            return;
        // clip the painted selection to the visible view
        if (vstart.row < 0)
            vstart = {row: 0, col: 0};
        if (vend.row > this.vlc)
            vend = {row: this.vlc, col: this.vll};

        // start painting
        ctx.save();
        ctx.translate(this.leftPadding, 0);
        ctx.globalCompositeOperation = "xor";
        ctx.fillStyle = "blue"; // TODO: take from CSS
        // if the entire selection is just on one row:
        if (vstart.row == vend.row) {
            this.highlight(ctx, vstart.row, vstart.col, vend.col);
        } else {
            // do the first line of selection
            this.highlight(ctx, vstart.row, vstart.col, this.vll);
            // do the rest of the lines
            for (var k = vstart.row + 1; k < vend.row; k++)
                this.highlight(ctx, k, 0, this.vll);
            // do the last line
            this.highlight(ctx, vend.row, 0, vend.col);
        }
        ctx.restore();
        this.refreshMetaData(0);
    },

    shiftCanvasOnScroll: function (ctx) {
        var firstLine;
        var lastLine;
        var shift = (this.fvl - this.fvl_prev) * this.charSize.height; // vertical shift in pixels
        var tmpctx = this.getScratchContext();
        tmpctx.canvas.width = ctx.canvas.width;
        tmpctx.canvas.height = ctx.canvas.height;
        tmpctx.clearRect(0, 0, this.w, this.h);
        if (this.fvl > this.fvl_prev) {
            tmpctx.drawImage(ctx.canvas, 0, 0, this.w - 16, this.h, 0, -shift, this.w - 16, this.h);
            ctx.clearRect(0, 0, this.w - 16, this.h);
            ctx.drawImage(tmpctx.canvas, 0, 0, this.w - 16, this.h, 0, 0, this.w - 16, this.h);
            firstLine = this.getTextLine(this.fvl_prev + this.vlc);
            lastLine = this.getTextLine(this.fvl + this.vlc);
            for (var m = firstLine.index; m <= lastLine.index; m++) {
                this.model.setRowDirty(m);
            }
        } else if (this.fvl < this.fvl_prev) {
            tmpctx.drawImage(ctx.canvas, 0, 0, this.w - 16, this.h, 0, -shift, this.w - 16, this.h);
            ctx.clearRect(0, 0, this.w - 16, this.h);
            ctx.drawImage(tmpctx.canvas, 0, 0, this.w - 16, this.h, 0, 0, this.w - 16, this.h);
            firstLine = this.getTextLine(this.fvl);
            lastLine = this.getTextLine(this.fvl_prev);
            for (var m = firstLine.index; m <= lastLine.index; m++) {
                this.model.setRowDirty(m);
            }
        }
        this.fvl_prev = this.fvl;
    },

    paintText: function (ctx) {
        var firstLine = this.getTextLine(this.fvl);
        var lastLine = this.getTextLine(this.fvl + this.vlc);
        // Make it so that the first firstLine.offset wrapped lines are outside the clipped area
        ctx.translate(0, - firstLine.offset * this.charSize.height);
        for(var k = firstLine.index; k <= lastLine.index; k++) {
            if (this.model.isRowDirty(k)) { // The text has to be redrawen
                ctx.clearRect(-this.leftPadding, -this.charSize.height * 0.75, this.w, this.charSize.height * this.lines[k].nowl);
                ctx.fillStyle = this.style.color;
                this.paintTextLine(ctx, k);
                ctx.translate(0, this.charSize.height);
                this.model.setRowClean(k);
            } else { // Just need to translate
                ctx.translate(0, this.charSize.height * this.lines[k].nowl);
            }
        }
    }
});

/* Scrollbar2 - the next generation
 * The main changes:
 *  1. This scrollbar maps from the scrollable's "value" space to the
 *      scrollbar's pixel space, as opposed to "th.components.Scrollbar"
 *      which maps from the scrollable's pixel space to the scrollbar's
 *      pixel space.
 *  2. This scrollbar does not interfere with the scrollable's properties
 *      but instead, interacts with it using two entry functions:
 *      scrollUp and scrollDown which take as an argument the number of
 *      "value units" the scrollable has to move
 */
dojo.declare("thx.textarea.Scrollbar", th.Container, {
    constructor: function(parms) {
        if (!parms) parms = {};
        this.orientation = parms.orientation || th.VERTICAL;
        this.value = parms.value || 0;
        this.min = parms.min || 0;
        this.max = parms.max || 100;
        this.increment = parms.increment || 2;

        this.up = new th.components.Button();
        this.down = new th.components.Button();
        this.bar = new th.components.Button();
        this.add([ this.up, this.down, this.bar ]);

        this.bus.bind("click", this.up, this.scrollup, this);
        this.bus.bind("click", this.down, this.scrolldown, this);
        this.bus.bind("mousedrag", this.bar, this.onmousedrag, this);
        this.bus.bind("mouseup", this.bar, this.onmouseup, this);
    },

    onmousedrag: function(e) {
        var currentPosition = (this.orientation == th.VERTICAL) ? e.clientY : e.clientX;

        if (this.dragstart_mouse === undefined) {
            this.dragstart_mouse = currentPosition;
            return;
        }

        // difference in pixels; needs to be translated to a difference in value
        var diff = currentPosition - this.dragstart_mouse;
        this.dragstart_mouse = currentPosition;
        
        // the difference in the value
        var delta = diff / this.ratio;
        this.value += delta;
        if (this.value < this.min) this.value = this.min;
        if (this.value > this.max) this.value = this.max;
        this.layout();
        if (this.scrollable) 
            if (delta > 0)
                this.scrollable.scrollDown(delta);
            else
                this.scrollable.scrollUp(-delta);
        this.repaint();
        if (this.scrollable) this.scrollable.repaint();
    },

    onmouseup: function(e) {
        delete this.dragstart_value;
        delete this.dragstart_mouse;
    },

    scrollup: function(e) {
        if (this.value > this.min) {
            this.value = Math.min(this.min, this.value - this.increment);
            if (this.scrollable) this.scrollable.scrollUp(this.increment);
            this.render();
            if (this.scrollable) this.scrollable.repaint();
        }
    },

    scrolldown: function(e) {
        if (this.value < this.max) {
            this.value = Math.min(this.max, this.value + this.increment);
            if (this.scrollable) this.scrollable.scrollDown(this.increment);
            this.render();
            if (this.scrollable) this.scrollable.repaint();
        }
    },

    layout: function() {
        var d = this.d();

        if (this.orientation == th.VERTICAL) {
            var w = d.b.iw;
            var h = 12;
            this.up.bounds = { x: d.i.l + 1, y: d.i.t, width: w, height: h };
            this.down.bounds = { x: d.i.l + 1, y: d.b.ih - h, width: w, height: h };
        }

        // check if there's a scrollable attached; if so, refresh state
        if (this.scrollable !== undefined) {
            // si := scrollable info
            var si = this.scrollable.getScrollInfo();
            var scrollbar_span = d.b.ih - this.up.bounds.height - this.down.bounds.height;
            this.min = 0;
            this.max = si.span - si.scope;
            this.scope = si.scope;
            this.ratio = scrollbar_span / si.span;
            this.value = si.offset;
        }

        // if the maximum value is less than the minimum, we're in an invalid state and won't paint anything
        if (this.max < this.min) {
            for (var i = 0; i < this.children.length; i++) delete this.children[i].bounds;
            return;
        }

        if (this.orientation == th.VERTICAL) {
            var bar_length = Math.floor(this.ratio * this.scope);
            var bar_top = Math.floor(this.up.bounds.height + this.ratio * this.value);

            this.bar.bounds = { x: d.i.l + 1, y: bar_top + 3, width: d.b.iw, height: bar_length }
        } else {
            // this.orientation ===th.HORIZONTAL ???
        }
    },

    paint: function(ctx) {
        if (this.max < 0) return;

        // paint the track
        if (this.style.scrollTopImage)
            ctx.drawImage(this.style.scrollTopImage, 1, this.up.bounds.height);
        if (this.style.scrollMiddleImage)
            ctx.drawImage(this.style.scrollMiddleImage, 1,
                    this.up.bounds.height + this.style.scrollTopImage.height,
                    this.style.scrollMiddleImage.width,
                    this.down.bounds.y - this.down.bounds.height - (this.up.bounds.x - this.up.bounds.height));
        if (this.style.scrollBottomImage)
            ctx.drawImage(this.style.scrollBottomImage, 1, this.down.bounds.y - this.style.scrollBottomImage.height);

        // propagate the styles to the children if not already there
        if (this.style.scrollHandleTopImage && !this.bar.style.topImage) {
            this.bar.style.topImage = this.style.scrollHandleTopImage;
            this.bar.style.middleImage = this.style.scrollHandleMiddleImage;
            this.bar.style.bottomImage = this.style.scrollHandleBottomImage;
            this.up.style.backgroundImage = this.style.scrollUpArrow;
            this.down.style.backgroundImage = this.style.scrollDownArrow;
        }

        this.inherited(arguments);
    }     
});

dojo.declare("thx.textarea.Actions", null, { 
    constructor: function(editor) {
        this.editor = editor;
        this.ignoreRepaints = false;
    },

    // this is a generic helper method used by various cursor-moving methods
    handleCursorSelection: function(args) {
        if (args.event.shiftKey) {
            if (this.editor.selection === undefined) this.editor.setSelection({start: args.pos});
            this.editor.setSelection({start: this.editor.getSelection().start, end: this.editor.cursor});
        } else {
            this.editor.setSelection(undefined);
        }
    },

    moveCursorLeft: function(args) {
        var pos = dojo.clone(args.pos);
        if (pos.col > 0)
            pos.col--;
        else if (pos.row > 0) {
                pos.row--;
                pos.col = this.editor.model.getRowLength(pos.row);
        }
        this.editor.setCursor(pos);
        this.handleCursorSelection(args);
        this.editor.repaint();
        return args;
    },

    moveCursorRight: function(args) {
        var pos = dojo.clone(args.pos);
        if (pos.col < this.editor.model.getRowLength(pos.row))
            pos.col++;
        else if (pos.row < this.editor.model.getRowCount()) {
                pos.row++;
                pos.col = 0;
        }
        this.editor.setCursor(pos);
        this.handleCursorSelection(args);
        this.editor.repaint();
        return args;
    },

    moveCursorUp: function(args) {
        var pos = dojo.clone(args.pos);
        if (pos.row > 0) {
            pos.row--;
            pos.col = Math.min(pos.col, this.editor.model.getRowLength(pos.row)); 
        }
        this.editor.setCursor(pos);
        this.handleCursorSelection(args);
        this.editor.repaint();
        return args;
    },

    moveCursorDown: function(args) {
        var pos = dojo.clone(args.pos);
        if (pos.row < this.editor.model.getRowCount() - 1) {
            pos.row++;
            pos.col = Math.min(pos.col, this.editor.model.getRowLength(pos.row));
        }
        this.editor.setCursor(pos);
        this.handleCursorSelection(args);
        this.editor.repaint();
        return args;
    },

    moveToLineStart: function(args) {
        var pos = dojo.clone(args.pos);
        var line = this.editor.model.getRowArray(pos.row).join('');
        var match = /^(\s+).*/.exec(line);
        var leadingWhitespaceLength = 0;

        // Check to see if there is leading white space and move to the first text if that is the case
        if (match && match.length == 2) {
            leadingWhitespaceLength = match[1].length;
        }

        if (pos.col == 0) {
            pos.col = leadingWhitespaceLength;
        } else if (pos.col == leadingWhitespaceLength) {
            pos.col = 0;
        } else {
            pos.col = leadingWhitespaceLength;
        }
        this.editor.setCursor(pos);
        this.handleCursorSelection(args);
        this.editor.repaint();
        return args;
    },

    moveToLineEnd: function(args) {
        var pos = dojo.clone(args.pos);
        pos.col = this.editor.model.getRowLength(pos.row);
        this.editor.setCursor(pos);
        this.handleCursorSelection(args);
        this.editor.repaint();
        return args;
    },

    moveToFileTop: function(args) {
        var pos = dojo.clone(args.pos);
        pos.row = 0;
        pos.col = Math.min(pos.col, this.editor.model.getRowLength(pos.row));
        this.editor.setCursor(pos);
        this.handleCursorSelection(args);
        this.editor.repaint();
        return args;
    },

    moveToFileBottom: function(args) {
        var pos = dojo.clone(args.pos);
        pos.row = this.editor.model.getRowCount() - 1;
        pos.col = Math.min(pos.col, this.editor.model.getRowLength(pos.row));
        this.editor.setCursor(pos);
        this.handleCursorSelection(args);
        this.editor.repaint();
        return args;
    },

    movePageUp: function(args) {
        var pos = dojo.clone(args.pos);
        if (pos.row > this.editor.getPageRows()) {
            pos.row -= this.editor.getPageRows();
        } else {
            pos.row = 0;
        }
        pos.col = Math.min(pos.col, this.editor.model.getRowLength(pos.row));
        this.editor.setCursor(pos);
        this.handleCursorSelection(args);
        this.editor.repaint();
        return args;
    },

    movePageDown: function(args) {
        var pos = dojo.clone(args.pos);
        if (pos.row < this.editor.model.getRowCount() - this.editor.getPageRows()) {
            pos.row += this.editor.getPageRows();
        } else {
            pos.row = this.editor.model.getRowCount() - 1;
        }
        pos.col = Math.min(pos.col, this.editor.model.getRowLength(pos.row));
        this.editor.setCursor(pos);
        this.handleCursorSelection(args);
        this.editor.repaint();
        return args;
    },

    moveWordLeft: function(args) {
        var pos = dojo.clone(args.pos);
        var row = this.editor.model.getRowArray(pos.row);
        var c, charCode;

        if (args.pos.col == 0) { // -- at the start to move up and to the end
            var newargs = this.moveCursorUp(args);
            this.moveToLineEnd(newargs);
            return;
        }

        // Short circuit if cursor is ahead of actual spaces in model
        if (row.length < pos.col) {
            args = this.moveToLineEnd(args);
        }
        var newcol = pos.col;

        // This slurps up trailing spaces
        var wasSpaces = false;
        while (newcol > 0) {
            newcol--;

            c = row[newcol];
            charCode = c.charCodeAt(0);
            if (charCode == 32) {
                wasSpaces = true;
            } else {
                newcol++;
                break;
            }
        }

        // This jumps to stop words        
        if (!wasSpaces) {
            while (newcol > 0) {
                newcol--;
                c = row[newcol];
                charCode = c.charCodeAt(0);
                if ( (charCode < 65) || (charCode > 122) ) { // if you get to an alpha you are done
                    if (newcol != args.pos.col - 1) newcol++; // right next to a stop char, move back one
                    break;
                }
            }
        }
        
        pos.col = newcol;
        this.editor.setCursor(pos);
        this.handleCursorSelection(args);
        this.editor.repaint();
    },

    moveWordRight: function(args) {
        var pos = dojo.clone(args.pos);
        var row = this.editor.model.getRowArray(pos.row);
        var c, charCode;

        if (row.length <= pos.col) { // -- at the edge so go to the next line
            this.moveCursorDown(this.moveToLineStart(args));
            return;
        }

        var newcol = pos.col;

        // This slurps up leading spaces
        var wasSpaces = false;
        while (newcol < row.length) {
            c = row[newcol];
            charCode = c.charCodeAt(0);
            if (charCode == 32) {
                wasSpaces = true;
                newcol++;
            } else {
                break;
            }
        }

        // This jumps to stop words        
        if (!wasSpaces) {        
            while (newcol < row.length) {
                newcol++;

                if (row.length == newcol) { // one more to go
                    this.moveToLineEnd(args);
                    return;
                }

                c = row[newcol];
                charCode = c.charCodeAt(0);
            
                if ( (charCode < 65) || (charCode > 122) ) {
                    break;
                }
            }
        }
    
        pos.col = newcol;
        this.editor.setCursor(pos);
        this.handleCursorSelection(args);
        this.editor.repaint();
    },

    undoRedo: function(args) {
        if (! args.event.shiftKey) {    // holding down the shift key causes the undo keystroke to be a redo TODO: move this logic to key handler
            this.undo();
        } else {
            this.redo();
        }
    },

    undo: function() {
        this.editor.undoManager.undo();
    },

    redo: function() {
        this.editor.undoManager.redo();
    },

    selectAll: function(args) {
        // do nothing with an empty doc
        if (this.editor.model.getMaxCols == 0) return;

        args.start = { col: 0, row: 0 };
        args.end = { col: this.editor.model.getRowLength(this.editor.model.getRowCount() - 1), row: this.editor.model.getRowCount() - 1 };

        this.select(args);
    },

    select: function(args) {
        if (args.start) {
            this.editor.setSelection({ start: args.start, end: args.end });
            this.editor.setCursor(args.end);
        } else {
            this.editor.setSelection(undefined);
        }
        this.editor.repaint();
    },

    insertTab: function(args) {
        if (this.editor.getSelection() && !args.undoInsertTab) {
            this.indent(args);
            return;
        }

        var tabWidth = parseInt(_settings.get('tabsize') || bespin.defaultTabSize);   // TODO: global needs fixing
        var tabWidthCount = tabWidth;
        var tab = "";
        while (tabWidthCount-- > 0) {
            tab += " ";
        }
        
        this.editor.model.insertCharacters({row: args.pos.row, col: args.pos.col}, tab);            
        this.editor.moveCursor({row: args.pos.row, col: args.pos.col + tabWidth});
        
        this.repaint();
        
        // undo/redo
        args.action = "insertTab";
        var redoOperation = args;
        var undoArgs = { action: "removeTab", queued: args.queued, pos: bespin.editor.utils.copyPos(args.pos) };
        var undoOperation = undoArgs;
        this.editor.undoManager.addUndoOperation(new bespin.editor.UndoItem(undoOperation, redoOperation));
    },
    
    // this function can only be called by editor.undoManager for undo insertTab in the case of beeing nothing selected
    removeTab: function(args) {
        var tabWidth = parseInt(_settings.get('tabsize') || bespin.defaultTabSize);   // TODO: global needs fixing
        
        this.editor.model.deleteCharacters({row: args.pos.row, col: args.pos.col}, tabWidth);
        this.editor.moveCursor({row: args.pos.row, col: args.pos.col});
        
        this.repaint();
        
        args.action = "removeTab";
        var redoOperation = args;
        var undoArgs = { action: "insertTab", undoInsertTab: true, queued: args.queued, pos: bespin.editor.utils.copyPos(args.pos) };
        var undoOperation = undoArgs;
        this.editor.undoManager.addUndoOperation(new bespin.editor.UndoItem(undoOperation, redoOperation));
    },

    indent: function(args) {
        var historyIndent = args.historyIndent || false;    
        if (!historyIndent) {
            var newHistoryIndent = [];
        }
        var selection = args.selection || this.editor.getSelection();
        var fakeSelection = args.fakeSelection || false;
        var startRow = selection.startPos.row;
        var endRow = selection.endPos.row;
        var tabWidth = parseInt(_settings.get('tabsize') || bespin.defaultTabSize);   // TODO: global needs fixing
        var tabWidthCount = tabWidth;
        var tab = "";
        while (tabWidthCount-- > 0) {
            tab += " ";
        }

        for (var y = startRow; y <= endRow; y++) {
            if (!historyIndent) {
                var row = this.editor.model.getRowArray(y).join("");
                var match = /^(\s+).*/.exec(row);
                var leadingWhitespaceLength = 0;
                if (match && match.length == 2) {
                    leadingWhitespaceLength = match[1].length;
                }
                var charsToInsert = (leadingWhitespaceLength % tabWidth ? tabWidth - (leadingWhitespaceLength % tabWidth) : tabWidth);
                this.editor.model.insertCharacters({row: y, col: 0}, tab.substring(0, charsToInsert));
                newHistoryIndent.push(charsToInsert);
            } else {
                this.editor.model.insertCharacters({row: y, col: 0}, tab.substring(0, historyIndent[y - startRow]));                
            } 
        }

        if (!fakeSelection) {
            selection.startPos.col += (historyIndent ? historyIndent[0] : tab.length);
            selection.endPos.col += (historyIndent ? historyIndent[historyIndent.length-1] : tab.length);
            this.editor.setSelection(selection);
        }
        args.pos.col += (historyIndent ? historyIndent[historyIndent.length-1] : tab.length);
        this.editor.cursorPosition.col = args.pos.col;
        historyIndent = historyIndent ? historyIndent : newHistoryIndent;
        this.repaint();

        // undo/redo
        args.action = "indent";
        args.selection = selection;
        var redoOperation = args;
        var undoArgs = { action: "unindent", queued: args.queued, selection: selection, fakeSelection: fakeSelection, historyIndent: historyIndent, pos: bespin.editor.utils.copyPos(args.pos) };
        var undoOperation = undoArgs;
        this.editor.undoManager.addUndoOperation(new bespin.editor.UndoItem(undoOperation, redoOperation));
    },

    unindent: function(args) {
        var historyIndent = args.historyIndent || false;
        if (!historyIndent) {
            var newHistoryIndent = [];
        }
        var selection = args.selection || this.editor.getSelection();
        var fakeSelection = args.fakeSelection || false;
        if (!selection) {
            fakeSelection = true;
            selection = {startPos: {row: args.pos.row, col: args.pos.col}, endPos: {row: args.pos.row, col: args.pos.col}};
        }
        var startRow = selection.startPos.row;
        var endRow = selection.endPos.row;
        var tabWidth = parseInt(_settings.get('tabsize') || bespin.defaultTabSize);   // TODO: global needs fixing

        for (var y = startRow; y <= endRow; y++) {
            if (historyIndent) {
                var charsToDelete = historyIndent[y - startRow];
            } else {
                var row = this.editor.model.getRowArray(y).join("");
                var match = /^(\s+).*/.exec(row);
                var leadingWhitespaceLength = 0;
                if (match && match.length == 2) {
                    leadingWhitespaceLength = match[1].length;
                }
                var charsToDelete = leadingWhitespaceLength >= tabWidth ? (leadingWhitespaceLength % tabWidth ? leadingWhitespaceLength % tabWidth : tabWidth) : leadingWhitespaceLength;               
                newHistoryIndent.push(charsToDelete);
            }

            if (charsToDelete) {
                this.editor.model.deleteCharacters({row: y, col: 0}, charsToDelete);
            }
            if (y == startRow) {
                selection.startPos.col = Math.max(0, selection.startPos.col - charsToDelete);
            }
            if (y == endRow) {
                selection.endPos.col = Math.max(0, selection.endPos.col - charsToDelete);
            }
            if (y == args.pos.row) {
                args.pos.col = Math.max(0, args.pos.col - charsToDelete);
            }
        }
        this.editor.cursorPosition.col = args.pos.col;

        if (!fakeSelection) {
            this.editor.setSelection(selection);
        }
        historyIndent = historyIndent ? historyIndent : newHistoryIndent;
        this.repaint();
        
        // undo/redo
        args.action = "unindent";
        args.selection = selection;
        var redoOperation = args;
        var undoArgs = { action: "indent", queued: args.queued, selection: selection, fakeSelection: fakeSelection, historyIndent: historyIndent, pos: bespin.editor.utils.copyPos(args.pos) };
        var undoOperation = undoArgs;
        this.editor.undoManager.addUndoOperation(new bespin.editor.UndoItem(undoOperation, redoOperation));
    },

    // NOTE: Actually, clipboard.js is taking care of this unless EditorOnly mode is set
    cutSelection: function(args) {
        this.copySelection(args);
        this.deleteSelection(args);
    },
    
    // NOTE: Actually, clipboard.js is taking care of this unless EditorOnly mode is set
    copySelection: function(args) {
        var selectionObject = this.editor.getSelection();
        if (selectionObject) {
            var selectionText = this.editor.model.getChunk(selectionObject);
            if (selectionText) {
                bespin.util.clipboard.Manual.copy(selectionText);
            }
        }
    },

    deleteSelectionAndInsertChunk: function(args) {
        var oldqueued = args.queued;

        args.queued = true;
        var selection = this.editor.getSelection();
        var chunk = this.deleteSelection(args);
        args.pos = bespin.editor.utils.copyPos(this.editor.cursorPosition);
        var endPos = this.insertChunk(args);

        args.queued = oldqueued;

        // undo/redo
        args.action = "deleteSelectionAndInsertChunk";
        args.selection = selection;
        var redoOperation = args;
        var undoArgs = {
            action: "deleteChunkAndInsertChunkAndSelect",
            pos: bespin.editor.utils.copyPos(args.pos),
            endPos: endPos,
            queued: args.queued,
            chunk: chunk
        };
        var undoOperation = undoArgs;
        this.editor.undoManager.addUndoOperation(new bespin.editor.UndoItem(undoOperation, redoOperation));
    },

    deleteChunkAndInsertChunkAndSelect: function(args) {
        var oldqueued = args.queued;

        args.queued = true;
        this.deleteChunk(args);
        this.insertChunkAndSelect(args);

        args.queued = oldqueued;

        // undo/redo
        args.action = "deleteChunkAndInsertChunkAndSelect";
        var redoOperation = args;
        var undoArgs = {
            action: "deleteSelectionAndInsertChunk",
            pos: bespin.editor.utils.copyPos(args.pos),
            queued: args.queued,
            selection: args.selection
        };
        var undoOperation = undoArgs;
        this.editor.undoManager.addUndoOperation(new bespin.editor.UndoItem(undoOperation, redoOperation));
    },

    // NOTE: Actually, clipboard.js is taking care of this unless EditorOnly mode is set
    pasteFromClipboard: function(args) {
        var clipboard = (args.clipboard) ? args.clipboard : bespin.util.clipboard.Manual.data();
        if (clipboard == undefined) return; // darn it clipboard!
        args.chunk = clipboard;
        this.insertChunk(args);
    },

    insertChunk: function(args) {
        if (this.editor.selection) {
            this.deleteSelectionAndInsertChunk(args);
        } else {
            var pos = this.editor.model.insertChunk(bespin.editor.utils.copyPos(this.editor.cursorPosition), args.chunk);
            this.editor.moveCursor(pos);
            this.repaint();

            // undo/redo
            args.action = "insertChunk";
            var redoOperation = args;
            var undoArgs = { action: "deleteChunk", pos: bespin.editor.utils.copyPos(args.pos), queued: args.queued, endPos: pos };
            var undoOperation = undoArgs;
            this.editor.undoManager.addUndoOperation(new bespin.editor.UndoItem(undoOperation, redoOperation));

            return pos;
        }
    },

    deleteChunk: function(args) {
        var chunk = this.editor.model.deleteChunk({ startPos: args.pos, endPos: args.endPos });
        this.editor.moveCursor(args.pos);
        this.repaint();

        // undo/redo
        args.action = "deleteChunk";
        var redoOperation = args;
        var undoArgs = { action: "insertChunk", pos: bespin.editor.utils.copyPos(args.pos), queued: args.queued, chunk: chunk };
        var undoOperation = undoArgs;
        this.editor.undoManager.addUndoOperation(new bespin.editor.UndoItem(undoOperation, redoOperation));
    },

    //deleteLine: function(args) {
    //    this.editor.lines.splice(args.pos.row);
    //    if (args.pos.row >= this.editor.lines.length) this.editor.moveCursor({ row: args.pos.row - 1, col: args.pos.col });
    //    this.repaint();
    //},

    joinLine: function(args) {
        if (args.joinDirection == "up") {
            if (args.pos.row == 0) return;

            var newcol = this.editor.model.getRowLength(args.pos.row - 1);
            this.editor.model.joinRow(args.pos.row - 1);
            args.pos.row--;
            args.pos.col = newcol;
        } else {
            if (args.pos.row >= this.editor.model.getRowCount() - 1) return;

            this.editor.model.joinRow(args.pos.row);
        }

        this.editor.refreshMetaData(0);
        this.editor.setCursor(args.pos);
        this.repaint();
    },

    killLine: function(args) {
        // select the current row
        this.editor.setSelection({ startPos: { row: args.pos.row, col: 0 }, endPos: { row: args.pos.row + 1, col: 0 } });
        this.cutSelection(args); // cut (will save and redo will work)
    },
    
    deleteSelection: function(args) {
        if (!this.editor.selection) return;
        var selection = this.editor.getSelection();
        var start = bespin.editor.utils.copyPos(selection.start);
        var chunk = this.editor.model.getChunk(selection);
        this.editor.model.deleteChunk(selection);

/*        // undo/redo
        args.action = "deleteSelection";
        var redoOperation = args;
        var undoArgs = { action: "insertChunkAndSelect", pos: bespin.editor.utils.copyPos(startPos), queued: args.queued, chunk: chunk };
        var undoOperation = undoArgs;
        this.editor.undoManager.addUndoOperation(new bespin.editor.UndoItem(undoOperation, redoOperation));*/

        // setting the selection to undefined has to happen *after* we enqueue the undoOp otherwise replay breaks
        this.editor.setSelection(undefined);
        this.editor.setCursor(start);
        this.repaint();

        return chunk;
    },

    insertChunkAndSelect: function(args) {
        var endPos = this.editor.model.insertChunk(args.pos, args.chunk);

        args.action = "insertChunkAndSelect";
        var redoOperation = args;
        var undoArgs = { action: "deleteSelection", pos: bespin.editor.utils.copyPos(endPos), queued: args.queued };
        var undoOperation = undoArgs;
        this.editor.undoManager.addUndoOperation(new bespin.editor.UndoItem(undoOperation, redoOperation));

        // setting the selection to undefined has to happen *after* we enqueue the undoOp otherwise replay breaks
        this.editor.setSelection({ startPos: args.pos, endPos: endPos });
        this.editor.moveCursor(endPos);
        this.repaint();
    },

    backspace: function(args) {
        if (this.editor.selection) {
            this.deleteSelection(args);
        } else {
            if (args.pos.col > 0) {
                args.pos.col--;
                this.deleteCharacter(args);
                if (this.editor.model.getRowLength(args.pos.row) % this.editor.vll == 0)
                    this.editor.refreshMetaData(0);
                this.editor.setCursor(args.pos);
                this.editor.repaint();
            } else {
                args.joinDirection = "up";
                this.joinLine(args);
            }
        }
    },

    deleteKey: function(args) {
        if (this.editor.selection) {
            this.deleteSelection(args);
        } else {
            if (args.pos.col < this.editor.model.getRowLength(args.pos.row)) {
                this.deleteCharacter(args);
                if (this.editor.model.getRowLength(args.pos.row) % this.editor.vll == 0)
                    this.editor.refreshMetaData(0);
                this.editor.setCursor(args.pos);
                this.editor.repaint();
            } else {
                args.joinDirection = "down";
                this.joinLine(args);
            }
        }
    },

    deleteCharacter: function(args) {
        if (args.pos.col < this.editor.model.getRowLength(args.pos.row)) {
            var deleted = this.editor.model.deleteCharacters(args.pos, 1);
        }
    },

    newline: function(args) {
        var autoindentAmount = 0; // later load this variable from the settings
        var chunk = this.deleteSelection(args);
        args.pos = dojo.clone(this.editor.cursor);
        this.editor.model.splitRow(args.pos, autoindentAmount);
        args.pos.row += 1;
        args.pos.col = autoindentAmount;
        this.editor.refreshMetaData(0);
        this.editor.setSelection(undefined);
        this.editor.setCursor(args.pos);
        this.editor.repaint();
    },

    // it seems kinda silly, but when you have a region selected and you insert a character, I have a separate action that is invoked.
    // this is because it's really two operations: deleting the selected region and then inserting a character. Each of these two
    // actions adds an operation to the undo queue. So I have two choices for
    deleteSelectionAndInsertCharacter: function(args) {
        var oldqueued = args.queued;

        args.queued = true;
        var chunk = this.deleteSelection(args);
        args.pos = dojo.clone(this.editor.cursor);
        this.insertCharacter(args);

/*        args.queued = oldqueued;

        // undo/redo
        args.action = "deleteSelectionAndInsertCharacter";
        var redoOperation = args;
        var undoArgs = {
            action: "deleteCharacterAndInsertChunkAndSelect",
            pos: bespin.editor.utils.copyPos(args.pos),
            queued: args.queued,
            chunk: chunk
        };
        var undoOperation = undoArgs;
        this.editor.undoManager.addUndoOperation(new bespin.editor.UndoItem(undoOperation, redoOperation));*/
    },

    deleteCharacterAndInsertChunkAndSelect: function(args) {
        var oldqueued = args.queued;

        args.queued = true;
        this.deleteCharacter(args);
        this.insertChunkAndSelect(args);

        args.queued = oldqueued;

        // undo/redo
        args.action = "deleteCharacterAndInsertChunkAndSelect";
        var redoOperation = args;
        var undoArgs = { action: "deleteSelectionAndInsertCharacter", pos: bespin.editor.utils.copyPos(args.pos), queued: args.queued };
        var undoOperation = undoArgs;
        this.editor.undoManager.addUndoOperation(new bespin.editor.UndoItem(undoOperation, redoOperation));
    },

    insertCharacter: function(args) {
        if (this.editor.selection) {
            this.deleteSelectionAndInsertCharacter(args);
        } else {
            var nowl_before = Math.floor(this.editor.model.getRowLength(args.pos.row) / this.editor.vll);
            this.editor.model.insertCharacters(args.pos, args.newchar);
            var nowl_after = Math.floor(this.editor.model.getRowLength(args.pos.row) / this.editor.vll);
            args.pos.col++;
            if (nowl_after > nowl_before) {
                this.editor.refreshMetaData(0);
            }
            this.editor.setCursor(args.pos);
            this.editor.repaint();
        }
    },
    
    moveCursorRowToCenter: function(args) {
        var saveCursorRow = this.editor.cursorPosition.row;
        var halfRows = Math.floor(this.editor.ui.visibleRows / 2);
        if (saveCursorRow > (this.editor.ui.firstVisibleRow + halfRows)) { // bottom half, so move down
            this.editor.cursorPosition.row = this.editor.cursorPosition.row + halfRows;
        } else { // top half, so move up
            this.editor.cursorPosition.row = this.editor.cursorPosition.row - halfRows;
        }
        this.editor.ui.ensureCursorVisible();
        this.editor.cursorPosition.row = saveCursorRow;
    },

    repaint: function() {
/*        if (!this.ignoreRepaints) {
            this.editor.ui.ensureCursorVisible();
            this.editor.paint();
        }*/
        this.editor.repaint();
    }
});

dojo.declare("thx.textarea.KeyListener", null, {
    constructor: function(editor, actions) {
        this.editor = editor;
        this.actions = actions;
        this.skipKeypress = false;

        this.defaultKeyMap = {};

        // Allow for multiple key maps to be defined
        this.keyMap = this.defaultKeyMap;
    },

    bindKey: function(keyCode, metaKey, ctrlKey, altKey, shiftKey, action) {
        this.defaultKeyMap[[keyCode, metaKey, ctrlKey, altKey, shiftKey]] = 
            (typeof action == "string") ?
                function() { 
                    var toFire = bespin.events.toFire(action);
                    bespin.publish(toFire.name, toFire.args);
                } : dojo.hitch(this.actions, action);
    },

    bindKeyString: function(modifiers, keyCode, action) {
        var ctrlKey = (modifiers.toUpperCase().indexOf("CTRL") != -1);
        var altKey = (modifiers.toUpperCase().indexOf("ALT") != -1);
        var metaKey = (modifiers.toUpperCase().indexOf("META") != -1) || (modifiers.toUpperCase().indexOf("APPLE") != -1) || (modifiers.toUpperCase().indexOf("CMD") != -1);
        var shiftKey = (modifiers.toUpperCase().indexOf("SHIFT") != -1);
        return this.bindKey(keyCode, metaKey, ctrlKey, altKey, shiftKey, action);
    },
    
    bindKeyStringSelectable: function(modifiers, keyCode, action) {
        this.bindKeyString(modifiers, keyCode, action);
        this.bindKeyString("SHIFT " + modifiers, keyCode, action);
    },

    onkeydown: function(e) {
//        var handled = _commandLine.handleCommandLineFocus(e);
//        if (handled) return false;
//        var args = { event: e, pos: bespin.editor.utils.copyPos(this.editor.cursorPosition) };
        var args = { event: e, pos: dojo.clone(this.editor.cursor) };
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
//        var handled = _commandLine.handleCommandLineFocus(e);
//        if (handled) return false;
        // This is to get around the Firefox bug that happens the first time of jumping between command line and editor
        // Bug https://bugzilla.mozilla.org/show_bug.cgi?id=478686
        if (e.charCode == 'j'.charCodeAt() && e.ctrlKey) {
            dojo.stopEvent(e);
            return false;
        }

        // If key should be skipped, BUT there are some chars like "@|{}[]\" that NEED the ALT- or CTRL-key to be accessable
        // on some platforms and keyboardlayouts (german?). This is not working for "^"
        if ([64 /*@*/, 91/*[*/, 92/*\*/, 93/*]*/, 94/*^*/, 123/*{*/, 124/*|*/, 125/*}*/, 126/*~*/ ].indexOf(e.charCode) != -1) {
            this.skipKeypress = false;
        } else if (this.skipKeypress) {
            if (!bespin.util.keys.passThroughToBrowser(e)) dojo.stopEvent(e);
            return this.returnValue;
        }

//        var args = { event: e, pos: bespin.editor.utils.copyPos(this.editor.cursorPosition) };
        var args = { event: e, pos: dojo.clone(this.editor.cursor) };
        var actions = this.actions;

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

dojo.declare("thx.textarea.DocumentModel", null, {
    type: "Document Model",

    constructor: function() {
        this.rows = [];
    },

    getDirtyRows: function() {
        var dr = (this.dirtyRows) ? this.dirtyRows : [];
        this.dirtyRows = null;
        return dr;
    },

    setRowDirty: function(row) {
        if (!this.dirtyRows) this.dirtyRows = new Array(this.rows.length);
        this.dirtyRows[row] = true;
    },

    setRowClean: function(row) {
        if (!this.dirtyRows) this.dirtyRows = new Array(this.rows.length);
        this.dirtyRows[row] = false;
    },

    isRowDirty: function (row) {
        if (!this.dirtyRows) this.dirtyRows = new Array(this.rows.length);
        return this.dirtyRows[row];
    },

    setRowArray: function(rowIndex, row) {
        if (!dojo.isArray(row)) {
            row = row.split('');
        }
        this.rows[rowIndex] = row;
    },

    // gets the row array for the specified row, creating it and any intermediate rows as necessary
    getRowArray: function(rowIndex) {
        while (this.rows.length <= rowIndex) this.rows.push([]);
//        return this.rows[rowIndex];
        var tabstop = 4; // TODO: get the tabstop parameter from the settings
        var row = this.rows[rowIndex];
        var modified_row = [];
        for (var k = 0; k < row.length; k++) {
            if (row[k].charCodeAt(0) === 9) {
                var toInsert = tabstop - (k % tabstop);
                modified_row[k] = "";
                for (var m = 0; m < toInsert; m++) {
                    modified_row[k] += " ";
                }
            } else {
                modified_row[k] = row[k];
            }
        }
        return modified_row;
    },

    // gets the number of characters in the passed row
    getRowLength: function(rowIndex) {
        var tabstop = 4; // TODO: get the tabstop parameter from the settings
        var length = 0;
        var row = this.rows[rowIndex];
        for (var k = 0; k < row.length; k++) {
            if (row[k].charCodeAt(0) === 9) {
                var toInsert = tabstop - (k % tabstop);
                length += toInsert;
            } else {
                length++;
            }
        }
        return length;
    },

    // checks if there is a row at the specified index; useful because getRowArray() creates rows as necessary
    hasRow: function(rowIndex) {
        return (this.rows[rowIndex]);
    },

    // will insert blank spaces if passed col is past the end of passed row
    insertCharacters: function(pos, string) {
        var row = this.getRowArray(pos.row);
        while (row.length < pos.col) row.push(" ");

        var newrow = (pos.col > 0) ? row.splice(0, pos.col) : [];
        newrow = newrow.concat(string.split(""));
        this.rows[pos.row] = newrow.concat(row);

        this.setRowDirty(pos.row);
    },

    getDocument: function() {
        var file = [];
        for (var x = 0; x < this.getRowCount(); x++) {
            file[x] = this.getRowArray(x).join('');
        }
        return file.join("\n");
    },

    insertDocument: function(content) {
        this.clear();
        var rows = content.split("\n");
        for (var x = 0; x < rows.length; x++) {
            this.insertCharacters({ row: x, col: 0 }, rows[x]);
        }
    },

    changeEachRow: function(changeFunction) {
        for (var x = 0; x < this.getRowCount(); x++) {
            var row = this.getRowArray(x);
            row = changeFunction(row);
            this.setRowArray(x, row);
        }
    },

    replace: function(search, replace) {
      for (var x = 0; x < this.getRowCount(); x++) {
        var line = this.getRowArray(x).join('');

        if (line.match(search)) {
          var regex = new RegExp(search, "g");
          var newline = line.replace(regex, replace);
          if (newline != line) {
            this.rows[x] = newline.split('');
          }
        }
      }
    },

    // will silently adjust the length argument if invalid
    deleteCharacters: function(pos, length) {
        var row = this.getRowArray(pos.row);
        var diff = (pos.col + length - 1) - row.length;
        if (diff > 0) length -= diff;
        if (length > 0) {
            this.setRowDirty(pos.row);
            return row.splice(pos.col, length).join("");
        }
        return "";
    },

    clear: function() {
        this.rows = [];
    },

    deleteRows: function(row, count) {
        var diff = (row + count - 1) - this.rows.length;
        if (diff > 0) count -= diff;
        if (count > 0) this.rows.splice(row, count);
    },

    // splits the passed row at the col specified, putting the right-half on a new line beneath the pased row
    splitRow: function(pos, autoindentAmount) {
        var row = this.getRowArray(pos.row);

        var newRow;
        if (autoindentAmount > 0) {
            newRow = bespin.util.makeArray(autoindentAmount);
        } else {
            newRow = [];
        }

        if (pos.col < row.length) {
            newRow = newRow.concat(row.splice(pos.col));
        }

        if (pos.row == (this.rows.length - 1)) {
            this.rows.push(newRow);
        } else {
            var newRows = this.rows.splice(0, pos.row + 1);
            newRows.push(newRow);
            newRows = newRows.concat(this.rows);
            this.rows = newRows;
        }
    },

    // joins the passed row with the row beneath it
    joinRow: function(rowIndex) {
        if (rowIndex >= this.rows.length - 1) return;
        var row = this.getRowArray(rowIndex);
        this.rows[rowIndex] = row.concat(this.rows[rowIndex + 1]);
        this.rows.splice(rowIndex + 1, 1);
    },

    // returns the maximum number of columns across all rows
    getMaxCols: function() {
        var cols = 0;
        for (var i = 0; i < this.rows.length; i++) cols = Math.max(cols, this.rows[i].length);
        return cols;
    },

    // returns the number of rows in the model
    getRowCount: function() {
        return this.rows.length;
    },

    // returns a "chunk": a string representing a part of the document with \n characters representing end of line
    getChunk: function(selection) {
        var startPos = selection.start;
        var endPos = selection.end;

        var startCol, endCol;
        var chunk = "";

        // get the first line
        startCol = startPos.col;
        var row = this.getRowArray(startPos.row);
        endCol = (endPos.row == startPos.row) ? endPos.col : row.length;
        if (endCol > row.length) endCol = row.length;
        chunk += row.join("").substring(startCol, endCol);

        // get middle lines, if any
        for (var i = startPos.row + 1; i < endPos.row; i++) {
            chunk += "\n";
            chunk += this.getRowArray(i).join("");
        }

        // get the end line
        if (startPos.row != endPos.row) {
            startCol = 0;
            endCol = endPos.col;
            row = this.getRowArray(endPos.row);
            if (endCol > row.length) endCol = row.length;
            chunk += "\n" + row.join("").substring(startCol, endCol);
        }

        return chunk;
    },

    // deletes the text between the startPos and endPos, joining as necessary. startPos and endPos are inclusive
    deleteChunk: function(selection) {
        var chunk = this.getChunk(selection);

        var startPos = selection.start;
        var endPos = selection.end;

        var startCol, endCol;

        // get the first line
        startCol = startPos.col;
        var row = this.getRowArray(startPos.row);
        endCol = (endPos.row == startPos.row) ? endPos.col : row.length;
        if (endCol > row.length) endCol = row.length;
        this.deleteCharacters({ row: startPos.row, col: startCol}, endCol - startCol);

        // get the end line
        if (startPos.row != endPos.row) {
            startCol = 0;
            endCol = endPos.col;
            row = this.getRowArray(endPos.row);
            if (endCol > row.length) endCol = row.length;
            this.deleteCharacters({ row: endPos.row, col: startCol}, endCol - startCol);
        }

        // remove any lines in-between
        if ((endPos.row - startPos.row) > 1) this.deleteRows(startPos.row + 1, endPos.row - startPos.row - 1);

        // join the rows
        if (endPos.row != startPos.row) this.joinRow(startPos.row);

        return chunk;
    },

    // inserts the chunk and returns the ending position
    insertChunk: function(pos, chunk) {
        var lines = chunk.split("\n");
        var cpos = bespin.editor.utils.copyPos(pos);
        for (var i = 0; i < lines.length; i++) {
            this.insertCharacters(cpos, lines[i]);
            cpos.col = cpos.col + lines[i].length;

            if (i < lines.length - 1) {
                this.splitRow(cpos);
                cpos.col = 0;
                cpos.row = cpos.row + 1;
            }
        }
        return cpos;
    },
    
    findBefore: function(row, col, comparator) {
        var line = this.getRowArray(row);
        if (!dojo.isFunction(comparator)) comparator = function(letter) { // default to non alpha
            if (letter.charAt(0) == ' ') return true;
            var letterCode = letter.charCodeAt(0);
            return (letterCode < 48) || (letterCode > 122); // alpha only
        };
        
        while (col > 0) {
            var letter = line[col];
            if (!letter) continue;
            
            if (comparator(letter)) {
                col++; // move it back
                break;
            }
            
            col--;
        }
        
        return { row: row, col: col };
    },

    findAfter: function(row, col, comparator) {
        var line = this.getRowArray(row);
        if (!dojo.isFunction(comparator)) comparator = function(letter) { // default to non alpha
            if (letter.charAt(0) == ' ') return true;
            var letterCode = letter.charCodeAt(0);
            return (letterCode < 48) || (letterCode > 122); // alpha only
        };
        
        while (col < line.length) {
            col++;
            
            var letter = line[col];
            if (!letter) continue;

            if (comparator(letter)) break;
        }
        
        return { row: row, col: col };
    }

});

