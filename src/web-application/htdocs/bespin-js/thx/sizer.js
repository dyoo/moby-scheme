dojo.provide("thx.sizer");

dojo.declare("th.components.Sizer", th.Container, {
    HORIZONTAL: "horizontal",
    VERTICAL: "vertical",

    LEFT: 0X01,
    RIGHT: 0X02,
    TOP: 0X04,
    BOTTOM: 0X08,
    CENTER: 0X10,
    constructor: function (direction) {
        // The sizer has two dimensions - a primary dimension along its direction, and a secondary dimension perpendicular to the direction
        this.direction = direction || this.HORIZONTAL;
        this.children = []; // an array of child objects. this is separated from this.layoutdata to maintain compatibility to the container methods
        this.layoutdata = []; // an array of objects of the form {cell: {x, y, w, h}, alignment, inflating}
    },

    add: function (child, alignment, inflating) {
        // I can only add a children with which the splitter can interact
        if (child.getPreferredSize === undefined || child.bounds === undefined) {
            return;
        }
        // TODO: add validation of the parameters
        child.parent = this;
        this.children.push(child);
        this.layoutdata.push({
            cell: {x: 0, y: 0, w: 0, h: 0},
            alignment: alignment,
            inflating: inflating
        });
    },

    getPreferredSize: function () {
        var width = 0;
        var height = 0;
        var childSize;
        if (this.direction === this.HORIZONTAL) {
            for (var k = 0; k < this.children.length;  k++) {
                childSize = this.children[k].getPreferredSize();
                width += childSize.w; // accumulate along the primary dimension
                height = Math.max(height, childSize.h); // fit according to maximum in the secondary dimension
            }
        } else if (this.direction === this.VERTICAL) {
            for (var k = 0; k < this.children.length;  k++) {
                childSize = this.children[k].getPreferredSize();
                height += childSize.h;
                width = Math.max(width, childSize.w);
            }
        }
        return {w: width, h: height};
    },

    layout: function () {
        var d = this.d();
        var size = this.getPreferredSize();
        var childSize;
        var extra;
        var residue;
        var k;
        if (this.direction === this.HORIZONTAL) {
            extra = Math.floor((this.bounds.width - size.w) / this.children.length);
            residue = (this.bounds.width - size.w) % this.children.length;
            // I assume here that the extra is positive. TODO: figure out what to do with negative extra
            if (extra < 0) {
                alert("Error: Unhandled situation: the children are bigger than the sizer");
                return;
            }
            // Allocate the cell dimensions
            childSize = this.children[0].getPreferredSize();
            this.layoutdata[0].cell.w = childSize.w + extra + ((0 < residue) ? 1 : 0);
            this.layoutdata[0].cell.h = Math.max(this.bounds.height, size.h);
            this.layoutdata[0].cell.x = this.bounds.x;
            this.layoutdata[0].cell.y = this.bounds.y;
            for (var k = 1; k < this.children.length; k++) {
                childSize = this.children[k].getPreferredSize();
                this.layoutdata[k].cell.w = childSize.w + extra + ((k < residue) ? 1 : 0);
                this.layoutdata[k].cell.h = Math.max(this.bounds.height, size.h);
                this.layoutdata[k].cell.x = this.layoutdata[k - 1].cell.x + this.layoutdata[k - 1].cell.w;
                this.layoutdata[k].cell.y = this.bounds.y;
            }
        } else if (this.direction === this.VERTICAL) {
            extra = Math.floor((this.bounds.height - size.h) / this.children.length);
            residue = (this.bounds.height - size.h) % this.children.length;
            // I assume here that the extra is positive. TODO: figure out what to do with negative extra
            if (extra < 0) {
                alert("Error: Unhandled situation: the children are bigger than the sizer");
                return;
            }
            // Allocate the cell dimensions
            childSize = this.children[0].getPreferredSize();
            this.layoutdata[0].cell.w = Math.max(this.bounds.width, size.w);
            this.layoutdata[0].cell.h = childSize.h + extra + ((0 < residue) ? 1 : 0);
            this.layoutdata[0].cell.x = this.bounds.x;
            this.layoutdata[0].cell.y = this.bounds.y;
            for (var k = 1; k < this.children.length; k++) {
                childSize = this.children[k].getPreferredSize();
                this.layoutdata[k].cell.w = Math.max(this.bounds.width, size.w);
                this.layoutdata[k].cell.h = childSize.h + extra + ((k < residue) ? 1 : 0);
                this.layoutdata[k].cell.x = this.bounds.x;
                this.layoutdata[k].cell.y = this.layoutdata[k - 1].cell.y + this.layoutdata[k - 1].cell.h;
            }
        }
        // Set the dimension and position of the children inside the cells according to their parameters
        for (k = 0; k < this.children.length; k++) {
            var x, y, w, h;
            var cell = this.layoutdata[k].cell;
            var child = this.children[k].getPreferredSize();
            if (this.layoutdata[k].inflating === true) {
                x = cell.x;
                y = cell.y;
                w = cell.w;
                h = cell.h;
            } else {
                w = child.w;
                h = child.h;
                if ((this.layoutdata[k].alignment & this.CENTER) != 0) {
                    x = cell.x + Math.floor((cell.w - child.w) / 2);
                    y = cell.y + Math.floor((cell.h - child.h) / 2);
                } else {
                    if ((this.layoutdata[k].alignment & this.TOP) != 0) {
                        y = cell.y;
                    } else if ((this.layoutdata[k].alignment & this.BOTTOM) != 0) {
                        y = cell.y + cell.h - child.h;
                    }
                    if ((this.layoutdata[k].alignment & this.LEFT) != 0) {
                        x = cell.x;
                    } else if ((this.layoutdata[k].alignment & this.RIGHT) != 0) {
                        x = cell.x + cell.w - child.w;
                    }
                }
            }
            this.children[k].bounds.x = x;
            this.children[k].bounds.y = y;
            this.children[k].bounds.width = w;
            this.children[k].bounds.height = h;
        }
    },

    paintSelf: function (ctx) {
        // for debug purposes, draw the border of the cells
        var cell;
        ctx.save();
        ctx.color = "white";
        for (var k = 0; k < this.layoutdata.length; k++) {
            cell = this.layoutdata[k].cell;
            console.debug(k, cell);
            ctx.strokeRect(cell.x, cell.y, cell.w, cell.h);
        }
        ctx.restore();
    }
});

