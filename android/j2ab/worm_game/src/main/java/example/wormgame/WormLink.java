/*
 *
 * Copyright (c) 2007, Sun Microsystems, Inc.
 *
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions
 * are met:
 *
 *  * Redistributions of source code must retain the above copyright
 *    notice, this list of conditions and the following disclaimer.
 *  * Redistributions in binary form must reproduce the above copyright
 *    notice, this list of conditions and the following disclaimer in the
 *    documentation and/or other materials provided with the distribution.
 *  * Neither the name of Sun Microsystems nor the names of its contributors
 *    may be used to endorse or promote products derived from this software
 *    without specific prior written permission.
 *
 * THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
 * "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
 * LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
 * A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT OWNER OR
 * CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL,
 * EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO,
 * PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR
 * PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF
 * LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING
 * NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
 * SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
 */
/*
 * WormLink.java
 *
 * Created on March 30, 2001, 16:15
 * @version
 */
package example.wormgame;


/**
 * WormLink represents one sub-section of a worm. Because the
 * worm will usually contain a few straight segments, this is
 * a relatively cost effective way to store the entire worm.
 * The [X,Y] coordinates are the "tail" of the worm. The link
 * is drawn starting at the tail and proceeding "len" spaces
 * in direction "dir".
 */
public class WormLink {
    private int x;
    private int y;
    private int len;
    private byte dir;

    private WormLink() {
    }

    public WormLink(int startX, int startY, int length, byte direction) {
        x = startX;
        y = startY;
        dir = direction;
        len = length - 1;
    }

    /**
     * Create a worm link with a length of 1. This constructor is
     * used when the worm changes direction.
     */
    public WormLink(int startX, int startY, byte direction) {
        this(startX, startY, 1, direction);
    }

    /**
     * Add one cell length to the head of this segment.
     */
    public void increaseLength() {
        len++;
    }

    /**
     * Remove one cell length from the tail of this segment.
     */
    public void decreaseLength() {
        len--;

        switch (dir) {
        case Worm.LEFT:
            x--;

            break;

        case Worm.RIGHT:
            x++;

            break;

        case Worm.UP:
            y--;

            break;

        case Worm.DOWN:
            y++;

            break;
        }
    }

    /**
     * Get the length, in cells, of this segment.
     */
    public int getLength() {
        return len + 1;
    }

    /**
     * Get the X coordinate of the cell that contains the head of this
     * worm segment.
     */
    public int getX() {
        return x;
    }

    /**
     * Get the Y coordinate of the cell that contains the head of this
     * worm segment.
     */
    public int getY() {
        return y;
    }

    /**
     * Get the X coordinate of the cell that contains the tail of this
     * worm segment.
     */
    public int getEndX() {
        if (dir == Worm.LEFT) {
            return x - len;
        }

        if (dir == Worm.RIGHT) {
            return x + len;
        }

        return x;
    }

    /**
     * Get the Y coordinate of the cell that contains the tail of this
     * worm segment.
     */
    public int getEndY() {
        if (dir == Worm.DOWN) {
            return y + len;
        }

        if (dir == Worm.UP) {
            return y - len;
        }

        return y;
    }

    /**
     * Get the direction this worm segment is pointing.
     */
    public byte getDirection() {
        return dir;
    }

    /**
     * Returns true if the worm segment is at the given cell
     */
    public boolean contains(int x, int y) {
        switch (dir) {
        case Worm.LEFT:
            return ((y == this.y) && ((x <= this.x) && (x >= getEndX())));

        case Worm.RIGHT:
            return ((y == this.y) && ((x >= this.x) && (x <= getEndX())));

        case Worm.UP:
            return ((x == this.x) && ((y <= this.y) && (y >= getEndY())));

        case Worm.DOWN:
            return ((x == this.x) && ((y >= this.y) && (y <= getEndY())));
        }

        return false;
    }

    /**
     * Debug method.
     */
    public String toString() {
        String dirString;

        switch (dir) {
        case Worm.LEFT:
            dirString = "Left";

            break;

        case Worm.RIGHT:
            dirString = "Right";

            break;

        case Worm.UP:
            dirString = "Up";

            break;

        case Worm.DOWN:
            dirString = "Down";

            break;

        default:
            dirString = "UNKNOWN -- " + dir;
        }

        return " pos == [" + x + "," + y + "]" + " - [" + getEndX() + "," + getEndY() + "]" +
        "   len == " + getLength() + "   dir == " + dirString;
    }
}
