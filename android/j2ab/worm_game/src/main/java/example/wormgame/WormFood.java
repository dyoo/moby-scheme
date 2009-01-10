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
 * WormFood.java
 *
 * Created on March 30, 2001, 16:15
 * @version
 */
package example.wormgame;

import javax.microedition.lcdui.Graphics;


/**
 * WormFood is a food item that the worm can eat. It is placed at
 * a pseudo randomly generated location in the pit.
 */
public class WormFood {
    /** Current food location x coordinate in cells. */
    private int cellX;

    /** Current food location y coordinate in cells. */
    private int cellY;

    /*
     * These members are for convenience only. They are generated using
     * expensive multiplication operations and are cached for quick access
     */

    /** Current food location x coordinate in pixels. */
    private int x;

    /** Current food location y coordinate in pixels. */
    private int y;

    /** Size of arena in cells. */
    private long arenaSize;

    /**
     * Constructor for worm food object.
     * @param pit pit to which the food is associated. (unused currently)
     */
    public WormFood(WormPit pit) {
        arenaSize = WormPit.CellWidth * WormPit.CellHeight;
        regenerate();
    }

    /**
     * Returns true if the food item is at the given cell.
     * @param cx x coordinate of cell to test
     * @param cy y coordinate of cell to test
     * @return true, if the food is at the cell coordinate
     */
    public boolean isAt(int cx, int cy) {
        return ((this.cellX == cx) && (this.cellY == cy));
    }

    /**
     * Get the X coordinate of the cell that contains the food item.
     * @return x coordinate of food cell location
     */
    public int getX() {
        return cellX;
    }

    /**
     * Get the Y coordinate of the cell that contains the food item.
     * @return y coordinate of food cell location
     */
    public int getY() {
        return cellY;
    }

    /**
     * Regenerate the food item. Whenever the worm eats a piece of
     * food, this method is called.
     */
    public void regenerate() {
        int loc = (int)(System.currentTimeMillis() % arenaSize);
        cellY = (int)(loc / WormPit.CellWidth);
        cellX = (int)(loc % WormPit.CellHeight);
        y = cellY * WormPit.CELL_SIZE; // Cache to save time during paint
        x = cellX * WormPit.CELL_SIZE; // Cache to save time during paint
                                       // If food was generated outside of the board, try a new location

        if (!WormPit.isInBounds(cellX, cellY)) {
            regenerate();
        }
    }

    /**
     * Paint the piece of food.
     * @param g graphics object to receive rendering of food object
     */
    public void paint(Graphics g) {
        g.setColor(WormPit.FOOD_COLOUR);
        g.fillRect(x + 1, y + 1, WormPit.CELL_SIZE - 2, WormPit.CELL_SIZE - 2);
        g.setColor(WormPit.DRAW_COLOUR);
        g.drawRect(x, y, WormPit.CELL_SIZE - 1, WormPit.CELL_SIZE - 1);
    }
}
