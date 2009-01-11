package org.plt.gui;

public abstract class BasicPicture implements Picture {
    int pinholeX;
    int pinholeY;
    public int getPinholeX() { 
	return pinholeX;
    }

    public int getPinholeY() {
	return pinholeY;
    }

    public void setPinhole(int x, int y) {
	this.pinholeX = x;
	this.pinholeY = y;
    }
}
