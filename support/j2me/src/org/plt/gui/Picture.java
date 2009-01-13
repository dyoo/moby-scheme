package org.plt.gui;


public interface Picture {
    public int getPinholeX();
    public int getPinholeY();
    public void setPinhole(int x, int y);
    public int getWidth();
    public int getHeight();
    public void draw(Graphics g, int x, int y);

    public void accept(PictureVisitor visitor, int x, int y);
}
