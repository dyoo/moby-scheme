package org.plt.gui;

public class CirclePicture extends BasicPicture {
    int r;
    String style;
    Color color;
    public CirclePicture(int r, String style, Color color) {
	setPinhole(r, r);
	this.r = r;
	this.style = style;
	this.color = color;

    }
    public String getStyle() {
	return this.style;
    }

    public Color getColor() {
	return this.color;
    }

    public int getRadius() {
	return this.r;
    }

    public int getWidth() {
	return 2*r;
    }

    public int getHeight() {
	return 2*r;
    }

    public void accept(PictureVisitor visitor, int x, int y) {
	visitor.visit(this, x, y);
    }
}
