package org.plt.gui;
import javax.microedition.lcdui.*;

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
    public int getWidth() {
	return 2*r;
    }

    public int getHeight() {
	return 2*r;
    }

    public void draw(Graphics g, int x, int y) {
	g.setColor(color.r(), color.g(), color.b());
	if (style.toUpperCase().equals("SOLID")) {
	    g.fillArc(x - this.getPinholeX(),
		      y - this.getPinholeY(),
		      2*r,
		      2*r,
		      0,
		      360);
	} else {
	    g.drawArc(x - this.getPinholeX(),
		      y - this.getPinholeY(),
		      2*r,
		      2*r,
		      0,
		      360);
	}
    }

    public void accept(PictureVisitor visitor, int x, int y) {
	visitor.visit(this, x, y);
    }
}
