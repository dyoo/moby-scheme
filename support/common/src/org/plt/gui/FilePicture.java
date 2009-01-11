package org.plt.gui;
import javax.microedition.lcdui.*;

public class FilePicture extends BasicPicture {
    Image img;
    public FilePicture(String filename) {
	try {
	    this.img = Image.createImage(filename);
	} catch (java.io.IOException e) {
	    throw new RuntimeException("Can't find image " + filename);
	}
	setPinhole(img.getWidth() / 2, 
		   img.getHeight() / 2);
    }

    public int getWidth() {
	return img.getWidth();
    }

    public int getHeight() {
	return img.getHeight();
    }

    public void draw(Graphics g, int x, int y) {
	g.drawImage(img,
		    x - this.getPinholeX(),
		    y - this.getPinholeY(),
		    g.TOP | g.LEFT);
    }	

    public void accept(PictureVisitor visitor, int x, int y) {
	visitor.visit(this, x, y);
    }

}
