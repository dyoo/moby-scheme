package org.plt.gui;

public class FilePicture extends BasicPicture {
    javax.microedition.lcdui.Image img;
    public FilePicture(String filename) {
	try {
	    this.img = javax.microedition.lcdui.Image.createImage(filename);
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

    public javax.microedition.lcdui.Image getImage() {
	return this.img;
    }

    public void accept(PictureVisitor visitor, int x, int y) {
	visitor.visit(this, x, y);
    }

}
