package org.plt.gui;


public class Scene implements Picture {
    int width;
    int height;

    PictureList pictures;

    private Scene(int width, int height) {
	this.width = width;
	this.height = height;
	this.pictures = new PictureList();
    }
    
    private Scene(int width, int height, PictureList pictures) {
	this.width = width;
	this.height = height;
	this.pictures = pictures;
    }


    public static Scene emptyScene(int width, int height) {
	return new Scene(width, height);
    }

    public Scene placeImage(Picture pic, int x, int y) {
	return new Scene(this.width, this.height, 
			 this.pictures.add(pic, x, y));
    }

    public int getPinholeX() { return 0; }

    public int getPinholeY() { return 0; }

    public void setPinhole(int x, int y) {
	if (x != 0 || y != 0) {
	    throw new RuntimeException
		("Scene pinhole must be (0, 0)");
	}
    }

    public int getWidth() { return this.width; }

    public int getHeight() { return this.height; }

    public PictureList getPictures() {
	return this.pictures;
    }


    public void accept(PictureVisitor visitor, int x, int y) {
	visitor.visit(this, x, y);
    }
}
