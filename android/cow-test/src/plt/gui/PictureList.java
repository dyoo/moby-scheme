package plt.gui;

// Simple linked list.  No abstractions.
public class PictureList {
    public PictureNode first;

    public PictureList() {
	this.first = null;
    }

    public PictureList(PictureNode first) {
	this.first = first;
    }

    public PictureList add(Picture picture, int x, int y) {
	return new PictureList(new PictureNode(picture,
					       x,
					       y,
					       first));
    }

    public PictureList reverse() {
	PictureList result = new PictureList();
	PictureNode node = this.first;
	while (node != null) {
	    result = result.add(node.picture, node.x, node.y);
	    node = node.rest;
	}
	return result;
    }


    public static class PictureNode {
	public Picture picture;
	public int x;
	public int y;
	public PictureNode rest;
	public PictureNode(Picture picture,
			   int x, 
			   int y, 
			   PictureNode rest) {
	    this.picture = picture;
	    this.x = x;
	    this.y = y;
	    this.rest = rest;
	}
    }
}
