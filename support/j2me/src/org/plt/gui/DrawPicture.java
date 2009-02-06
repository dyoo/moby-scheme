package org.plt.gui;

public class DrawPicture implements PictureVisitor {
    public DrawPicture(Graphics g) {
	this.g = g;
    }


    public void visit(Scene s, int x, int y) {
	// empty scene is pure white.
	g.setColor(255, 255, 255);
	// Fixme: it looks like we're ignoring x and y here, but
	// we really should be paying attention to it.
	g.fillRect(0, 0, s.getWidth(), s.getHeight());	
	PictureList.PictureNode node = s.getPictureList().reverse().first;
	while(node != null) {
	    node.picture.accept(this, node.x, node.y);
	    node = node.rest;
	}
    }

    public void visit(RectanglePicture r, int x, int y) {
	g.setColor(r.getColor().r(), r.getColor().g(), r.getColor().b());
	if (r.getStyle().toUpperCase().equals("SOLID")) {
	    g.fillRect(x - r.getPinholeX(), 
		       y - r.getPinholeY(), width, height);
	} else {
	    g.drawRect(x - r.getPinholeX(), 
		       y - r.getPinholeY(),
		       r.getWidth(),
		       r.getHeight());
	}
    }

    public void visit(FilePicture f, int x, int y) {
	g.drawImage(f.getImage(),
		    x - this.getPinholeX(),
		    y - this.getPinholeY());

    }

    public void visit(CirclePicture c, int x, int y) {
	g.setColor(c.getColor().r(), c.getColor().g(), c.getColor().b());
	if (c.getStyle().toUpperCase().equals("SOLID")) {
	    g.fillArc(x - c.getPinholeX(),
		      y - c.getPinholeY(),
		      2 * c.getRadius(),
		      2 * c.getRadius(),
		      0,
		      360);
	} else {
	    g.drawArc(x - c.getPinholeX(),
		      y - c.getPinholeY(),
		      2 * c.getRadius(),
		      2 * c.getRadius(),
		      0,
		      360);
	}
    }

    public void visit(TextPicture t, int x, int y) {
	g.setFont(t.getFont());
	g.setColor(t.getColor().r(), t.getColor().g(), t.getColor().b());
	g.drawString(t.getText(),
		     x - t.getPinholeX(), 
		     y - t.getPinholeY()); 
    }
}
