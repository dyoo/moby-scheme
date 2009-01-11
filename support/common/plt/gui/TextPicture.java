package plt.gui;
import javax.microedition.lcdui.*;

public class TextPicture extends BasicPicture {
    String text;
    Color color;
    Font font;

    public TextPicture(String text, int pointSize, Color color) {
	this.text = text;
	this.color = color;
	this.font = Font.getFont(Font.FACE_MONOSPACE,
				 Font.STYLE_PLAIN,
				 closestFontSize(pointSize));
    }

    static int closestFontSize(int pointSize) {
	if (pointSize <= Font.SIZE_SMALL) {
	    return Font.SIZE_SMALL;
	} else if (pointSize <= Font.SIZE_MEDIUM) {
	    return Font.SIZE_MEDIUM;
	} else {
	    return Font.SIZE_LARGE;
	}
    }

    public int getWidth() {
	return this.font.stringWidth(this.text);
    }

    public int getHeight() {
	return this.font.getHeight();
    }

    public void draw(Graphics g, int x, int y) {
	g.setFont(this.font);
	g.setColor(this.color.getRGB());
	g.drawString(this.text,
		     x - this.getPinholeX(), 
		     y - this.getPinholeY(),
		     g.TOP | g.LEFT); 
    }	


    public void accept(PictureVisitor visitor, int x, int y) {
	visitor.visit(this, x, y);
    }
}
