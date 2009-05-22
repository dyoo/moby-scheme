package org.plt.gui;

public class GraphicsAdapter implements Graphics {
    private javax.microedition.lcdui.Graphics g;

    public GraphicsAdapter(javax.microedition.lcdui.Graphics g) {
	this.g = g;
    }

    public void setColor(int red, int green, int blue) {
	g.setColor(red, green, blue);
    }

    public void setFont(javax.microedition.lcdui.Font f) {
	g.setFont(f);
    }


    public void fillRect(int x, int y, int width, int height) {
	g.fillRect(x, y, width, height);
    }

    public void drawRect(int x, int y, int width, int height) {
	g.drawRect(x, y, width, height);
    }

    public void drawImage(javax.microedition.lcdui.Image image, int x, int y) {
	g.drawImage(image, x, y, g.TOP | g.LEFT);
    }

    public void fillArc(int x, int y, int width, int height, int startAngle, int arcAngle) {
	g.fillArc(x, y, width, height, startAngle, arcAngle);
    }

    public void drawArc(int x, int y, int width, int height, int startAngle, int arcAngle) {
	g.drawArc(x, y, width, height, startAngle, arcAngle);
    }

    public void drawString(String str, int x, int y) {
	g.drawString(str, x, y, g.TOP | g.LEFT);
    }

}
