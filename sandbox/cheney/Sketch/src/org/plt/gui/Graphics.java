package org.plt.gui;


public interface Graphics {
    void setColor(int r, int g, int b);
    // fixme: we shouldn't depend on lcdui.Font here.
    void setFont(javax.microedition.lcdui.Font font);

    void fillRect(int x, int y, int width, int height);
    void drawRect(int x, int y, int width, int height);
    // fixme: we shouldn't depend on lcdui.Image here.
    void drawImage(javax.microedition.lcdui.Image image, int x, int y);
    void fillArc(int x, int y, int width, int height, int startAngle, int arcAngle);
    void drawArc(int x, int y, int width, int height, int startAngle, int arcAngle);
    void drawString(String str, int x, int y);	
}
