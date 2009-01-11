package plt.gui;

public class Color {
    private int _r, _g, _b;
    public Color(int r, int g, int b) {
	this._r = r;
	this._g = g;
	this._b = b;
    }
    public int r() { return this._r; } 
    public int g() { return this._g; }
    public int b() { return this._b; }


    public int getRGB() {
	return (_r << (8*2)) | (_g << (8*1)) | _b;
    }

    public static Color BLACK = new Color(0, 0, 0);   
    public static Color WHITE = new Color(255, 255, 255);
    public static Color RED = new Color(255, 0, 0);
    public static Color GREEN = new Color(0, 255, 0);
    public static Color BLUE = new Color(0, 0, 255);
    // fixme: we need a color database!

    public static Color lookup(String name) {
	if (name.toUpperCase().equals("BLACK")) {
	    return BLACK;
	}
	if (name.toUpperCase().equals("WHITE")) {
	    return WHITE;
	}
	if (name.toUpperCase().equals("RED")) {
	    return RED;
	}
	if (name.toUpperCase().equals("GREEN")) {
	    return GREEN;
	}
	if (name.toUpperCase().equals("BLUE")) {
	    return BLUE;
	}
	return BLACK;
    }
}
