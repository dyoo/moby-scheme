package org.plt.gui;
import java.util.Hashtable;

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


    static Color BLACK = new Color(0, 0, 0);

    //////////////////////////////////////////////////////////////////////
    // This chunk of code was generated by src/dump-color-database.ss.
    static Hashtable db = new Hashtable();
    static { 
	db.put("ORANGE", new Color(255, 165, 0));
	db.put("RED", new Color(255, 0, 0));
	db.put("ORANGERED", new Color(255, 69, 0));
	db.put("TOMATO", new Color(255, 99, 71));
	db.put("DARKRED", new Color(139, 0, 0));
	db.put("RED", new Color(255, 0, 0));
	db.put("FIREBRICK", new Color(178, 34, 34));
	db.put("CRIMSON", new Color(220, 20, 60));
	db.put("DEEPPINK", new Color(255, 20, 147));
	db.put("MAROON", new Color(176, 48, 96));
	db.put("INDIAN RED", new Color(205, 92, 92));
	db.put("INDIANRED", new Color(205, 92, 92));
	db.put("MEDIUM VIOLET RED", new Color(199, 21, 133));
	db.put("MEDIUMVIOLETRED", new Color(199, 21, 133));
	db.put("VIOLET RED", new Color(208, 32, 144));
	db.put("VIOLETRED", new Color(208, 32, 144));
	db.put("LIGHTCORAL", new Color(240, 128, 128));
	db.put("HOTPINK", new Color(255, 105, 180));
	db.put("PALEVIOLETRED", new Color(219, 112, 147));
	db.put("LIGHTPINK", new Color(255, 182, 193));
	db.put("ROSYBROWN", new Color(188, 143, 143));
	db.put("PINK", new Color(255, 192, 203));
	db.put("ORCHID", new Color(218, 112, 214));
	db.put("LAVENDERBLUSH", new Color(255, 240, 245));
	db.put("SNOW", new Color(255, 250, 250));
	db.put("CHOCOLATE", new Color(210, 105, 30));
	db.put("SADDLEBROWN", new Color(139, 69, 19));
	db.put("BROWN", new Color(132, 60, 36));
	db.put("DARKORANGE", new Color(255, 140, 0));
	db.put("CORAL", new Color(255, 127, 80));
	db.put("SIENNA", new Color(160, 82, 45));
	db.put("ORANGE", new Color(255, 165, 0));
	db.put("SALMON", new Color(250, 128, 114));
	db.put("PERU", new Color(205, 133, 63));
	db.put("DARKGOLDENROD", new Color(184, 134, 11));
	db.put("GOLDENROD", new Color(218, 165, 32));
	db.put("SANDYBROWN", new Color(244, 164, 96));
	db.put("LIGHTSALMON", new Color(255, 160, 122));
	db.put("DARKSALMON", new Color(233, 150, 122));
	db.put("GOLD", new Color(255, 215, 0));
	db.put("YELLOW", new Color(255, 255, 0));
	db.put("OLIVE", new Color(128, 128, 0));
	db.put("BURLYWOOD", new Color(222, 184, 135));
	db.put("TAN", new Color(210, 180, 140));
	db.put("NAVAJOWHITE", new Color(255, 222, 173));
	db.put("PEACHPUFF", new Color(255, 218, 185));
	db.put("KHAKI", new Color(240, 230, 140));
	db.put("DARKKHAKI", new Color(189, 183, 107));
	db.put("MOCCASIN", new Color(255, 228, 181));
	db.put("WHEAT", new Color(245, 222, 179));
	db.put("BISQUE", new Color(255, 228, 196));
	db.put("PALEGOLDENROD", new Color(238, 232, 170));
	db.put("BLANCHEDALMOND", new Color(255, 235, 205));
	db.put("MEDIUM GOLDENROD", new Color(234, 234, 173));
	db.put("MEDIUMGOLDENROD", new Color(234, 234, 173));
	db.put("PAPAYAWHIP", new Color(255, 239, 213));
	db.put("MISTYROSE", new Color(255, 228, 225));
	db.put("LEMONCHIFFON", new Color(255, 250, 205));
	db.put("ANTIQUEWHITE", new Color(250, 235, 215));
	db.put("CORNSILK", new Color(255, 248, 220));
	db.put("LIGHTGOLDENRODYELLOW", new Color(250, 250, 210));
	db.put("OLDLACE", new Color(253, 245, 230));
	db.put("LINEN", new Color(250, 240, 230));
	db.put("LIGHTYELLOW", new Color(255, 255, 224));
	db.put("SEASHELL", new Color(255, 245, 238));
	db.put("BEIGE", new Color(245, 245, 220));
	db.put("FLORALWHITE", new Color(255, 250, 240));
	db.put("IVORY", new Color(255, 255, 240));
	db.put("GREEN", new Color(0, 255, 0));
	db.put("LAWNGREEN", new Color(124, 252, 0));
	db.put("CHARTREUSE", new Color(127, 255, 0));
	db.put("GREEN YELLOW", new Color(173, 255, 47));
	db.put("GREENYELLOW", new Color(173, 255, 47));
	db.put("YELLOW GREEN", new Color(154, 205, 50));
	db.put("YELLOWGREEN", new Color(154, 205, 50));
	db.put("MEDIUM FOREST GREEN", new Color(107, 142, 35));
	db.put("OLIVEDRAB", new Color(107, 142, 35));
	db.put("MEDIUMFORESTGREEN", new Color(107, 142, 35));
	db.put("DARK OLIVE GREEN", new Color(85, 107, 47));
	db.put("DARKOLIVEGREEN", new Color(85, 107, 47));
	db.put("DARKSEAGREEN", new Color(143, 188, 139));
	db.put("LIME", new Color(0, 255, 0));
	db.put("DARK GREEN", new Color(0, 100, 0));
	db.put("DARKGREEN", new Color(0, 100, 0));
	db.put("LIME GREEN", new Color(50, 205, 50));
	db.put("LIMEGREEN", new Color(50, 205, 50));
	db.put("FOREST GREEN", new Color(34, 139, 34));
	db.put("FORESTGREEN", new Color(34, 139, 34));
	db.put("SPRING GREEN", new Color(0, 255, 127));
	db.put("SPRINGGREEN", new Color(0, 255, 127));
	db.put("MEDIUM SPRING GREEN", new Color(0, 250, 154));
	db.put("MEDIUMSPRINGGREEN", new Color(0, 250, 154));
	db.put("SEA GREEN", new Color(46, 139, 87));
	db.put("SEAGREEN", new Color(46, 139, 87));
	db.put("MEDIUM SEA GREEN", new Color(60, 179, 113));
	db.put("MEDIUMSEAGREEN", new Color(60, 179, 113));
	db.put("AQUAMARINE", new Color(112, 216, 144));
	db.put("LIGHTGREEN", new Color(144, 238, 144));
	db.put("PALE GREEN", new Color(152, 251, 152));
	db.put("PALEGREEN", new Color(152, 251, 152));
	db.put("MEDIUM AQUAMARINE", new Color(102, 205, 170));
	db.put("MEDIUMAQUAMARINE", new Color(102, 205, 170));
	db.put("TURQUOISE", new Color(64, 224, 208));
	db.put("LIGHTSEAGREEN", new Color(32, 178, 170));
	db.put("MEDIUM TURQUOISE", new Color(72, 209, 204));
	db.put("MEDIUMTURQUOISE", new Color(72, 209, 204));
	db.put("HONEYDEW", new Color(240, 255, 240));
	db.put("MINTCREAM", new Color(245, 255, 250));
	db.put("ROYALBLUE", new Color(65, 105, 225));
	db.put("DODGERBLUE", new Color(30, 144, 255));
	db.put("DEEPSKYBLUE", new Color(0, 191, 255));
	db.put("CORNFLOWERBLUE", new Color(100, 149, 237));
	db.put("STEEL BLUE", new Color(70, 130, 180));
	db.put("STEELBLUE", new Color(70, 130, 180));
	db.put("LIGHTSKYBLUE", new Color(135, 206, 250));
	db.put("DARK TURQUOISE", new Color(0, 206, 209));
	db.put("DARKTURQUOISE", new Color(0, 206, 209));
	db.put("CYAN", new Color(0, 255, 255));
	db.put("AQUA", new Color(0, 255, 255));
	db.put("DARKCYAN", new Color(0, 139, 139));
	db.put("TEAL", new Color(0, 128, 128));
	db.put("SKY BLUE", new Color(135, 206, 235));
	db.put("SKYBLUE", new Color(135, 206, 235));
	db.put("CADET BLUE", new Color(96, 160, 160));
	db.put("CADETBLUE", new Color(95, 158, 160));
	db.put("DARK SLATE GRAY", new Color(47, 79, 79));
	db.put("DARKSLATEGRAY", new Color(47, 79, 79));
	db.put("LIGHTSLATEGRAY", new Color(119, 136, 153));
	db.put("SLATEGRAY", new Color(112, 128, 144));
	db.put("LIGHT STEEL BLUE", new Color(176, 196, 222));
	db.put("LIGHTSTEELBLUE", new Color(176, 196, 222));
	db.put("LIGHT BLUE", new Color(173, 216, 230));
	db.put("LIGHTBLUE", new Color(173, 216, 230));
	db.put("POWDERBLUE", new Color(176, 224, 230));
	db.put("PALETURQUOISE", new Color(175, 238, 238));
	db.put("LIGHTCYAN", new Color(224, 255, 255));
	db.put("ALICEBLUE", new Color(240, 248, 255));
	db.put("AZURE", new Color(240, 255, 255));
	db.put("MEDIUM BLUE", new Color(0, 0, 205));
	db.put("MEDIUMBLUE", new Color(0, 0, 205));
	db.put("DARKBLUE", new Color(0, 0, 139));
	db.put("MIDNIGHT BLUE", new Color(25, 25, 112));
	db.put("MIDNIGHTBLUE", new Color(25, 25, 112));
	db.put("NAVY", new Color(36, 36, 140));
	db.put("BLUE", new Color(0, 0, 255));
	db.put("INDIGO", new Color(75, 0, 130));
	db.put("BLUE VIOLET", new Color(138, 43, 226));
	db.put("BLUEVIOLET", new Color(138, 43, 226));
	db.put("MEDIUM SLATE BLUE", new Color(123, 104, 238));
	db.put("MEDIUMSLATEBLUE", new Color(123, 104, 238));
	db.put("SLATE BLUE", new Color(106, 90, 205));
	db.put("SLATEBLUE", new Color(106, 90, 205));
	db.put("PURPLE", new Color(160, 32, 240));
	db.put("DARK SLATE BLUE", new Color(72, 61, 139));
	db.put("DARKSLATEBLUE", new Color(72, 61, 139));
	db.put("DARKVIOLET", new Color(148, 0, 211));
	db.put("DARK ORCHID", new Color(153, 50, 204));
	db.put("DARKORCHID", new Color(153, 50, 204));
	db.put("MEDIUMPURPLE", new Color(147, 112, 219));
	db.put("CORNFLOWER BLUE", new Color(68, 64, 108));
	db.put("MEDIUM ORCHID", new Color(186, 85, 211));
	db.put("MEDIUMORCHID", new Color(186, 85, 211));
	db.put("MAGENTA", new Color(255, 0, 255));
	db.put("FUCHSIA", new Color(255, 0, 255));
	db.put("DARKMAGENTA", new Color(139, 0, 139));
	db.put("VIOLET", new Color(238, 130, 238));
	db.put("PLUM", new Color(221, 160, 221));
	db.put("LAVENDER", new Color(230, 230, 250));
	db.put("THISTLE", new Color(216, 191, 216));
	db.put("GHOSTWHITE", new Color(248, 248, 255));
	db.put("WHITE", new Color(255, 255, 255));
	db.put("WHITESMOKE", new Color(245, 245, 245));
	db.put("GAINSBORO", new Color(220, 220, 220));
	db.put("LIGHT GRAY", new Color(211, 211, 211));
	db.put("LIGHTGRAY", new Color(211, 211, 211));
	db.put("SILVER", new Color(192, 192, 192));
	db.put("GRAY", new Color(190, 190, 190));
	db.put("DARK GRAY", new Color(169, 169, 169));
	db.put("DARKGRAY", new Color(169, 169, 169));
	db.put("DIM GRAY", new Color(105, 105, 105));
	db.put("DIMGRAY", new Color(105, 105, 105));
	db.put("BLACK", new Color(0, 0, 0));
    }
    //////////////////////////////////////////////////////////////////////
    //////////////////////////////////////////////////////////////////////


    public static Color lookup(String name) {
	if (db.containsKey(name.toUpperCase())) {
	    return (Color) db.get(name.toUpperCase());
	}
	else {
	    return BLACK;
	}
    }
}
