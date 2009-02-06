package org.plt;

import javax.microedition.lcdui.*;
import javax.microedition.midlet.*;
import org.plt.types.*;
import org.plt.gui.*;


// World kernel functions

public class WorldKernel {

    public static Scene empty_dash_scene(Object width, Object height) {
	return Scene.emptyScene(((org.plt.types.Number)width).toInt(),
				((org.plt.types.Number)height).toInt());
    }

    public static Scene place_dash_image(Object image,
					   Object x,
					   Object y,
					   Object scene) {
	return ((Scene)scene).placeImage((Picture)image,
					 ((org.plt.types.Number) x).toInt(),
					 ((org.plt.types.Number) y).toInt());
    }

    public static Picture circle(Object radius, Object style, Object color) {
	return new CirclePicture(((org.plt.types.Number)radius).toInt(),
				 coerseToString(style),
				 Color.lookup(coerseToString(color)));
    }

    
    public static Picture nw_colon_rectangle(Object width, Object height, 
					       Object style, Object color) {
	return new NwRectanglePicture(((org.plt.types.Number)width).toInt(),
				      ((org.plt.types.Number)height).toInt(),
				      coerseToString(style),
				      Color.lookup(coerseToString(color)));
    }

    public static Picture rectangle(Object width, Object height, 
				      Object style, Object color) {
	return new RectanglePicture(((org.plt.types.Number)width).toInt(),
				    ((org.plt.types.Number)height).toInt(),
				    coerseToString(style),
				    Color.lookup(coerseToString(color)));
    }


    public static Object key_equal__question_(Object k1, Object k2) {
	return toLogic(k1.toString().toUpperCase().equals
			      (k2.toString().toUpperCase()));

    }


    public static Picture text(Object s, Object size, Object color) {
	return new TextPicture((String)s,
			       ((org.plt.types.Number)size).toInt(),
			       Color.lookup(coerseToString(color)));
    }


    // Loads up the image resource named by filename.
    // FIXME: we still don't have a good way to prevent the user from
    // colliding with this name accidently...
    public static Picture _dash_kernel_dash_create_dash_image(Object pathString) {
	return new FilePicture((String) pathString);
    }



    public static org.plt.types.Number image_dash_width(Object img) {
	return new Rational(((Picture)img).getWidth(), 1);
    }


    public static org.plt.types.Number image_dash_height(Object img) {
	return new Rational(((Picture)img).getHeight(), 1);
    }
    

    public static Picture image_dash_rotate(Object image) {
	// Fill me in!
	return (Picture) image;
    }


    // Coerses a symbol or string into a string.
    private static String coerseToString(Object obj) {
	if (obj instanceof Symbol) {
	    return ((Symbol)obj).toString();
	} else {
	    return (String) obj;
	}
    }


    // Converts from boolean to Logics.
    private static Logic toLogic(boolean b) {
	return b ? Logic.TRUE : Logic.FALSE;
    }
}
