package org.plt;

import javax.microedition.lcdui.*;
import javax.microedition.midlet.*;
import org.plt.types.*;
import org.plt.gui.*;


// World kernel functions

public class WorldKernel {
    public static Scene empty_dash_scene(Object[] args) {
	return _empty_dash_scene(args[0], args[1]);
    }

    private static Scene _empty_dash_scene(Object width, Object height) {
	return Scene.emptyScene(((org.plt.types.Number)width).toInt(),
				((org.plt.types.Number)height).toInt());
    }



    public static Scene place_dash_image(Object[] args) {
	return _place_dash_image(args[0], args[1], args[2], args[3]);
    }

    private static Scene _place_dash_image(Object image,
					   Object x,
					   Object y,
					   Object scene) {
	return ((Scene)scene).placeImage((Picture)image,
					 ((org.plt.types.Number) x).toInt(),
					 ((org.plt.types.Number) y).toInt());
    }



    public static Picture circle(Object[] args) {
	return _circle(args[0], args[1], args[2]);
    }

    private static Picture _circle(Object radius, Object style, Object color) {
	return new CirclePicture(((org.plt.types.Number)radius).toInt(),
				 coerseToString(style),
				 Color.lookup(coerseToString(color)));
    }



    public static Picture nw_colon_rectangle(Object[] args) {
	return _nw_colon_rectangle(args[0], args[1]);
    }
    
    private static Picture _nw_colon_rectangle(Object width, Object height, 
					       Object style, Object color) {
	return new NwRectanglePicture(((org.plt.types.Number)width).toInt(),
				      ((org.plt.types.Number)height).toInt(),
				      coerseToString(style),
				      Color.lookup(coerseToString(color)));
    }



    public static Picture rectangle(Object[] args) {
	return _rectangle(args[0], args[1], args[2], args[3]);
    }

    private static Picture _rectangle(Object width, Object height, 
				      Object style, Object color) {
	return new RectanglePicture(((org.plt.types.Number)width).toInt(),
				    ((org.plt.types.Number)height).toInt(),
				    coerseToString(style),
				    Color.lookup(coerseToString(color)));
    }


    public static Object key_equal__question_(Object[] args) {
	return _key_equal__question_(args[0], args[1]);
    }

    private static Object _key_equal__question_(Object k1, Object k2) {
	return toLogic(k1.toString().toUpperCase().equals
			      (k2.toString().toUpperCase()));

    }



    public static Picture text(Object[] args) {
	return _text(args[0], args[1], args[2]);
    }

    public static Picture _text(Object s, Object size, Object color) {
	return new TextPicture((String)s,
			       ((org.plt.types.Number)size).toInt(),
			       Color.lookup(coerseToString(color)));
    }


    // Loads up the image resource named by filename.
    // FIXME: we still don't have a good way to prevent the user from
    // colliding with this name accidently...
    public static Picture _dash_kernel_dash_create_dash_image(Object[] args) {
	return new FilePicture((String) args[0]);
    }


    public static  org.plt.types.Number image_dash_width(Object[] args) {
	return _image_dash_width(args[0]);
    }


    private static org.plt.types.Number _image_dash_width(Object img) {
	return new Rational(((Picture)img).getWidth(), 1);
    }



    public  static org.plt.types.Number image_dash_height(Object[] args) {
	return _image_dash_height(args[0]);
    }

    private static org.plt.types.Number _image_dash_height(Object img) {
	return new Rational(((Picture)img).getHeight(), 1);
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