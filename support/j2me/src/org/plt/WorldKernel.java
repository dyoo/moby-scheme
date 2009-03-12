package org.plt;

import javax.microedition.lcdui.*;
import javax.microedition.midlet.*;
import org.plt.types.*;
import org.plt.gui.*;


// World kernel functions

public class WorldKernel {
    private static Object width;
    private static Object height;
    private static Object frameRate;
    private static Object initialWorld;

    private static Callable onTickHandler;
    private static Callable onKeyEventHandler;
    private static Callable onMouseEventHandler;
    private static Callable onMessageEventHandler;
    private static Callable onLocationChangeEventHandler;
    private static Callable onTiltChangeEventHandler;
    private static Callable onRedrawHandler;
    private static Callable stopWhenHandler;

    // Here are defaults for our handlers.
    static {
	onTickHandler = 
	    onKeyEventHandler = 
	    onMouseEventHandler =
	    onMessageEventHandler =
	    onLocationChangeEventHandler =
	    new Callable() {
		public Object call(Object[] args) {
		    return args[0];
		}
	    };
	onRedrawHandler = new Callable() {
		public Object call(Object[] args) {
		    return Scene.emptyScene(100, 100);
		}
	    };
	stopWhenHandler = new Callable() {
		public Object call(Object[] args) {
		    return Logic.FALSE;
		}
	    };
    }

    public static Object big_dash_bang(Object width,
				       Object height,
				       Object frameRate,
				       Object initialWorld) {
	WorldKernel.width = width;
        WorldKernel.height = height;
	WorldKernel.frameRate = frameRate;
	WorldKernel.initialWorld = initialWorld;
	return VoidObject.VOID;
    }
    public static Object getWidth() {
	return WorldKernel.width;
    }
    public static Object getHeight() {
	return WorldKernel.height;
    }
    public static Object getFrameRate() {
	return WorldKernel.frameRate;
    }
    public static Object getInitialWorld() {
	return WorldKernel.initialWorld;
    }
    public static Callable getOnTickHandler() {
	return WorldKernel.onTickHandler;
    }
    public static Callable getOnKeyEventHandler() {
	return WorldKernel.onKeyEventHandler;
    }
    public static Callable getOnMouseEventHandler() {
	return WorldKernel.onMouseEventHandler;
    }
    public static Callable getOnMessageEventHandler() {
	return WorldKernel.onMessageEventHandler;
    }
    public static Callable getOnLocationChangeEventHandler() {
	return WorldKernel.onLocationChangeEventHandler;
    }
    public static Callable getOnTiltChangeEventHandler() {
	return WorldKernel.onTiltChangeEventHandler;
    }
    public  static Callable getOnRedrawHandler() {
	return WorldKernel.onRedrawHandler;
    }
    public static Callable getStopWhenHandler() {
	return WorldKernel.stopWhenHandler;
    }



    //////////////////////////////////////////////////////////////////////




    public static Object on_dash_tick(Object callable) {
	WorldKernel.onTickHandler = (Callable) callable;
	return VoidObject.VOID;
    }

    public static Object on_dash_key_dash_event(Object callable) {
	WorldKernel.onKeyEventHandler = (Callable) callable;
	return VoidObject.VOID;
    }


    public static Object on_dash_mouse_dash_event(Object callable) {
	WorldKernel.onMouseEventHandler = (Callable) callable;
	return VoidObject.VOID;
    }

    public static Object on_dash_message_dash_event(Object callable) {
	WorldKernel.onMessageEventHandler = (Callable) callable;
	return VoidObject.VOID;
    }


    public static Object on_dash_location_dash_change_dash_event(Object callable) {
	WorldKernel.onLocationChangeEventHandler = (Callable) callable;
	return VoidObject.VOID;
    }

    public static Object on_dash_redraw(Object callable) {
	WorldKernel.onRedrawHandler = (Callable) callable;
	return VoidObject.VOID;
    }

    public static Object stop_dash_when(Object callable) {
	WorldKernel.stopWhenHandler = (Callable) callable;
	return VoidObject.VOID;
    }


    public static Object on_dash_tilt_dash_change_dash_event(Object callable) {
	WorldKernel.onTiltChangeEventHandler = (Callable) callable;
	return VoidObject.VOID;
    }



    //////////////////////////////////////////////////////////////////////

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
    

    public static org.plt.types.Logic image_question_(Object x) {
	return toLogic(x instanceof Picture);
    }


    public static Logic image_equal__question_(Object obj1, 
					       Object obj2) {
	// Fixme: fill me in
	return toLogic(obj1.equals(obj2));
    }

    public static Picture image_dash_rotate(Object image, Object degrees) {
	// Fixme: fill me in
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
