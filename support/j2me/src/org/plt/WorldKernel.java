package org.plt;

import javax.microedition.lcdui.*;
import javax.microedition.midlet.*;
import org.plt.types.*;
import org.plt.gui.*;

import org.plt.world.WorldRunner;
import org.plt.world.WorldTransformer;
import org.plt.world.WorldJudge;

import org.plt.world.config.*;



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
    private static Callable onOrientationChangeEventHandler;
    private static Callable onAccelerationChangeEventHandler;
    private static Callable onRedrawHandler;
    private static Callable stopWhenHandler;

    private static WorldRunner runner;

    // Here are defaults for our handlers.
    static {
	onTickHandler = 
	    onKeyEventHandler = 
	    onMouseEventHandler =
	    onMessageEventHandler =
	    onLocationChangeEventHandler =
	    onOrientationChangeEventHandler =
	    onAccelerationChangeEventHandler =
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
	runner = new WorldRunner();
    }


    //////////////////////////////////////////////////////////////////////
    // Configuration initialization
    static class ConfigReader implements ConfigVisitor {
	public void visit(OnTick config) {
	    WorldKernel.onTickHandler = config.c;
	}

	public void visit(OnMouse config) {
	    WorldKernel.onMouseEventHandler = config.c;
	}

	public void visit(OnKey config) {
	    WorldKernel.onKeyEventHandler = config.c;
	}

	public void visit(OnMessage config) {
	    WorldKernel.onMessageEventHandler = config.c;
	}

	public void visit(OnLocationChange config) {
	    WorldKernel.onLocationChangeEventHandler = config.c;
	}

	public void visit(OnTilt config) {
	    WorldKernel.onOrientationChangeEventHandler = config.c;
	}

	public void visit(OnAcceleration config) {
	    WorldKernel.onAccelerationChangeEventHandler = config.c;
	}

	public void visit(OnRedraw config) {
	    WorldKernel.onRedrawHandler = config.c;
	}

	public void visit(StopWhen config) {
	    WorldKernel.stopWhenHandler = config.c;
	}
    }
    //////////////////////////////////////////////////////////////////////



    public static void setRunner(WorldRunner runner) {
	WorldKernel.runner = runner;
    }


    public static Object bigBang(Object width,
				 Object height,
				 Object frameRate,
				 Object initialWorld,
				 Object[] config) {
	WorldKernel.width = width;
        WorldKernel.height = height;
	WorldKernel.frameRate = frameRate;
	WorldKernel.initialWorld = initialWorld;

	ConfigVisitor configReader = new ConfigReader();
	for (int i = 0; i < config.length; i++) {
	    ((Config) config[i]).accept(configReader);
	}

	WorldKernel.runner.setWorld(initialWorld);
	WorldKernel.runner.setDelay
	    ((long) ((org.plt.types.NumberTower.multiply
		      ((org.plt.types.Number) frameRate,
		       new org.plt.types.Rational(1000, 1))).toInt()));

	WorldKernel.runner.setOnTick(new WorldTransformer() {
		public Object transform(Object world) {
		    return onTickHandler.call(new Object[] { world });
		}
	    });
	WorldKernel.runner.setStopWhen(new WorldJudge() {
		public boolean judge(Object world) {
		    return ((org.plt.types.Logic) 
			    stopWhenHandler.call(new Object[] { world }))
			.isTrue();
		}
	    });
	return WorldKernel.runner.bigBang();
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

    public static Callable getOnOrientationChangeEventHandler() {
	return WorldKernel.onOrientationChangeEventHandler;
    }

    public static Callable getOnAccelerationChangeEventHandler() {
	return WorldKernel.onAccelerationChangeEventHandler;
    }

    public  static Callable getOnRedrawHandler() {
	return WorldKernel.onRedrawHandler;
    }

    public static Callable getStopWhenHandler() {
	return WorldKernel.stopWhenHandler;
    }


    //////////////////////////////////////////////////////////////////////

    public static Scene emptyScene(Object width, Object height) {
	return Scene.emptyScene(((org.plt.types.Number)width).toInt(),
				((org.plt.types.Number)height).toInt());
    }

    public static Scene placeImage(Object image,
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

    
    public static Picture nwRectangle(Object width, Object height, 
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


    public static Logic isKeyEqual(Object k1, Object k2) {
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
    public static Picture _kernelCreateImage(Object pathString) {
	return new FilePicture((String) pathString);
    }



    public static org.plt.types.Number imageWidth(Object img) {
	return new Rational(((Picture)img).getWidth(), 1);
    }


    public static org.plt.types.Number imageHeight(Object img) {
	return new Rational(((Picture)img).getHeight(), 1);
    }
    

    public static org.plt.types.Logic isImage(Object x) {
	return toLogic(x instanceof Picture);
    }


    public static Logic isImageEqual(Object obj1, 
					       Object obj2) {
	// Fixme: fill me in
	return toLogic(obj1.equals(obj2));
    }

    public static Picture imageRotate(Object image, Object degrees) {
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
