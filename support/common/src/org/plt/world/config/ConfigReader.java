package org.plt.world.config;

import org.plt.types.*;

import org.plt.gui.Scene;

public class ConfigReader {
    public Callable onTickHandler;
    public Callable onKeyHandler;
    public Callable onMouseHandler;
    public Callable onMessageHandler;
    public Callable onLocationChangeHandler;
    public Callable onTiltHandler;
    public Callable onAccelerationHandler;
    public Callable onRedrawHandler;
    public Callable stopWhenHandler;


    public ConfigReader() {

	// Here are defaults for our handlers.
	onTickHandler = 
	    onKeyHandler = 
	    onMouseHandler =
	    onMessageHandler =
	    onLocationChangeHandler =
	    onTiltHandler =
	    onAccelerationHandler =
	    new Callable() {
		public Object call(Object[] args) {
		    return args[0];
		}
	    };
	onRedrawHandler = new Callable() {
		public Object call(Object[] args) {
		    return Scene.emptyScene(320, 480);
		}
	    };
	stopWhenHandler = new Callable() {
		public Object call(Object[] args) {
		    return Logic.FALSE;
		}
	    };
    }



    public void load(Object[] configs) {
	for (int i = 0; i < configs.length; i++) {
	    ((Config) configs[i]).accept(new MyVisitor());
	}
    }



    class MyVisitor implements ConfigVisitor {
	public void visit(OnTick config) {
	    onTickHandler = config.c;
	}

	public void visit(OnMouse config) {
	    onMouseHandler = config.c;
	}

	public void visit(OnKey config) {
	    onKeyHandler = config.c;
	}

	public void visit(OnMessage config) {
	    onMessageHandler = config.c;
	}

	public void visit(OnLocationChange config) {
	    onLocationChangeHandler = config.c;
	}

	public void visit(OnTilt config) {
	    onTiltHandler = config.c;
	}

	public void visit(OnAcceleration config) {
	    onAccelerationHandler = config.c;
	}

	public void visit(OnRedraw config) {
	    onRedrawHandler = config.c;
	}

	public void visit(StopWhen config) {
	    stopWhenHandler = config.c;
	}
    }

}