package org.plt.world.config;

import org.plt.types.Callable;

public class Kernel {
    static public Config onKey(Object c) {
	return new OnKey((Callable) c);
    }

    static public Config onTick(Object c) {
	return new OnTick((Callable) c);
    }

    static public Config onMouse(Object c) {
	return new OnMouse((Callable) c);
    }

    static public Config onMessage(Object c) {
	return new OnMessage((Callable) c);
    }

    static public Config onLocationChange(Object c){
	return new OnLocationChange((Callable) c);
    }

    static public Config onTilt(Object c) {
	return new OnTilt((Callable) c);
    }

    static public Config onAcceleration(Object c) {
	return new OnAcceleration((Callable) c);
    }

    static public Config onRedraw(Object c) {
	return new OnRedraw((Callable) c);
    }

    static public Config stopWhen(Object c) {
	return new StopWhen((Callable) c);
    }
}