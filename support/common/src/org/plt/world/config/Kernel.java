package org.plt.world.config;

import org.plt.types.Callable;
import org.plt.types.Number;
import org.plt.types.Rational;
import org.plt.types.NumberTower;

public class Kernel {
    static public Config onKey(Object c) {
	return new OnKey((Callable) c);
    }

    static public Config onTick(Object delay, Object c) {
	Number delayInMilliseconds =
	    (NumberTower.multiply((Number) delay,
				  new Rational(1000, 1)));

	return new OnTick((Number) delayInMilliseconds,
			  (Callable) c);
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