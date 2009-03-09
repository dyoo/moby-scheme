package org.plt.lib;

import org.plt.types.*;
import org.plt.platform.Platform;

public class Tilt {
    public static Object getXTilt() {
	TiltService service = 
	    Platform.getInstance().getTiltService();
	return service.getXTilt();
    }

    public static Object getYTilt() {
	TiltService service = 
	    Platform.getInstance().getTiltService();
	return service.getYTilt();
    }

    public static Object getZTilt() {
	TiltService service = 
	    Platform.getInstance().getTiltService();
	return service.getZTilt();
    }
}
