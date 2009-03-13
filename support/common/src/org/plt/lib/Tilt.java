package org.plt.lib;

import org.plt.types.*;
import org.plt.platform.Platform;

public class Tilt {
    public static Object getXAcceleration() {
	TiltService service = 
	    Platform.getInstance().getTiltService();
	return service.getXAcceleration();
    }

    public static Object getYAcceleration() {
	TiltService service = 
	    Platform.getInstance().getTiltService();
	return service.getYAcceleration();
    }

    public static Object getZAcceleration() {
	TiltService service = 
	    Platform.getInstance().getTiltService();
	return service.getZAcceleration();
    }


    public static Object getAzimuth() {
	TiltService service = 
	    Platform.getInstance().getTiltService();
	return service.getAzimuth();
    }

    public static Object getPitch() {
	TiltService service = 
	    Platform.getInstance().getTiltService();
	return service.getPitch();
    }

    public static Object getRoll() {
	TiltService service = 
	    Platform.getInstance().getTiltService();
	return service.getRoll();
    }
}
