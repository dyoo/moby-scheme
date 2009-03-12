package org.plt.platform;

import org.plt.lib.LocationService;
import org.plt.lib.TiltService;
import org.plt.types.*;
import org.plt.LocationChangeListener;
import org.plt.world.TiltChangeListener;


public class J2MEPlatform implements PlatformI {
    public String getName() {
	return "J2ME";
    }


    public TiltService getTiltService() {
	return new TiltService() {
	    public Object getXTilt() {
		return FloatPoint.ZERO;
	    }

	    public Object getYTilt() {
		return FloatPoint.ZERO;
	    }

	    public Object getZTilt() {
		return FloatPoint.ZERO;
	    }

	    public void addTiltChangeListener(TiltChangeListener t) {}
	    
	};
    }

    public LocationService getLocationService() {
	return new LocationService() {
	    // fixme!
	    public Object getLatitude() {
		return FloatPoint.ZERO;
	    }

	    public Object getLongitude() {
		return FloatPoint.ZERO;
	    }

	    public Object getAltitude() {
		return FloatPoint.ZERO;
	    }

	    public Object getBearing() {
		return FloatPoint.ZERO;
	    }

	    public Object getSpeed() {
		return FloatPoint.ZERO;
	    }
	    
	    public void addLocationChangeListener(LocationChangeListener l) {
		// fill me in
	    }
	};
    }
}
