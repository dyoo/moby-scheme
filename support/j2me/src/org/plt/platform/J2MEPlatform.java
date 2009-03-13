package org.plt.platform;

import org.plt.lib.LocationService;
import org.plt.lib.TiltService;
import org.plt.types.*;
import org.plt.LocationChangeListener;
import org.plt.world.TiltChangeListener;
import org.plt.world.AccelerationChangeListener;


public class J2MEPlatform implements PlatformI {
    public String getName() {
	return "J2ME";
    }


    public TiltService getTiltService() {
	return new TiltService() {
	    public Object getXAcceleration() {
		return FloatPoint.ZERO;
	    }

	    public Object getYAcceleration() {
		return FloatPoint.ZERO;
	    }

	    public Object getZAcceleration() {
		return FloatPoint.ZERO;
	    }


	    public Object getAzimuth() {
		return FloatPoint.ZERO;
	    }

	    public Object getPitch() {
		return FloatPoint.ZERO;
	    }

	    public Object getRoll() {
		return FloatPoint.ZERO;
	    }

	    public void addTiltChangeListener(TiltChangeListener t) {}
	    public void addAccelerationChangeListener(AccelerationChangeListener t) {}

	    public void shutdownService() {}
	    public void startService() {}
	    
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

	    public void shutdownService() {}
	    public void startService() {}
	};
    }
}
