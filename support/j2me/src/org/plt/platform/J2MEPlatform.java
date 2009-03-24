package org.plt.platform;

import org.plt.lib.LocationService;
import org.plt.lib.TiltService;
import org.plt.lib.SmsService;
import org.plt.lib.NetworkService;

import org.plt.types.*;

import org.plt.world.LocationChangeListener;
import org.plt.world.OrientationChangeListener;
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

	    public void addOrientationChangeListener(OrientationChangeListener t) {}
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
	    
	    public Object getDistanceBetween(String lat1, String long1,
					     String lat2, String long2) {
		return FloatPoint.ZERO;
	    }

	    public void addLocationChangeListener(LocationChangeListener l) {
		// fill me in
	    }

	    public void shutdownService() {}
	    public void startService() {}
	};
    }

    public SmsService getSmsService() {
	return new SmsService() {
	    public void sendTextMessage(String destinationString, String msg) {
		// Fill me in.
	    }
	};
    }

    public NetworkService getNetworkService() {
	return new NetworkService() {
	    public String getUrl(String url) {
		// Fill me in.
		return "";
	    }
	};
    }
}
