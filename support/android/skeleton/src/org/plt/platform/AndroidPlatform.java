package org.plt.platform;


import android.app.Activity;

import org.plt.lib.LocationService;
import org.plt.types.*;


public class AndroidPlatform implements PlatformI {
    private Activity activity;


    public String getName() {
	return "Android";
    }

    public void setActivity (Activity a) {
	this.activity = a;
    }


    public LocationService getLocationService() {
	return new LocationService() {
	    // fixme!
	    public Object getLatitude() {
		// 41 44' N
		return FloatPoint.fromString("44.73");
	    }

	    public Object getLongitude() {
		// 71 26' W
		return FloatPoint.fromString("-71.43");
	    }

	    public Object getAttitude() {
		return Logic.FALSE;
	    }

	    public Object getBearing() {
		return Logic.FALSE;
	    }

	    public Object getSpeed() {
		return Logic.FALSE;
	    }
	    
	};
    }

}