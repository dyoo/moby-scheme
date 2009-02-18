package org.plt.platform;


import android.app.Activity;
import android.content.Context;
import android.location.LocationManager;
import android.location.Criteria;

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



    public class AndroidLocationService 
	implements LocationService {

	public AndroidLocationService() {
	    LocationManager manager = (LocationManager) 
		activity.getSystemService(Context.LOCATION_SERVICE);
	    Criteria c = new Criteria();
	    c.setCostAllowed(false);
	}

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
	    
    }

    public LocationService getLocationService() {
	return new AndroidLocationService();
    }

}
