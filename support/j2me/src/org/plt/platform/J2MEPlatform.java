package org.plt.platform;

import org.plt.lib.LocationService;
import org.plt.types.*;


public class J2MEPlatform implements PlatformI {
    public String getName() {
	return "J2ME";
    }

    public LocationService getLocationService() {
	return new LocationService() {
	    // fixme!
	    public Object getLatitude() {
		return Logic.FALSE;
	    }

	    public Object getLongitude() {
		return Logic.FALSE;
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