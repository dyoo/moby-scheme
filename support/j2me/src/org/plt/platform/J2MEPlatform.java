package org.plt.platform;

import org.plt.lib.LocationService;
import org.plt.types.*;
import org.plt.MessageListener;


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

	    public Object getAltitude() {
		return Logic.FALSE;
	    }

	    public Object getBearing() {
		return Logic.FALSE;
	    }

	    public Object getSpeed() {
		return Logic.FALSE;
	    }
	    
	    public void addListener(MessageListener l) {
		// fill me in
	    }
	};
    }
}