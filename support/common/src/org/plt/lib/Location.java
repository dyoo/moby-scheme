package org.plt.lib;

import org.plt.types.*;
import org.plt.platform.Platform;

public class Location {
    public static Object getLatitude() {
	LocationService service = 
	    Platform.getInstance().getLocationService();
	return service.getLatitude();
    }

    public static Object getLongitude() {
	LocationService service = 
	    Platform.getInstance().getLocationService();
	return service.getLongitude();
    }

    public static Object getAttitude() {
	LocationService service = 
	    Platform.getInstance().getLocationService();
	return service.getAttitude();

    }

    public static Object getBearing() {
	LocationService service = 
	    Platform.getInstance().getLocationService();
	return service.getBearing();

    }

    public static Object getSpeed() {
	LocationService service = 
	    Platform.getInstance().getLocationService();
	return service.getSpeed();

    }
}