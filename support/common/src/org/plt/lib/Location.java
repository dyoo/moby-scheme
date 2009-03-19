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

    public static Object getAltitude() {
	LocationService service = 
	    Platform.getInstance().getLocationService();
	return service.getAltitude();

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

    public static Object getDistanceBetween(Object lat1,
					    Object long1,
					    Object lat2,
					    Object long2) {
	LocationService service = 
	    Platform.getInstance().getLocationService();
	return service.getDistanceBetween
	    (lat1.toString(),
	     long1.toString(),
	     lat2.toString(),
	     long2.toString());

    }
}