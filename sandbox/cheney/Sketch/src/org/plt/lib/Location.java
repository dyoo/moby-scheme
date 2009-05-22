package org.plt.lib;

import org.plt.types.*;
import org.plt.platform.Platform;
import org.plt.Kernel;


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
	    (Kernel.exact_dash__greaterthan_inexact(lat1).toString(),
	     Kernel.exact_dash__greaterthan_inexact(long1).toString(),
	     Kernel.exact_dash__greaterthan_inexact(lat2).toString(),
	     Kernel.exact_dash__greaterthan_inexact(long2).toString());
    }
}
