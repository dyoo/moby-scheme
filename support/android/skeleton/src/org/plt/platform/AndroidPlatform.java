package org.plt.platform;


import android.app.Activity;
import android.content.Context;
import android.location.Location;
import android.location.LocationManager;
import android.location.LocationProvider;
import android.location.LocationListener;
import android.location.Criteria;
import android.os.Bundle;

import org.plt.lib.LocationService;
import org.plt.types.*;

import org.plt.MessageListener;

import java.util.Iterator;
import java.util.ArrayList;


public class AndroidPlatform implements PlatformI {
    private Activity activity;


    public String getName() {
	return "Android";
    }

    public void setActivity (Activity a) {
	this.activity = a;
    }



    static private AndroidLocationService locationService;

    private class AndroidLocationService 
	implements LocationService {
	private LocationManager manager;
	private LocationProvider provider;
	private Location lastLocation;
	private java.util.List listeners;

	public AndroidLocationService() {

	    this.manager = (LocationManager) 
		activity.getSystemService(Context.LOCATION_SERVICE);
	    this.lastLocation = null;
	    this.listeners = new ArrayList();

	    Criteria c = new Criteria();
	    this.provider = manager.getProvider
		(manager.getBestProvider(c, false));

	    int pollingTime = 1000;
	    int minDistance = 0;
	    this.manager.requestLocationUpdates
		(this.provider.getName(),
		 pollingTime,
		 minDistance,
		 new LocationListener() {
		     public void onLocationChanged(Location location) {
			 lastLocation = location;
			 
			 Iterator iter = listeners.iterator();
			 Object msg =
			     new Pair(FloatPoint.fromString
				      (""+location.getLatitude()),
				      new Pair(FloatPoint.fromString
					       (""+location.getLongitude()), 
					       Empty.EMPTY));;
			 while (iter.hasNext()) {
			     ((MessageListener)iter.next()).onMessage(msg);
			 }
		     }
		     public void onProviderDisabled(String provider) {
		     }
		     public void onProviderEnabled(String provider) {
		     }
		     public void onStatusChanged(String provider,
						 int status,
						 Bundle extras) { 
		     }
								
		 });
	}

	public void addListener(MessageListener listener) {
	    this.listeners.add(listener);
	}


	// fixme!
	public Object getLatitude() {
	    if (this.lastLocation == null) {
		return FloatPoint.fromString("0");
	    }
	    return FloatPoint.fromString
		("" + this.lastLocation.getLatitude());
	}

	public Object getLongitude() {
	    if (this.lastLocation == null) {
		return FloatPoint.fromString("0");
	    }
	    return FloatPoint.fromString
		("" + this.lastLocation.getLongitude());
	}

	public Object getAltitude() {
	    if (this.lastLocation == null) {
		return FloatPoint.fromString("0");
	    }
	    return FloatPoint.fromString
		("" + this.lastLocation.getAltitude());
	}

	public Object getBearing() {
	    if (this.lastLocation == null) {
		return FloatPoint.fromString("0");
	    }
	    return FloatPoint.fromString
		("" + this.lastLocation.getBearing());
	}

	public Object getSpeed() {
	    if (this.lastLocation == null) {
		return FloatPoint.fromString("0");
	    }
	    return FloatPoint.fromString
		("" + this.lastLocation.getSpeed());
	}
	    
    }


    public LocationService getLocationService() {
	if (locationService == null) {
	    locationService = new AndroidLocationService();
	} 
	return locationService;
    }

}
