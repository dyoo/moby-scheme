package org.plt.platform;


import android.app.Activity;
import android.content.Context;
import android.location.Location;
import android.location.LocationManager;
import android.location.LocationProvider;
import android.location.LocationListener;
import android.location.Criteria;
import android.os.Bundle;
import android.os.HandlerThread;
import android.os.Looper;

import android.hardware.SensorManager; 
import android.hardware.SensorListener; 




import org.plt.lib.LocationService;
import org.plt.lib.TiltService;
import org.plt.types.*;

import org.plt.MessageListener;
import org.plt.LocationChangeListener;
import org.plt.world.TiltChangeListener;

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





    static private LocationService locationService;
    static private TiltService tiltService;



    private class AndroidLocationService 
	implements LocationService, LocationListener {
	private LocationManager manager;
	private Location lastLocation;
	private java.util.List listeners;
	private java.util.List locationListeners;


	public AndroidLocationService() {
	    this.listeners = new ArrayList();
	    this.locationListeners = new ArrayList();
	}

	public void startService() {
	    manager = (LocationManager) 
		activity.getSystemService(Context.LOCATION_SERVICE);
	    final AndroidLocationService service = this;

	    new HandlerThread("Location") {
		public void run() {
		    Looper.prepare();
		    Criteria c = new Criteria();
		    int pollingTime = 1000;
		    int minDistance = 0;
		    
		    java.util.List providerNames = manager.getProviders(c, true);
		    for(int i = 0; i < providerNames.size(); i++) {
			String providerName = (String) providerNames.get(i);
			manager.requestLocationUpdates
			    (providerName, pollingTime, minDistance, service);
			
		    }
		    
		    Looper.loop();
		}
	    }.start();
	}

	public void shutdownService() {
	    this.manager.removeUpdates(this);
	}


//////////////////////////////////////////////////////////////////////
	public void onLocationChanged(Location location) {
	    lastLocation = location;
	    Iterator iter = locationListeners.iterator();
	    while (iter.hasNext()) {
		((LocationChangeListener)iter.next()).
		    onLocationChange(FloatPoint.fromString("" + location.getLatitude()),
				     FloatPoint.fromString("" + location.getLongitude()));
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
//////////////////////////////////////////////////////////////////////




	public void addMessageListener(MessageListener listener) {
	    this.listeners.add(listener);
	}

	public void addLocationChangeListener(LocationChangeListener listener) {
	    this.locationListeners.add(listener);
	}



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






    //////////////////////////////////////////////////////////////////////
    // Accelerometer.

    public TiltService getTiltService() {
	if (tiltService == null) {
	    tiltService = new AndroidTiltService();
	}
	return tiltService;
    }


    private class AndroidTiltService implements TiltService, SensorListener {
	SensorManager manager;
	float[] lastValues;
	java.util.List listeners;

	public AndroidTiltService() {
	    listeners = new ArrayList();
	}

	public void startService() {
	    final AndroidTiltService service = this;
	    new HandlerThread("Tilt") {
		public void run() {
		    Looper.prepare();
		    manager = (SensorManager) 
			activity.getSystemService(Context.SENSOR_SERVICE);
		    manager.registerListener(service, SensorManager.SENSOR_ACCELEROMETER);
		    Looper.loop();
		}
	    }.start();
	}

	public void shutdownService() {
	    manager.unregisterListener(this);
	}


	public void onAccuracyChanged(int sensor, int accuracy) {
	}

	public void onSensorChanged(int sensor, float[] values) {
	    lastValues = values;
	    Object x = getXTilt();
	    Object y = getYTilt();
	    Object z = getZTilt();
	    for(int i = 0; i < listeners.size(); i++) {
		((TiltChangeListener) listeners.get(i)).onTiltChange(x, y, z);

	    }
	}


	public Object getXTilt() {
	    if (lastValues != null) {
		return FloatPoint.fromString(""+lastValues[SensorManager.DATA_X]);
	    }
	    return FloatPoint.ZERO;
	}

	public Object getYTilt() {
	    if (lastValues != null) {
		return FloatPoint.fromString(""+lastValues[SensorManager.DATA_Y]);
	    }
	    return FloatPoint.ZERO;
	}

	public Object getZTilt() {
	    if (lastValues != null) {
		return FloatPoint.fromString(""+lastValues[SensorManager.DATA_Z]);
	    }
	    return FloatPoint.ZERO;
	}

	public void addTiltChangeListener(TiltChangeListener l) {
	    listeners.add(l);
	}
    }

}
