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

import android.telephony.gsm.SmsManager;

import android.hardware.SensorManager; 
import android.hardware.SensorListener; 


import org.plt.lib.LocationService;
import org.plt.lib.TiltService;
import org.plt.lib.SmsService;

import org.plt.types.*;

import org.plt.world.MessageListener;
import org.plt.world.LocationChangeListener;
import org.plt.world.OrientationChangeListener;
import org.plt.world.AccelerationChangeListener;

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

	public Object getDistanceBetween(String lat1, String long1,
					 String lat2, String long2) {
	    System.out.println("Getting distance between: " + lat1);
	    System.out.println(" " + long1);
	    System.out.println(" " + lat2);
	    System.out.println(" " + long2);
	    float[] results = new float[1];
	    Location.distanceBetween(Double.parseDouble(lat1),
				     Double.parseDouble(long1),
				     Double.parseDouble(lat2),
				     Double.parseDouble(long2),
				     results);
	    return FloatPoint.fromString("" + results[0]);
	}
    }


    public LocationService getLocationService() {
	if (locationService == null) {
	    locationService = new AndroidLocationService();
	} 
	return locationService;
    }



    public SmsService getSmsService() {
	return new SmsService() {
	    public void sendTextMessage(String address, String msg) {
		SmsManager manager = SmsManager.getDefault();
		manager.sendTextMessage(address,
					null,
					msg,
					null,
					null);
	    }
	};
    }




    //////////////////////////////////////////////////////////////////////
    // Accelerometer.

    public TiltService getTiltService() {
	if (tiltService == null) {
	    tiltService = new AndroidTiltService();
	}
	return tiltService;
    }


    private class AndroidTiltService implements TiltService {
	SensorManager manager;
	float[] lastOrientationValues;
	float[] lastAccelerationValues;

	java.util.List orientationListeners;
	java.util.List accelerationListeners;

	SensorListener mainOrientationListener;
	SensorListener mainAccelerationListener;


	public AndroidTiltService() {
	    orientationListeners = new ArrayList();
	    accelerationListeners = new ArrayList();

	    mainOrientationListener = new SensorListener() {
		    public void onAccuracyChanged(int sensor, int accuracy) {
		    }

		    public void onSensorChanged(int sensor, float[] values) {
			lastOrientationValues = values;
			Object azimuth = FloatPoint.fromString(""+ values[0]);
			Object pitch = FloatPoint.fromString(""+values[1]);
			Object roll = FloatPoint.fromString(""+values[2]);
			for(int i = 0; i < orientationListeners.size(); i++) {
			    ((OrientationChangeListener) orientationListeners.get(i)).onOrientationChange(azimuth, pitch, roll);

			}
		    }
		};

	    mainAccelerationListener = new SensorListener() {
		    public void onAccuracyChanged(int sensor, int accuracy) {
		    }

		    public void onSensorChanged(int sensor, float[] values) {
			lastAccelerationValues = values;
			Object x = FloatPoint.fromString(""+ values[0]);
			Object y = FloatPoint.fromString(""+values[1]);
			Object z = FloatPoint.fromString(""+values[2]);
			for(int i = 0; i < accelerationListeners.size(); i++) {
			    ((AccelerationChangeListener) accelerationListeners.get(i)).onAccelerationChange(x, y, z);

			}
		    }


		};
	}

	public void startService() {
	    final AndroidTiltService service = this;
	    new HandlerThread("Tilt") {
		public void run() {
		    Looper.prepare();
		    manager = (SensorManager) 
			activity.getSystemService(Context.SENSOR_SERVICE);
		    manager.registerListener(mainAccelerationListener, SensorManager.SENSOR_ACCELEROMETER);
		    manager.registerListener(mainOrientationListener, SensorManager.SENSOR_ORIENTATION);
		    Looper.loop();
		}
	    }.start();
	}

	public void shutdownService() {
	    manager.unregisterListener(mainAccelerationListener);	   
	    manager.unregisterListener(mainOrientationListener);
	}


	public Object getXAcceleration() {
	    if (lastAccelerationValues != null) {
		return FloatPoint.fromString(""+lastAccelerationValues[0]);
	    }
	    return FloatPoint.ZERO;
	}

	public Object getYAcceleration() {
	    if (lastAccelerationValues != null) {
		return FloatPoint.fromString(""+lastAccelerationValues[1]);
	    }
	    return FloatPoint.ZERO;
	}

	public Object getZAcceleration() {
	    if (lastAccelerationValues != null) {
		return FloatPoint.fromString(""+lastAccelerationValues[2]);
	    }
	    return FloatPoint.ZERO;
	}


	public Object getAzimuth() {
	    if (lastOrientationValues != null) {
		return FloatPoint.fromString(""+lastOrientationValues[0]);
	    }
	    return FloatPoint.ZERO;
	}

	public Object getPitch() {
	    if (lastOrientationValues != null) {
		return FloatPoint.fromString(""+lastOrientationValues[1]);
	    }
	    return FloatPoint.ZERO;
	}

	public Object getRoll() {
	    if (lastOrientationValues != null) {
		return FloatPoint.fromString(""+lastOrientationValues[2]);
	    }
	    return FloatPoint.ZERO;
	}




	public void addOrientationChangeListener(OrientationChangeListener l) {
	    orientationListeners.add(l);
	}

	public void addAccelerationChangeListener(AccelerationChangeListener l) {
	    accelerationListeners.add(l);
	}

    }

}
