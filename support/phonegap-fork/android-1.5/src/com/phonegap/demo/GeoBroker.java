package com.phonegap.demo;

import java.util.HashMap;

import android.content.Context;
import android.webkit.WebView;
import android.util.Log;
import android.location.Location;



/*
 * This class is the interface to the Geolocation.  It's bound to the geo object.
 * 
 * This class only starts and stops various GeoListeners, which consist of a GPS and a Network Listener
 */

public class GeoBroker implements LifecycleService {
    private WebView mAppView;
    private Context mCtx;
    private ArgTable arguments;
    private HashMap<String, GeoListener> geoListeners;
	
    public GeoBroker(Context ctx, WebView view, ArgTable args)
    {
	mCtx = ctx;
	mAppView = view;
	arguments = args;
	geoListeners = new HashMap<String, GeoListener>();
    }
	
    public void getCurrentLocation()
    {
	GeoListener listener = new GeoListener("global", mCtx, 10000, mAppView, arguments);
    }
	

    public float getDistanceBetween(double lat1, double long1, 
				    double lat2, double long2) {
	float[] result = new float[1];
	Location.distanceBetween(lat1, long1, lat2, long2, result);
	return result[0];
	
    }


    public String start(int freq, String key)
    {
// 	Log.d("GeoBroker start",
// 	      "Making new GeoListener with freq " + freq +
// 	      " and key " + key);
	GeoListener listener = new GeoListener(key, mCtx, freq, mAppView, arguments);
	geoListeners.put(key, listener);
	return key;
    }
	
    public void stop(String key)
    {
	GeoListener geo = geoListeners.remove(key);
	if (geo != null) {
	    geo.stop();
	}
    }





    // LifecycleService implementations

    public void onPause() {}

    public void onResume() {
	// FILL ME IN
    }

    public void onStop() {
	for ( GeoListener geo : geoListeners.values() ) {
	    geo.stop();
	}
	geoListeners.clear();
    }
    
    public void onRestart() {
	// FILL ME IN
    }

    public void onDestroy() {
	for ( GeoListener geo : geoListeners.values() ) {
	    geo.stop();
	}
	geoListeners.clear();
    }


}
