package com.phonegap.demo;

import android.content.Context;
import android.location.Location;
import android.webkit.WebView;

public class GeoListener {
    String id;
    String successCallback;
    String failCallback;

    LocationProviderListener providerListener;
    Context mCtx;
    private WebView mAppView;
    private ArgTable arguments;

    int interval;
	
    GeoListener(String i, Context ctx, int time, WebView appView, ArgTable args)
    {
	id = i;
	interval = time;
	mCtx = ctx;
	providerListener = new LocationProviderListener(mCtx, interval, this);
	mAppView = appView;
	arguments = args;
    }
	
    void success(Location loc)
    {
	arguments.put("gpsLat", new Float(loc.getLatitude()));
	arguments.put("gpsLng", new Float(loc.getLongitude()));
	/*
	 * We only need to figure out what we do when we succeed!
	 */
	if(!id.equals("global"))
	    {
		arguments.put("gpsId", id);
		mAppView.loadUrl("javascript:navigator.phonegap_geo.success()");
	    }
	else
	    {
			
		mAppView.loadUrl("javascript:Geolocation.gotCurrentPosition()");
		this.stop();
	    }
    }
	
    void fail()
    {
	// Do we need to know why?  How would we handle this?
	mAppView.loadUrl("javascript:GeoLocation.fail()");
    }
	

    void stop()
    {
	providerListener.stop();
    }

    public Location getCurrentLocation() {
	return providerListener.getLocation();
    }
}
