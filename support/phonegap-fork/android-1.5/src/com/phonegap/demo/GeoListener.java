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
	
    GeoListener(String i, Context ctx, int time, WebView appView, ArgTable args) {
	this.id = i;
	this.interval = time;
	this.mCtx = ctx;
	this.providerListener = new LocationProviderListener(mCtx, interval, this);
	this.mAppView = appView;
	this.arguments = args;
    }
	
    void success(Location loc)
    {
	/*
	 * We only need to figure out what we do when we succeed!
	 */
	if(!id.equals("global")) {
	    mAppView.loadUrl("javascript:navigator.phonegap_geo.success(\"" + 
			     id + "\", " 
			     + loc.getLatitude() + ", " 
			     + loc.getLongitude() + ")");
	} else {
	    mAppView.loadUrl("javascript:Geolocation.gotCurrentPosition("+
			     + loc.getLatitude() + ", " 
			     + loc.getLongitude() + ")");
	}
    }
	
    void fail()
    {
	// Do we need to know why?  How would we handle this?
	mAppView.loadUrl("javascript:GeoLocation.fail()");
    }
	

    void stop() {
	providerListener.stop();
    }

    void pause() {
	providerListener.pause();
    }

    void resume() {
	providerListener.resume();
    }
    

    public Location getCurrentLocation() {
	return providerListener.getLocation();
    }
}
