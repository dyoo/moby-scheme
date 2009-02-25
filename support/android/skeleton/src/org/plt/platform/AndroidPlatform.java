package org.plt.platform;


import android.app.Activity;
import android.content.Context;
import android.location.Location;
import android.location.LocationManager;
import android.location.LocationProvider;
import android.location.Criteria;

import org.plt.lib.LocationService;
import org.plt.types.*;

import java.util.Iterator;

public class AndroidPlatform implements PlatformI {
    private Activity activity;


    public String getName() {
	return "Android";
    }

    public void setActivity (Activity a) {
	this.activity = a;
    }



    public class AndroidLocationService 
	implements LocationService {
	private LocationManager manager;
	private LocationProvider provider;


	public AndroidLocationService() {

	    this.manager = (LocationManager) 
		activity.getSystemService(Context.LOCATION_SERVICE);
	    System.out.println("Providers: " +  manager.getAllProviders());
	    System.out.println("Providers: " +  manager.getAllProviders().size());
	    
	    Iterator iter = manager.getAllProviders().iterator();
	    while (iter.hasNext()) {
		LocationProvider provider = 
		    manager.getProvider((String) iter.next());
		System.out.println("provider: " + provider.getName());
		System.out.println("cost: " + provider.hasMonetaryCost());
	    }


	    Criteria c = new Criteria();
 	    c.setCostAllowed(true);
	    System.out.println("Probing for a provider");
	    this.provider = manager.getProvider
		(manager.getBestProvider(c, false));
	    System.out.println("Finally got a provider: " + this.provider);
	}

	// fixme!
	public Object getLatitude() {
	    if (this.provider == null) {
		return Logic.FALSE;
	    }
	    System.out.println("I have a provider: " + this.provider);
	    Location l =
		this.manager.getLastKnownLocation(this.provider.getName());
	    return FloatPoint.fromString("" + l.getLatitude());
// 	    // 41 44' N
// 	    return FloatPoint.fromString("44.73");
	}

	public Object getLongitude() {
	    if (this.provider == null) {
		return Logic.FALSE;
	    }
	    Location l =
		this.manager.getLastKnownLocation(this.provider.getName());
	    return FloatPoint.fromString("" + l.getLongitude());

// 	    // 71 26' W
// 	    return FloatPoint.fromString("-71.43");
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
	    
    }

    public LocationService getLocationService() {
	return new AndroidLocationService();
    }

}
