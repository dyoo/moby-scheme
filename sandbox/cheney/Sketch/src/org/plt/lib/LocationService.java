package org.plt.lib;

import org.plt.world.LocationChangeListener;


public interface LocationService {
    Object getLatitude();
    Object getLongitude();
    Object getAltitude();
    Object getBearing();
    Object getSpeed();

    Object getDistanceBetween(String lat1, String long1, String lat2, String long2);

    void addLocationChangeListener(LocationChangeListener listener);
    void shutdownService();
    void startService();
}
