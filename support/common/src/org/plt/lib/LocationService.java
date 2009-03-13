package org.plt.lib;

import org.plt.world.LocationChangeListener;


public interface LocationService {
    Object getLatitude();
    Object getLongitude();
    Object getAltitude();
    Object getBearing();
    Object getSpeed();

    void addLocationChangeListener(LocationChangeListener listener);
    void shutdownService();
    void startService();
}
