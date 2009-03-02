package org.plt.lib;

import org.plt.LocationChangeListener;


public interface LocationService {
    Object getLatitude();
    Object getLongitude();
    Object getAltitude();
    Object getBearing();
    Object getSpeed();

    void addLocationChangeListener(LocationChangeListener listener);
}