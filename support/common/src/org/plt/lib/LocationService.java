package org.plt.lib;

import org.plt.MessageListener;

public interface LocationService {
    Object getLatitude();
    Object getLongitude();
    Object getAltitude();
    Object getBearing();
    Object getSpeed();

    void addListener(MessageListener listener);
}