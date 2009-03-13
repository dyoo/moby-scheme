package org.plt.lib;

import org.plt.world.OrientationChangeListener;
import org.plt.world.AccelerationChangeListener;

public interface TiltService {
    Object getXAcceleration();
    Object getYAcceleration();
    Object getZAcceleration();

    Object getAzimuth();
    Object getPitch();
    Object getRoll();

    void addOrientationChangeListener(OrientationChangeListener listener);
    void addAccelerationChangeListener(AccelerationChangeListener listener);

    void shutdownService();
    void startService();
}
