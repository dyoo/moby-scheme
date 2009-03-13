package org.plt.lib;

import org.plt.world.TiltChangeListener;
import org.plt.world.AccelerationChangeListener;

public interface TiltService {
    Object getXAcceleration();
    Object getYAcceleration();
    Object getZAcceleration();

    Object getAzimuth();
    Object getPitch();
    Object getRoll();

    void addTiltChangeListener(TiltChangeListener listener);
    void addAccelerationChangeListener(AccelerationChangeListener listener);

    void shutdownService();
    void startService();
}
