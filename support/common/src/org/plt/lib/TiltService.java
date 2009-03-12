package org.plt.lib;

import org.plt.world.TiltChangeListener;

public interface TiltService {
    Object getXTilt();
    Object getYTilt();
    Object getZTilt();
    void addTiltChangeListener(TiltChangeListener listener);

}
