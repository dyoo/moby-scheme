package org.plt.platform;

import org.plt.lib.LocationService;
import org.plt.lib.TiltService;

public interface PlatformI {
    String getName();

    LocationService getLocationService();
    TiltService getTiltService();

    // Fill me in
}
