package org.plt.platform;

import org.plt.lib.LocationService;
import org.plt.lib.TiltService;
import org.plt.lib.SmsService;
import org.plt.lib.NetworkService;

public interface PlatformI {
    String getName();

    LocationService getLocationService();
    TiltService getTiltService();
    SmsService getSmsService();
    NetworkService getNetworkService();
}
