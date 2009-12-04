// Platform-specific stuff.
if (typeof(plt) === 'undefined') { var plt = {}; }

plt.lib = {};


(function () {

    plt.lib.Telephony = {};
    plt.lib.Telephony.getSignalStrengths = function() {
	return plt.platform.Platform.getInstance().getTelephonyService().getSignalStrengths();
    };
    //////////////////////////////////////////////////////////////////////
    
    plt.lib.Location = {};
    plt.lib.Location.getLatitude = function () {
	return plt.platform.Platform.getInstance().getLocationService().getLatitude();
    };

    plt.lib.Location.getLongitude = function () {
	return plt.platform.Platform.getInstance().getLocationService().getLongitude();
    };

    plt.lib.Location.getAttitude = function () {
	return plt.platform.Platform.getInstance().getLocationService().getAttitude();
    };

    plt.lib.Location.getBearing = function () {
	return plt.platform.Platform.getInstance().getLocationService().getBearing();
    };

    plt.lib.Location.getSpeed = function () {
	return plt.platform.Platform.getInstance().getLocationService().getSpeed();
    };

    plt.lib.Location.getDistanceBetween = function (lat1, long1, lat2, long2) {
	return plt.platform.Platform.getInstance().getLocationService().getDistanceBetween(lat1, long1, lat2, long2);
    };



    //////////////////////////////////////////////////////////////////////

    plt.lib.Net = {};
    plt.lib.Net.getUrl = function(url) {
	return plt.platform.Platform.getInstance().getNetworkService().getUrl(url);
    }


}());