// Platform-specific stuff.
var plt = plt || {};

plt.lib = {};


(function () {
    function toNum(x) {
	return plt.types.Rational.makeInstance(x, 1);
    }
    plt.lib.Telephony = {};
    plt.lib.Telephony.getSignalStrengths = function() {
	var result = plt.types.Empty.EMPTY;
	if (typeof Device != 'undefined') {
	    var infos = Device.getSignalStrengths();
	    for (var i = 0; i < infos.length; i++) {
		var info = infos.get(i);
		result = plt.types.Cons.makeInstance(
		    plt.Kernel.list([toNum(info.getId()),
				     toNum(info.getStrength())]),
		    result);
	    }
	}
	return result;
    };

    
    plt.lib.Location = {};
    plt.lib.Location.getLatitude = function () {
	if (typeof navigator.geolocation != 'undefined') {
	    return plt.types.FloatPoint.makeInstance(navigator.geolocation.lastPosition.latitude);
	} else {
	    return plt.types.Rational.ZERO;
	}
    };

    plt.lib.Location.getLongitude = function () {
	if (typeof navigator.geolocation != 'undefined') {
	    return plt.types.FloatPoint.makeInstance(navigator.geolocation.lastPosition.longitude);
	} else {
	    return plt.types.Rational.ZERO;
	}

    };

    plt.lib.Location.getAttitude = function () {
	if (typeof navigator.geolocation != 'undefined') {
	    return plt.types.Rational.ZERO;
	} else {
	    return plt.types.Rational.ZERO;
	}

    };

    plt.lib.Location.getBearing = function () {
	if (typeof navigator.geolocation != 'undefined') {
	    return plt.types.Rational.ZERO;
	} else {
	    return plt.types.Rational.ZERO;
	}

    };

    plt.lib.Location.getSpeed = function () {
	if (typeof navigator.geolocation != 'undefined') {
	    return plt.types.Rational.ZERO;
	} else {
	    return plt.types.Rational.ZERO;
	}

    };

    plt.lib.Location.getDistanceBetween = function (lat1, long1, lat2, long2) {
	if (typeof navigator.geolocation != 'undefined') {
	    return plt.types.FloatPoint.makeInstance(
		navigator.geolocation.getDistanceBetween(lat1.toFloat(),
							 long1.toFloat(),
							 lat2.toFloat(),
							 long2.toFloat()));
	} else {
	    return plt.types.Rational.ZERO;
	}
    };


}());