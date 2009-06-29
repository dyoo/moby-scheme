// Platform-specific stuff.
var plt = plt || {};

plt.platform = {};

(function() { 
    plt.platform.Platform = {};
    plt.platform.Platform.getInstance = function() {
    	return JavascriptPlatform;
    };
 
    var JavascriptPlatform = {};
 
    JavascriptPlatform.getLocationService = function() {
    	return JavascriptLocationService;
    };
 
    JavascriptPlatform.getTiltService = function() {
    	return JavascriptTiltService;
    };


    var locSuccessCallback = function(pos) {
    	for ( var i = 0; i < locListeners.length; i++ ) {
    		var listener = locListeners[i];
    		listener.locUpdate(pos.latitude, pos.longitude);
    	}
    }; 

    var locationListeners = [];
    var locId;

    var JavascriptLocationService = {

    	startService : function() {
    		watchId = Geolocation.watchPosition(locSuccessCallback, function() {}, {});
    	},

    	shutdownService : function() {
    		Geolocation.clearWatch(locId);
    	},
 
    	addLocationChangeListener : function(listener) {
    		// When the location changes, notify the listeners
    		// by calling them with the updated lat/long values.
    		locationListeners.push(listener);
    	} 
    };
 

    var accelListeners = [];
    var orientListeners = [];
    var shakeListeners = [];
    var accelId;
    var orientId;
    var shakeId;

    var accelSuccessCallback = function(accel) {
    	for ( var i = 0; i < accelListeners.length; i++ ) {
    		accelListeners[i].accelUpdate(accel.x, accel.y, accel.z);
    	}
    };
    
    var orientSuccessCallback = function(orient) {
	for ( var i = 0; i < orientListeners.length; i++ ) {
		orientListeners[i].orientUpdate(orient.azimuth, orient.pitch, orient.roll);
	}
    };

    var shakeSuccessCallback = function() {
    	for ( var i = 0; i < shakeListeners.length; i++ ) {
    		shakeListeners[i].shakeUpdate();
    	}
    };

    var JavascriptTiltService = {
    	startService : function() {
    	    if (typeof Accelerometer != "undefined") {
    	    	accelId = navigator.accelerometer.watchAcceleration(accelSuccessCallback, function() {});
    	    	orientId = navigator.accelerometer.watchOrientation(orientSuccessCallback, function() {});
    	    	shakeId = navigator.accelerometer.watchShake(shakeSuccessCallback, function() {});
    	    }
    	},

    	shutdownService : function() {
    	    if (typeof Accelerometer != "undefined") {
    	    	navigator.accelerometer.clearWatch(accelId);
    	    	navigator.accelerometer.clearWatch(orientId);
		navigator.accelerometer.stopAllShakeWatches();
    	    }
    	},
 
    	addOrientationChangeListener : function(listener) {
    	    // push a listener onto the orientation listeners
    	    orientListeners.push(listener);
    	},

    	addAccelerationChangeListener : function(listener) {
    	    // push a listener onto the acceleration listeners
    	    accelListeners.push(listener);
    	},

    	addShakeListener : function(listener) {
    	    // push a listener onto the shake listeners
    	    shakeListeners.push(listener);
    	}
    };
 
})();