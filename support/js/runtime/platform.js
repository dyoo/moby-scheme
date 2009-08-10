// Platform-specific stuff.
var plt = plt || {};

plt.platform = {};

(function() { 
    plt.platform.Platform = {};
    
    var _platformSingleton = false;
    plt.platform.Platform.getInstance = function() {
	if(! _platformSingleton) {
	    _platformSingleton = new JavascriptPlatform();
	}
	
    	return _platformSingleton;
    };
    
    function JavascriptPlatform() {
    	this.tiltService = JavascriptTiltService;
	this.shakeService = chooseShakeService();
    	this.locationService = chooseLocationService();
	this.telephonyService = chooseTelephonyService();

    };
    
    JavascriptPlatform.prototype.getTiltService = function() {
	return this.tiltService;
    };

    JavascriptPlatform.prototype.getLocationService = function() {
	return this.locationService;
    };

    JavascriptPlatform.prototype.getTelephonyService = function() {
	return this.telephonyService;
    };


    // Dynamically choose which location service we grab
    function chooseLocationService() {
	if (isPhonegapAvailable()) {
    	    return new PhonegapLocationService();
	} else if (isW3CLocationAvailable()) {
    	    return new W3CLocationService();
	} else {
	    return new NoOpLocationService();
	}
    };
 

    // isPhonegapAvailable: -> boolean
    // Returns true if the Phonegap library exists.
    function isPhonegapAvailable() {
	return (typeof Device != 'undefined');
    }


    // isW3CLocationAvailable: -> boolean
    // Returns true if the W3C Location API is available.
    function isW3CLocationAvailable() {
	return (typeof(navigator != 'undefined') &&
		typeof(navigator.geolocation != 'undefined'));
		     
    }




    //////////////////////////////////////////////////////////////////////
    // NoOpLocationService does nothing.

    function NoOpLocationService() {
    };
    
    NoOpLocationService.prototype.startService = function() {
    };

    NoOpLocationService.prototype.shutdownService = function() {
    };
    
    NoOpLocationService.prototype.addLocationChangeListener = function(listener) {
    };

    NoOpLocationService.prototype.getLatitude = function () {
	return plt.types.Rational.ZERO;
    };

    NoOpLocationService.prototype.getLongitude = function () {
	return plt.types.Rational.ZERO;
    };

    NoOpLocationService.prototype.getAttitude = function () {
	return plt.types.Rational.ZERO;
    };

    NoOpLocationService.prototype.getBearing = function () {
	return plt.types.Rational.ZERO;
    };

    NoOpLocationService.prototype.getSpeed = function () {
	return plt.types.Rational.ZERO;
    };

    NoOpLocationService.prototype.getDistanceBetween = function (lat1, long1, lat2, long2) {
	// FIXME: do something smarter here!
	return plt.types.Rational.ZERO;
    };


    //////////////////////////////////////////////////////////////////////

    function PhonegapLocationService() {
	this.locationListeners = [];
	this.currentPosition = {latitude: 0, 
				longitude: 0,
				atttitude: 0,
				bearing: 0,
				speed: 0};
	this.watchId = false;
    };
    
    PhonegapLocationService.prototype.startService = function() {
	var that = this;
	function locSuccessCallback(pos) {
	    that.currentPosition.latitude = pos.latitude;
	    that.currentPosition.longitude = pos.longitude;

    	    for ( var i = 0; i < that.locationListeners.length; i++ ) {
    		var listener = that.locationListeners[i];
    		listener.locUpdate(pos.latitude, pos.longitude);
    	    }
	}; 
    	this.watchId = navigator.geolocation.watchPosition(locSuccessCallback, function() {}, {});
    };

    PhonegapLocationService.prototype.shutdownService = function() {
    	navigator.geolocation.clearWatch(this.watchId);
    };
    
    PhonegapLocationService.prototype.addLocationChangeListener = function(listener) {
    	this.locationListeners.push(listener);
    };

    PhonegapLocationService.prototype.getLatitude = function () {
	return plt.types.FloatPoint.makeInstance(navigator.geolocation.lastPosition.latitude);
    };

    PhonegapLocationService.prototype.getLongitude = function () {
	return plt.types.FloatPoint.makeInstance(navigator.geolocation.lastPosition.longitude);
    };

    PhonegapLocationService.prototype.getAttitude = function () {
	return plt.types.Rational.ZERO;
    };

    PhonegapLocationService.prototype.getBearing = function () {
	return plt.types.Rational.ZERO;
    };

    PhonegapLocationService.prototype.getSpeed = function () {
	return plt.types.Rational.ZERO;
    };

    PhonegapLocationService.prototype.getDistanceBetween = function (lat1, long1, lat2, long2) {
	return plt.types.FloatPoint.makeInstance(
	    navigator.geolocation.getDistanceBetween(lat1.toFloat(),
						     long1.toFloat(),
						     lat2.toFloat(),
						     long2.toFloat()));
    };










    //////////////////////////////////////////////////////////////////////

    // This version of the location service uses the W3C location api
    // if it's available.

    function W3CLocationService() {
	this.locationListeners = [];
	this.currentPosition = {latitude: 0, 
				longitude: 0,
				atttitude: 0,
				bearing: 0,
				speed: 0};
	this.watchId = false;
    };
    
    W3CLocationService.prototype.startService = function() {
	var that = this;
	function locSuccessCallback(pos) {
	    that.currentPosition.latitude = pos.coords.latitude;
	    that.currentPosition.longitude = pos.coords.longitude;

    	    for ( var i = 0; i < that.locationListeners.length; i++ ) {
    		var listener = that.locationListeners[i];
    		listener.locUpdate(pos.coords.latitude, pos.coords.longitude);
    	    }
	};
	function noOp() {}
 
	if (typeof navigator.geolocation != 'undefined' &&
	    typeof navigator.geolocation.watchPosition != 'undefined') {
    	    this.watchId = navigator.geolocation.watchPosition(locSuccessCallback, 
							       noOp, 
							       {});
	}

	if (typeof navigator.geolocation != 'undefined' &&
	    typeof navigator.geolocation.getCurrentPosition != 'undefined') {
	    navigator.geolocation.getCurrentPosition(locSuccessCallback, noOp);	    
	}
    };

    W3CLocationService.prototype.shutdownService = function() {
	if (this.watchId) {
    	    navigator.geolocation.clearWatch(this.watchId);
	}
    };
    
    W3CLocationService.prototype.addLocationChangeListener = function(listener) {
    	this.locationListeners.push(listener);
    };


    W3CLocationService.prototype.getLatitude = function () {
	return plt.types.FloatPoint.makeInstance(this.currentPosition.latitude);
    };

    W3CLocationService.prototype.getLongitude = function () {
	return plt.types.FloatPOint.makeInstance(this.currentPosition.longitude);
    };

    W3CLocationService.prototype.getAttitude = function () {
	return plt.types.FloatPoint.makeInstance(this.currentPosition.attitude);
    };

    W3CLocationService.prototype.getBearing = function () {
	return plt.types.FloatPoint.makeInstance(this.currentPosition.bearing);
    };

    W3CLocationService.prototype.getSpeed = function () {
	return plt.types.FloatPoint.makeInstance(this.currentPosition.speed);
    };

    W3CLocationService.prototype.getDistanceBetween = function (lat1, long1, lat2, long2) {
	return plt.types.Rational.ZERO;
	// FIXME: do something smarter here!
    };




    ////////////////////////////////////////////////////////////////////// 

    // FIXME: cleanup tilt so it's matching the other services.

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



    //////////////////////////////////////////////////////////////////////

    function chooseTelephonyService() {
	if (isPhonegapAvailable()) {
	    return new PhonegapTelephonyService();
	} else {
	    return new NoOpTelephonyService();
	}
    }

    //////////////////////////////////////////////////////////////////////
    function NoOpTelephonyService() {
    }

    NoOpTelephonyService.prototype.getSignalStrengths = function() {
	return plt.types.Empty.EMPTY;
    };


    //////////////////////////////////////////////////////////////////////
    function PhonegapTelephonyService() {
    }

    PhonegapTelephonyService.prototype.getSignalStrengths = function() {
	var result = plt.types.Empty.EMPTY;
	var infos = Device.getSignalStrengths();
	for (var i = 0; i < infos.length; i++) {
	    var info = infos.get(i);
	    result = plt.types.Cons.makeInstance(
		plt.Kernel.list([toNum(info.getId()),
				 toNum(info.getStrength())]),
		result);
	}
	return result;
    };





    //////////////////////////////////////////////////////////////////////

    function chooseShakeService() {
	if (isPhonegapAvailable()) {
	    return new PhonegapShakeService();
	} else {
	    return new NoOpShakeService();
	}
    }

    //////////////////////////////////////////////////////////////////////

    function PhonegapShakeService() {
	this.listeners = [];
    }

    PhoneGapShakeService.prototype.startService = function() {
	var that = this;
	function success() {
	    for (var i = 0; i < that.listeners.length; i++) {
		that.listeners[i]();
	    }
	}
	function fail() {}
	navigator.accelerometer.watchShake(success, fail);
    };

    PhoneGapShakeService.prototype.shutdownService = function() {
    };

    PhoneGapShakeService.prototype.addListener = function(l) {
	this.listeners.push(l);
    };




    //////////////////////////////////////////////////////////////////////

    function NoOpShakeService() {
    }

    NoOpShakeService.prototype.startService = function() {
    };

    NoOpShakeService.prototype.shutdownService = function() {
    };

    NoOpShakeService.prototype.addListener = function(l) {
    };

 
})();