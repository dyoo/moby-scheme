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
	this.networkService = chooseNetworkService();
	this.soundService = chooseSoundService();
	this.powerService = choosePowerService();
	this.smsService = chooseSmsService();
    };
    
    JavascriptPlatform.prototype.getTiltService = function() {
	return this.tiltService;
    };

    JavascriptPlatform.prototype.getShakeService = function() {
	return this.shakeService;
    };

    JavascriptPlatform.prototype.getLocationService = function() {
	return this.locationService;
    };

    JavascriptPlatform.prototype.getTelephonyService = function() {
	return this.telephonyService;
    };

    JavascriptPlatform.prototype.getNetworkService = function() {
	return this.networkService;
    };

    JavascriptPlatform.prototype.getSoundService = function() {
	return this.soundService;
    };
    
    JavascriptPlatform.prototype.getPowerService = function() {
	return this.powerService;
    };

    JavascriptPlatform.prototype.getSmsService = function() {
	return this.smsService;
    };




    // Dynamically choose which location service we grab
    function chooseLocationService() {
	if (isPhonegapAvailable()) {
    	    return new PhonegapLocationService();
	} else if (isGoogleGearsAvailable()) {
	    return new GoogleGearsLocationService();
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


    // isGoogleGearsAvailable: -> boolean
    // Returns true if Google Gears exists.
    function isGoogleGearsAvailable() {
	return (window.google && window.google.gears && true);
    }



    // isW3CLocationAvailable: -> boolean
    // Returns true if the W3C Location API is available.
    function isW3CLocationAvailable() {
	return (typeof(navigator != 'undefined') &&
		typeof(navigator.geolocation != 'undefined'));
		     
    }

    //////////////////////////////////////////////////////////////////////

    

    // Given two places on a globe, return the shortest distance between them in meters (uses spherical geometry)
    function roughDistanceBetween(lat1, long1, lat2, long2) {
	var toRad = function(deg) {
	    return deg * Math.PI / 180; 
	}
	var subExpr = function(x, y) {
	    return Math.pow(Math.sin((toRad(x) - toRad(y))/ 2),
			    2);
	};
	return 6378000 * 2 * Math.asin(
	    Math.min(1,
		     Math.sqrt(subExpr(lat1, lat2) +
			       (Math.cos(toRad(lat1)) *
				Math.cos(toRad(lat2)) *
				subExpr(long1, long2)))));
    }




    function GoogleGearsLocationService() {
	this.geo = google.gears.factory.create("beta.geolocation");
	this.listeners = [];
	this.currentPosition = {latitude: 0, 
				longitude: 0,
				altitude: 0,
				bearing: 0,
				speed: 0};
	this.watchId = false;
    };
    
    GoogleGearsLocationService.prototype.startService = function() {
	var that = this;
	function success(position) {
	    that.currentPosition.latitude = position.latitude;
	    that.currentPosition.longitude = position.longitude;
	    that.currentPosition.altitude = position.altitude;
	    for(var i = 0; i < that.listeners.length; i++) {
		that.listeners[i](position.latitude,
				  position.longitude);
	    }
	}
	this.watchId = this.geo.watchPosition(success);
    };

    GoogleGearsLocationService.prototype.shutdownService = function() {
	this.geo.clearWatch(this.watchId);
    };
    
    GoogleGearsLocationService.prototype.addLocationChangeListener = function(listener) {
	this.listeners.push(listener);
    };

    GoogleGearsLocationService.prototype.getLatitude = function () {
	return plt.types.FloatPoint.makeInstance(this.currentPosition.latitude);
    };

    GoogleGearsLocationService.prototype.getLongitude = function () {
	return plt.types.FloatPoint.makeInstance(this.currentPosition.longitude);
    };

    GoogleGearsLocationService.prototype.getAltitude = function () {
	return plt.types.FloatPoint.makeInstance(this.currentPosition.altitude);
    };

    GoogleGearsLocationService.prototype.getBearing = function () {
	return plt.types.FloatPoint.makeInstance(this.currentPosition.bearing);
    };

    GoogleGearsLocationService.prototype.getSpeed = function () {
	return plt.types.FloatPoint.makeInstance(this.currentPosition.speed);
    };

    GoogleGearsLocationService.prototype.getDistanceBetween = function (lat1, long1, lat2, long2) {

	return plt.types.FloatPoint.makeInstance(
	    roughDistanceBetween(lat1.toFloat(), 
				 long1.toFloat(), 
				 lat2.toFloat(),
				 long2.toFloat()));
    };






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

    NoOpLocationService.prototype.getAltitude = function () {
	return plt.types.Rational.ZERO;
    };

    NoOpLocationService.prototype.getBearing = function () {
	return plt.types.Rational.ZERO;
    };

    NoOpLocationService.prototype.getSpeed = function () {
	return plt.types.Rational.ZERO;
    };

    NoOpLocationService.prototype.getDistanceBetween = function (lat1, long1, lat2, long2) {
	return plt.types.FloatPoint.makeInstance(
	    roughDistanceBetween(lat1.toFloat(), 
				 long1.toFloat(), 
				 lat2.toFloat(),
				 long2.toFloat()));
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
	    that.currentPosition.latitude = parseFloat(pos.latitude);
	    that.currentPosition.longitude = parseFloat(pos.longitude);

    	    for ( var i = 0; i < that.locationListeners.length; i++ ) {
    		var listener = that.locationListeners[i];
    		listener(that.currentPosition.latitude,
			 that.currentPosition.longitude);
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

    PhonegapLocationService.prototype.getAltitude = function () {
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
	var monitor = function(pos) {
	    that.currentPosition.latitude = pos.coords.latitude;
	    that.currentPosition.longitude = pos.coords.longitude;
    	    for ( var i = 0; i < that.locationListeners.length; i++ ) {
    		var listener = that.locationListeners[i];
    		listener(pos.coords.latitude, pos.coords.longitude);
    	    }
	};

	var onError = function() {};
 
	if (typeof navigator.geolocation != 'undefined' &&
	    typeof navigator.geolocation.getCurrentPosition != 'undefined') {
	    navigator.geolocation.watchPosition(monitor);
	}

	setTimeout(function() {navigator.geolocation.getCurrentPosition(monitor)}, 10000);
    };

    W3CLocationService.prototype.shutdownService = function() {
	if (this.watchId) {
    	    navigator.geolocation.clearWatch(this.watchId);
	    this.watchId = false;
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

    W3CLocationService.prototype.getAltitude = function () {
	return plt.types.FloatPoint.makeInstance(this.currentPosition.altitude);
    };

    W3CLocationService.prototype.getBearing = function () {
	return plt.types.FloatPoint.makeInstance(this.currentPosition.bearing);
    };

    W3CLocationService.prototype.getSpeed = function () {
	return plt.types.FloatPoint.makeInstance(this.currentPosition.speed);
    };

    W3CLocationService.prototype.getDistanceBetween = function (lat1, long1, lat2, long2) {
	return plt.types.FloatPoint.makeInstance(
	    roughDistanceBetween(lat1.toFloat(), 
				 long1.toFloat(), 
				 lat2.toFloat(),
				 long2.toFloat()));
    };




    ////////////////////////////////////////////////////////////////////// 

    // FIXME: cleanup tilt so it's matching the other services.

    var accelListeners = [];
    var orientListeners = [];
    var accelId;
    var orientId;
    var shakeId;

    var accelSuccessCallback = function(accel) {
    	for ( var i = 0; i < accelListeners.length; i++ ) {
    	    accelListeners[i](parseFloat(accel.x), parseFloat(accel.y), parseFloat(accel.z));
    	}
    };
    
    var orientSuccessCallback = function(orient) {
	for ( var i = 0; i < orientListeners.length; i++ ) {
	    orientListeners[i](parseFloat(orient.azimuth), parseFloat(orient.pitch), parseFloat(orient.roll));
	}
    };



    var JavascriptTiltService = {
    	startService : function() {
    	    if (typeof Accelerometer != "undefined") {
    	    	accelId = navigator.accelerometer.watchAcceleration(accelSuccessCallback, function() {});
    	    	orientId = navigator.accelerometer.watchOrientation(orientSuccessCallback, function() {});
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

    PhonegapShakeService.prototype.startService = function() {
	var that = this;
	function success() {
	    for (var i = 0; i < that.listeners.length; i++) {
		that.listeners[i]();
	    }
	}
	function fail() {}
	this.shakeId = navigator.accelerometer.watchShake(success, fail);
    };

    PhonegapShakeService.prototype.shutdownService = function() {
	navigator.accelerometer.stopAllShakeWatches();
    };

    PhonegapShakeService.prototype.addListener = function(l) {
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





    //////////////////////////////////////////////////////////////////////

    function chooseNetworkService() {
	if (isPhonegapAvailable()) {
	    return new PhonegapNetworkService();
	} else {
	    return new WeSchemeNetworkService();
	}
    }


    function PhonegapNetworkService() {}
    PhonegapNetworkService.prototype.getUrl = function(aUrl) {
	return Device.getUrl(aUrl);
    };



    function WeSchemeNetworkService() {}
    WeSchemeNetworkService.prototype.getUrl = function(aUrl) {
	var req = new XMLHttpRequest();
	var url = "/networkProxy?url=" + encodeURIComponent(aUrl);
	req.open("GET", url, false);
	req.send(null);
	return req.responseText;
    };

    //////////////////////////////////////////////////////////////////////



    function chooseSoundService() {
	if (isPhonegapAvailable()) {
	    return new PhonegapSoundService();
	} else if (supportsHtml5()) {
	    return new Html5SoundService();
	} else {
	    return new GenericSoundService();
	}
    }

    function supportsHtml5() {
	return !!(document.createElement('audio').canPlayType);

    }


    function PhonegapSoundService() {
    };
    PhonegapSoundService.prototype.beep = function() {
	navigator.notification.beep(1);
    };
    PhonegapSoundService.prototype.playSoundUrl = function(url) {
    	navigator.audio.playMusic(url);
    };
    PhonegapSoundService.prototype.playDtmfTone = function(tone, duration) {
	navigator.audio.playDTMF(tone);
        setTimeout(function() { navigator.audio.stopDTMF() },
                   duration);
    };
    PhonegapSoundService.prototype.stopSoundUrl = function(url) {
    	navigator.audio.stopMusic(url);
    };
    PhonegapSoundService.prototype.pauseSoundUrl = function(url) {
    	navigator.audio.pauseMusic(url);
    };
    PhonegapSoundService.prototype.setVolume = function(volume) {
    	navigator.audio.setMusicVolume(volume);
    }
    PhonegapSoundService.prototype.raiseVolume = function() {
    	navigator.audio.increaseMusicVolume();
    };
    PhonegapSoundService.prototype.lowerVolume = function() {
    	navigator.audio.decreaseMusicVolume();
    };




    function Html5SoundService() {
	this.cachedUrls = {};
	this.baseVolume = 100;
    }
    Html5SoundService.prototype.beep = function() {
	alert("Beep");
    };

    Html5SoundService.prototype.playSoundUrl = function(url) {
	if (! this.cachedUrls[url]) {
	    this.cachedUrls[url] = new Audio(url);
	}
	var audio = this.cachedUrls[url];
	if (audio.ended || audio.paused) {
	    this.cachedUrls[url].play();
	}
    };
    
    Html5SoundService.prototype.playDtmfTone = function(tone, duration) {
	// Can't do anything here.
	alert("dtmf tone");
    };
    Html5SoundService.prototype.stopSoundUrl = function(url) {
	if (this.cachedUrls[url]) {
	    this.cachedUrls[url].pause();
	    this.cachedUrls[url].currentTime = 0;
	}
    };

    Html5SoundService.prototype.pauseSoundUrl = function(url) {
	if (this.cachedUrls[url]) {
	    this.cachedUrls[url].pause();
	}
    };
    
    Html5SoundService.prototype.setVolume = function(volume) {
	for (var url in this.cachedUrls) {
	    this.cachedUrls[url].volume = volume / 100.0;
	}
	this.baseVolume = volume;
    };

    Html5SoundService.prototype.raiseVolume = function() {
	this.setVolume(Math.min(this.baseVolume + 5, 100));
    };

    Html5SoundService.prototype.lowerVolume = function() {
	this.setVolume(Math.max(this.baseVolume - 5, 0));
    };





    function GenericSoundService() {
    }
    GenericSoundService.prototype.beep = function() {
	alert("Beep");
    };

    GenericSoundService.prototype.playSoundUrl = function(url) {
	// Can't do anything here.
	alert("sound url " + url);
    };


    GenericSoundService.prototype.playDtmfTone = function(tone, duration) {
	// Can't do anything here.
	alert("dtmf tone");
    };

    GenericSoundService.prototype.stopSoundUrl = function(url) {
    };
    GenericSoundService.prototype.pauseSoundUrl = function(url) {
    };
    GenericSoundService.prototype.setVolume = function(volume) {
    };
    GenericSoundService.prototype.raiseVolume = function() {
    };
    GenericSoundService.prototype.lowerVolume = function() {
    };


    //////////////////////////////////////////////////////////////////////
    function choosePowerService() {
	if (isPhonegapAvailable()) {
	    return new PhonegapPowerService();
	} else {
	    return new GenericPowerService();
	}
    }

    function PhonegapPowerService() {
	this.currentLockFlags = -1;
    }

    PhonegapPowerService.prototype.setWakeLock = function(flags) {
    	if (flags != this.currentLockFlags) {
    	    navigator.power.setWakeLock(flags);
    	    this.currentLockFlags = flags;
    	}
    };

    PhonegapPowerService.prototype.releaseWakeLock = function() {
    	if (this.currentLockFlags != -1) {
    	    navigator.power.releaseWakeLock();
    	    this.currentLockFlags = -1;
    	}
    };


    function GenericPowerService() {
    }
    GenericPowerService.prototype.setWakeLock = function(flags) {
    };
    GenericPowerService.prototype.releaseWakeLock = function() {
    };
 

    //////////////////////////////////////////////////////////////////////

    function chooseSmsService() {
	if (isPhonegapAvailable()) {
	    return new PhonegapSmsService();
	} else {
	    return new GenericSmsService();
	}
    }
    function PhonegapSmsService() {
    }
    PhonegapSmsService.prototype.send = function(address, msg) {
	navigator.sms.send(number, msg);
    };
    function GenericSmsService() {
    }
    GenericSmsService.prototype.send = function(address, msg) {
    };



})();