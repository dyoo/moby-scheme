// The variables Device, Geo, Accel, Console, Args, and PlaylistPicker
// are all values defined by the DroidGap middleware.



/**
 * This class contains acceleration information
 * @constructor
 * @param {Number} x The force applied by the device in the x-axis.
 * @param {Number} y The force applied by the device in the y-axis.
 * @param {Number} z The force applied by the device in the z-axis.
 */
function Acceleration(x, y, z) {
    /**
     * The force applied by the device in the x-axis.
     */
    this.x = x;
    /**
     * The force applied by the device in the y-axis.
     */
    this.y = y;
    /**
     * The force applied by the device in the z-axis.
     */
    this.z = z;
    /**
     * The time that the acceleration was obtained.
     */
    this.timestamp = new Date().getTime();
}

/**
     * This class specifies the options for requesting acceleration data.
     * @constructor
     */
function AccelerationOptions() {
    /**
    	 * The timeout after which if acceleration data cannot be obtained the errorCallback
    	 * is called.
    	 */
    this.timeout = 1000;
}

/**
     * This class contains the orientation information from the phone
     * @constructor
     * @param {Number} azimuth The rotation around the Z axis (0<=azimuth<360).
     * 0 = North, 90 = East, 180 = South, 270 = West
     * @param {Number} pitch The rotation around X axis (-180<=pitch<=180),
     * with positive values when the z-axis moves toward the y-axis.
     * @param {Number} roll The rotation around Y axis (-90<=roll<=90),
     * with positive values when the z-axis moves toward the x-axis.
     */
function Orientation(azimuth, pitch, roll) {
    this.azimuth = azimuth;
    this.pitch = pitch;
    this.roll = roll;
}

/**
     * This class specifies the options for requesting orientation data.
     * @constructor
     */
function OrientationOptions() {
    /**
    	 * The timeout after which if orientation data cannot be obtained the errorCallback
    	 * is called.
    	 */
    this.timeout = 1000;
}


/**
     * This class provides access to device accelerometer data.
     * @constructor
     */
function Accelerometer() {
    /**
    	 * The last known acceleration.
    	 */
    this.lastAcceleration = null;

    /**
	 * The last known orientation.
	 */
    this.lastOrientation = null;

    this.shakeListeners = {};
    this.orientListeners = [];
    this.accelListeners = [];
    
    this.shakeIDCount = 0;
}

/**
     * Asynchronously aquires the current acceleration.
     * @param {Function} successCallback The function to call when the acceleration
     * data is available
     * @param {Function} errorCallback The function to call when there is an error 
     * getting the acceleration data.
     * @param {AccelerationOptions} options The options for getting the accelerometer data
     * such as timeout.
     */
/*
    Accelerometer.prototype.getCurrentAcceleration = function(successCallback, errorCallback, options) {
    	// If the acceleration is available then call success
    	// If the acceleration is not available then call error
    	
    	// Created for iPhone, Iphone passes back _accel obj litteral
    	if (_accel.x != null && _accel.y != null && _accel.z != null) {
    		var accel = new Acceleration(_accel.x,_accel.y,_accel.z);
    		Accelerometer.lastAcceleration = accel;
    		successCallback(accel);
    	}
	else {
		errorCallback();
	}
    }
*/

/**
     * Asynchronously aquires the current orientation.
     * @param {Function} successCallback The function to call when the orientation
     * data is available
     * @param {Function} errorCallback The function to call when there is an error 
     * getting the orientation data.
     * @param {OrientationOptions} options The options for getting the accelerometer data
     * such as timeout.
     */
/*
    Accelerometer.prototype.getCurrentOrientation = function(successCallback, errorCallback, options) {
	// If the orientation is available then call success
    	// If the orientation is not available then call error
    	
    	// Created for iPhone, Iphone passes back _accel obj litteral
    	if (_orient.azimuth != null && _orient.pitch != null && _orient.roll != null) {
    		var orient = new Orientation(_orient.azimuth,_orient.pitch,_orient.roll);
    		Accelerometer.lastOrientation = orient;
    		successCallback(orient);
    	}
	else {
		errorCallback();
	}
    }
*/

/**
     * Asynchronously aquires the acceleration repeatedly at a given interval.
     * @param {Function} successCallback The function to call each time the acceleration
     * data is available
     * @param {Function} errorCallback The function to call when there is an error 
     * getting the acceleration data.
     * @param {AccelerationOptions} options The options for getting the accelerometer data
     * such as timeout.
     */
/*    
    Accelerometer.prototype.watchAcceleration = function(successCallback, errorCallback, options) {
    	navigator.accelerometer.getCurrentAcceleration(successCallback, errorCallback, options);
    	// TODO: add the interval id to a list so we can clear all watches
     	var frequency = (options != undefined)? options.frequency : 1000;
    	return setInterval(function() {
    		navigator.accelerometer.getCurrentAcceleration(successCallback, errorCallback, options);
    	}, frequency);
    }
*/
/**
     * Asynchronously aquires the orientation repeatedly at a given interval.
     * @param {Function} successCallback The function to call each time the orientation
     * data is available
     * @param {Function} errorCallback The function to call when there is an error 
     * getting the orientation data.
     * @param {OrientationOptions} options The options for getting the accelerometer data
     * such as timeout.
     */
/*    
    Accelerometer.prototype.watchOrientation = function(successCallback, errorCallback, options) {
    	navigator.accelerometer.getCurrentOrientation(successCallback, errorCallback, options);
    	// TODO: add the interval id to a list so we can clear all watches
     	var frequency = (options != undefined)? options.frequency : 1000;
    	return setInterval(function() {
            navigator.accelerometer.getCurrentOrientation(successCallback, errorCallback, options);
    	}, frequency);
    }
*/    
/**
     * Clears the specified accelerometer watch.
     * @param {String} watchId The ID of the watch returned from #watchAcceleration.
     */
Accelerometer.prototype.clearWatch = function(watchId) {
    clearInterval(watchId);
}

if (typeof navigator.accelerometer == "undefined") navigator.accelerometer = new Accelerometer();



/**
     * This class provides access to the device media, interfaces to both sound and video
     * @constructor
     */
function Audio() {
    //   	this.src = src;
    this.callbacks = {};
}

Audio.prototype.play = function(file) {
}

Audio.prototype.pause = function(file) {
}

Audio.prototype.resume = function(file) {
}

Audio.prototype.stop = function(file) {
}

Audio.prototype.stopAll = function() {
}

Audio.prototype.increaseMusicVolume = function() {
}

Audio.prototype.decreaseMusicVolume = function() {
}

Audio.prototype.setMusicVolume = function() {
}

Audio.prototype.playDTMF = function(tone) {
}

Audio.prototype.stopDTMF = function() {
}


/**
     * This class contains information about any Audio errors.
     * @constructor
     */
function AudioError() {
    this.code = null,
    this.message = "";
}

AudioError.MEDIA_ERR_ABORTED 		= 1;
AudioError.MEDIA_ERR_NETWORK 		= 2;
AudioError.MEDIA_ERR_DECODE 		= 3;
AudioError.MEDIA_ERR_NONE_SUPPORTED 	= 4;


if (typeof navigator.audio == "undefined") navigator.audio = new Audio();


/**
     * This class provides access to the device camera.
     * @constructor
     */
function Camera() {
    
}

/**
     * 
     * @param {Function} successCallback
     * @param {Function} errorCallback
     * @param {Object} options
     */
Camera.prototype.getPicture = function(successCallback, errorCallback, options) {
    
}

if (typeof navigator.camera == "undefined") navigator.camera = new Camera();


/**
     * This class provides access to the device contacts.
     * @constructor
     */
function Contact() {
    this.name = "";
    this.phone = "";
    this.address = "";
}

/**
     * 
     * @param {Object} successCallback
     * @param {Object} errorCallback
     * @param {Object} options
     */
Contact.prototype.get = function(successCallback, errorCallback, options) {
    
}


function ContactManager() {
    // Dummy object to hold array of contacts
    this.contacts = [];
    this.timestap = new Date().getTime();
}

ContactManager.prototype.get = function(successCallback, errorCallback, options) {
    // Interface
}

if (typeof navigator.ContactManager == "undefined") navigator.ContactManager = new ContactManager();


/**
     * This class provides generic read and write access to the mobile device file system.
     */
function File() {
    /**
    	 * The data of a file.
    	 */
    this.data = "";
    /**
    	 * The name of the file.
    	 */
    this.name = "";
}

/**
     * Reads a file from the mobile device. This function is asyncronous.
     * @param {String} fileName The name (including the path) to the file on the mobile device. 
     * The file name will likely be device dependant.
     * @param {Function} successCallback The function to call when the file is successfully read.
     * @param {Function} errorCallback The function to call when there is an error reading the file from the device.
     */
File.prototype.read = function(fileName, successCallback, errorCallback) {
    
}

/**
     * Writes a file to the mobile device.
     * @param {File} file The file to write to the device.
     */
File.prototype.write = function(file) {
    
}

if (typeof navigator.file == "undefined") navigator.file = new File();


/**
     * This class provides access to device GPS data.
     * @constructor
     */
function Geolocation() {
    /**
    	 * The last known GPS position.
    	 */
    this.lastPosition = {latitude: 0, longitude: 0};

    this.listeners = [];
}

/**
     * Asynchronously aquires the current position.
     * @param {Function} successCallback The function to call when the position
     * data is available
     * @param {Function} errorCallback The function to call when there is an error 
     * getting the position data.
     * @param {PositionOptions} options The options for getting the position data
     * such as timeout.
     */
Geolocation.prototype.getCurrentPosition = function(successCallback, errorCallback, options) {
    // If the position is available then call success
    // If the position is not available then call error
}



// getDistanceBetween: number number number number -> number
// Returns the distance in meters between the two latitude/longitude pairs.
Geolocation.prototype.getDistanceBetween = function(lat1, long1, lat2, long2) {
    return Geo.getDistanceBetween(lat1, long1, lat2, long2);
}


/**
     * Asynchronously aquires the position repeatedly at a given interval.
     * @param {Function} successCallback The function to call each time the position
     * data is available
     * @param {Function} errorCallback The function to call when there is an error 
     * getting the position data.
     * @param {PositionOptions} options The options for getting the position data
     * such as timeout and the frequency of the watch.
     */
Geolocation.prototype.watchPosition = function(successCallback, errorCallback, options) {
    // Invoke the appropriate callback with a new Position object every time the implementation 
    // determines that the position of the hosting device has changed. 
    
    this.getCurrentPosition(successCallback, errorCallback, options);
    var frequency = (options != undefined)? options.frequency : 10000;
    
    var that = this;
    return setInterval(function() {
    	that.getCurrentPosition(successCallback, errorCallback, options);
    	//navigator.geolocation.getCurrentPosition(successCallback, errorCallback, options);
    }, frequency);
}


/**
     * Clears the specified position watch.
     * @param {String} watchId The ID of the watch returned from #watchPosition.
     */
Geolocation.prototype.clearWatch = function(watchId) {
    clearInterval(watchId);
}

navigator.phonegap_geo = new Geolocation();
if (typeof navigator.geolocation == "undefined") {
    navigator.geolocation = navigator.phonegap_geo;
}



/**
     * This class provides access to native mapping applications on the device.
     */
function Map() {
    
}

/**
     * Shows a native map on the device with pins at the given positions.
     * @param {Array} positions
     */
Map.prototype.show = function(positions) {
    
}

if (typeof navigator.map == "undefined") navigator.map = new Map();


/**
     * This class provides access to notifications on the device.
     */
function Notification() {
    
}

/**
     * Causes the device to blink a status LED.
     * @param {Integer} count The number of blinks.
     * @param {String} colour The colour of the light.
     */
Notification.prototype.blink = function(count, colour) {
    
}

/**
     * Causes the device to vibrate.
     * @param {Integer} mills The number of milliseconds to vibrate for.
     */
Notification.prototype.vibrate = function(mills) {
    
}

/**
     * Causes the device to beep.
     * @param {Integer} count The number of beeps.
     * @param {Integer} volume The volume of the beep.
     */
Notification.prototype.beep = function(count, volume) {
    
}

// TODO: of course on Blackberry and Android there notifications in the UI as well

if (typeof navigator.notification == "undefined") navigator.notification = new Notification();




/**
     * This class contains position information.
     * @param {Object} lat
     * @param {Object} lng
     * @param {Object} acc
     * @param {Object} alt
     * @param {Object} altacc
     * @param {Object} head
     * @param {Object} vel
     * @constructor
     */
function Position(lat, lng, acc, alt, altacc, head, vel) {
    /**
    	 * The latitude of the position.
    	 */
    this.latitude = lat;
    /**
    	 * The longitude of the position,
    	 */
    this.longitude = lng;
    /**
    	 * The accuracy of the position.
    	 */
    this.accuracy = acc;
    /**
    	 * The altitude of the position.
    	 */
    this.altitude = alt;
    /**
    	 * The altitude accuracy of the position.
    	 */
    this.altitudeAccuracy = altacc;
    /**
    	 * The direction the device is moving at the position.
    	 */
    this.heading = head;
    /**
    	 * The velocity with which the device is moving at the position.
    	 */
    this.velocity = vel;
    /**
    	 * The time that the position was obtained.
    	 */
    this.timestamp = new Date().getTime();
}

/**
     * This class specifies the options for requesting position data.
     * @constructor
     */
function PositionOptions() {
    /**
    	 * Specifies the desired position accuracy.
    	 */
    this.enableHighAccuracy = true;
    /**
    	 * The timeout after which if position data cannot be obtained the errorCallback
    	 * is called.
    	 */
    this.timeout = 10000;
}

/**
     * This class contains information about any GSP errors.
     * @constructor
     */
function PositionError() {
    this.code = null;
    this.message = "";
}

PositionError.UNKNOWN_ERROR = 0;
PositionError.PERMISSION_DENIED = 1;
PositionError.POSITION_UNAVAILABLE = 2;
PositionError.TIMEOUT = 3;



//////////////////////////////////////////////////////////////////////

// SMS management, both sending of SMS and receiving.

/**
     * This class provides access to the device SMS functionality.
     * @constructor
     */
function Sms() {
    this.listeners = [];
}

/**
     * Sends an SMS message.
     * @param {Integer} number The phone number to send the message to.
     * @param {String} msg The contents of the SMS message to send.
     * @param {Function} successCallback The function to call when the SMS message is sent.
     * @param {Function} errorCallback The function to call when there is an error sending the SMS message.
     * @param {PositionOptions} options The options for accessing the GPS location such as timeout and accuracy.
     */
Sms.prototype.send = function(number, msg) {
    Device.sendSmsMessage(number, msg);
};


// Add a SMS listener: whenever an sms message is received, notify all
// listeners
Sms.prototype.addListener = function(listener) {
    Device.smsStartService();
    this.listeners.push(listener);
};


// onReceiveSms: -> void
// Do not call this directly!
// The middleware will call this function, assigning appropriate values
// into the Args.
Sms.prototype.onReceiveSms = function() {
    var sender = "" + Args.get("sender");
    var message = "" + Args.get("message");
    for (var i = 0; i < this.listeners.length; i++) {
	(this.listeners[i])(sender, message);
    }
};

if (typeof navigator.sms == "undefined") { 
    navigator.sms = new Sms();
}


//////////////////////////////////////////////////////////////////////




/**
     * This class provides access to the telephony features of the device.
     * @constructor
     */
function Telephony() {
    
}

/**
     * Calls the specifed number.
     * @param {Integer} number The number to be called.
     */
Telephony.prototype.call = function(number) {
    
}

if (typeof navigator.telephony == "undefined") navigator.telephony = new Telephony();


// Android specific overrides here

Notification.prototype.vibrate = function(mills)
{
    Device.vibrate(mills);
}

/**
 * On the Android, we don't beep, we notify you with your 
 * notification!  We shouldn't keep hammering on this, and should
 * review what we want beep to do.
 */

Notification.prototype.beep = function(count, volume)
{
    Device.beep(count);
}


/**
 * Audio play, pause, stop, and result methods for the android audio
 *
 * Also methods to play and stop DTMF tones
 */
Audio.prototype.playMusic = function(file, endCallback) {
    Device.startPlayingAudio(file);
    this.callbacks[file] = endCallback;
}

Audio.prototype.pauseMusic = function(file) {
    Device.pauseAudio(file);
}

Audio.prototype.resumeMusic = function(file) {
    Device.resumeAudio(file);
}

Audio.prototype.stopMusic = function(file) {
    Device.stopPlayingAudio(file);
    delete this.callbacks[file];
}

Audio.prototype.stopAllMusic = function() {
    Device.stopAllAudio();
    this.callbacks = {};
}

Audio.prototype.musicFinished = function() {
    var file = Args.get("finishedMusicFile");
    //    Console.println("In musicFinshed for " + file);
    //    Console.println("typeof this.callbacks[file] = " + typeof this.callbacks[file]);

    if (typeof this.callbacks[file] != "undefined") {
    	this.callbacks[file]();
    }
    delete this.callbacks[file];
}

Audio.prototype.increaseMusicVolume = function(flags) {
    var theFlags = (typeof flags == "undefined")? Audio.FLAG_NONE : flags;
    Device.increaseMusicVolume(theFlags);
}

Audio.prototype.decreaseMusicVolume = function(flags) {
    var theFlags = (typeof flags == "undefined")? Audio.FLAG_NONE : flags;
    Device.decreaseMusicVolume(theFlags);
}

Audio.prototype.setMusicVolume = function(volume, flags) {
    var theFlags = (typeof flags == "undefined")? Audio.FLAG_NONE : flags;
    Device.setMusicVolume(volume, theFlags);
}

Audio.prototype.setRingerVolume = function(volume, flags) {
    var theFlags = (typeof flags == "undefined")? Audio.FLAG_NONE : flags;
    Device.setRingerVolume(volume, theFlags);
}


Audio.prototype.playDTMF = function(tone) {
    Device.playDTMF(tone);
}

Audio.prototype.stopDTMF = function() {
    Device.stopDTMF();
}

Audio.prototype.startMusicPlayer = function() {
    Device.startMusicPlayer();
}

// Flags for volume controls
Audio.FLAG_NONE = 0;
Audio.FLAG_SHOW_UI = 1;
Audio.FLAG_ALLOW_RINGER_MODES = 2;
Audio.FLAG_PLAY_SOUND = 4;
Audio.FLAG_REMOVE_SOUND_AND_VIBRATE = 8;
Audio.FLAG_VIBRATE = 16;


/**
 * Since we can't guarantee that we will have the most recent, we just try our best!
 *
 * Also, the API doesn't specify which version is the best version of the API
 */

Geolocation.prototype.getCurrentPosition = function(successCallback, errorCallback, options)
{
    Geolocation.global_success = successCallback;
    Geolocation.fail = errorCallback;
    Geo.getCurrentLocation();
}

// Run the global callback
Geolocation.gotCurrentPosition = function() {
    var lat = Args.get("gpsLat");
    var lng = Args.get("gpsLng");
    //  Console.println("Got position " + lat + ", " + lng);
    
    if (typeof lat == "undefined" || lat == null
        || typeof lng == "undefined" ||  lng == null)
    {
	this.fail();
    }
    else
    {
	p = {};
	p.latitude = lat;
	p.longitude = lng;
	this.global_success(p);
    }
}


/*
 * This turns on the GeoLocator class, which has two listeners.
 * The listeners have their own timeouts, and run independently of this process
 * In this case, we return the key to the watch hash
 */

Geolocation.prototype.watchPosition = function(successCallback, errorCallback, options)
{
    var frequency = (options != undefined)? (options.frequency || 10000) : 10000;

    //    Console.println("options.frequency = " + ((options != undefined)? options.frequency : 10000));

    if (!this.listeners)
    {
    	Console.println("Geoloc making listeners list in watchPosition");
    	this.listeners = [];
    }

    var key = this.listeners.push( {"success" : successCallback, "fail" : errorCallback }) - 1;
    
    //    Console.logd("Geoloc watchPosition", "Starting to watch position. key " + key + ", freq " + frequency);

    // TODO: Get the names of the method and pass them as strings to the Java.
    return Geo.start(frequency, key);
}

/*
 * Retrieve and stop this listener from listening to the GPS
 *
 */
Geolocation.prototype.success = function(key, lat, lng)
{
    var key = Args.get("gpsId");
    var lat = Args.get("gpsLat");
    var lng = Args.get("gpsLng");

    //    Console.println("Success for finding location " + lat + ", " + lng + " with key " + key);
    //    Console.println("typeof this.listeners = " + (typeof this.listeners));

    if (typeof key == "undefined" || key == null) {
        Console.logd("PhoneGap", "Geolocation key undefined in Geolocation.success");
    }
    else if (lat == null || lng == null) {
            this.listeners[key].fail();
    }
    else {
        p = {};
        p.latitude = lat;
        p.longitude = lng;
        this.lastPosition = p;
        this.listeners[key].success(p);
    }
}

Geolocation.prototype.fail = function(key)
{
    this.listeners[key].fail();
}

Geolocation.prototype.clearWatch = function(watchId)
{
    Geo.stop(watchId);
}

/* Identical to the iPhone, except we have to create this in the JS */
/*
_accel = {};
_accel.x = null;
_accel.y = null;
_accel.z = null;

_orient = {};
_orient.azimuth = null;
_orient.pitch = null;
_orient.roll = null;

// Variables containing the system time of the last rapid acceleration change
// and the last recorded shake.
var lastRapidChange = -1;
var lastShake = -1;

// the magnitude of the difference vector required to be considered a "rapid acceleration change"
var changeMagnitude = 7.25;
// the time window for two rapid acceleration changes to be considered a shake (milliseconds)
var shakeSpan = 250;
// the minimum time between shakes (milliseconds)
var shakeDelay = 1000;

function isRapidChange(accel, x, y, z) {
    var diff = {};
    diff.x = accel.x - x;
    diff.y = accel.y - y;
    diff.z = accel.z - z;
    return Math.sqrt( (diff.x * diff.x) + (diff.y * diff.y) + (diff.z * diff.z) ) > changeMagnitude;
}

function gotAcceleration(x,y,z) {
//    Console.println("Got accel: " + x + ", " + y + ", " + z);
    if ( isRapidChange(_accel, x, y, z) ) {
    	var curTime = new Date().getTime();
//    	Console.println("Rapid accel change at " + curTime);
    	if (curTime - lastRapidChange < shakeSpan && curTime - lastShake > shakeDelay) {
//    	    Console.println("Phone got shaken at " + curTime);
    	    lastShake = curTime;
    	    try {
    	    	navigator.accelerometer.gotShaken();
    	    } catch (e) {
    	    	Console.logd("PhoneGap", e.toString());
    	    }
    	}
//    	Console.println("Changing lastRapidChange to " + curTime);
    	lastRapidChange = curTime;
    }

    _accel.x = x;
    _accel.y = y;
    _accel.z = z;
}

function gotOrientation(azimuth, pitch, roll) {
    _orient.azimuth = azimuth;
    _orient.pitch = pitch;
    _orient.roll = roll;
}
*/

function emptyHash(h) {
    for (k in h) {
    	return false;
    }
    return true;
}

Accelerometer.prototype.allListenersEmpty = function() {
    return (emptyHash(this.shakeListeners) &&
            this.orientListeners.length == 0 &&
            this.accelListeners.length == 0);
}

// the magnitude of the difference vector required to be considered a "rapid acceleration change"
var defChangeMagnitude = 7.25;
// the time window for two rapid acceleration changes to be considered a shake (milliseconds)
var defShakeSpan = 250;
// the minimum time between shakes (milliseconds)
var defShakeDelay = 1000;

Accelerometer.prototype.watchShake = function(successCallback, errorCallback, options) {
    //    if (!this.shakeListeners) {
    //    	this.shakeListeners = [];
    //    }
    Accel.start();

    var shakeUID = "shakeListener" + (this.shakeIDCount++);
    var changeMag = defChangeMagnitude;
    var shakeSpan = defShakeSpan;
    var shakeDelay = defShakeDelay;

    if (typeof options != "undefined") {
    	if (typeof options.changeMagnitude != "undefined")
    	    changeMag = options.changeMagnitude;
    	if (typeof options.shakeSpan != "undefined")
    	    shakeSpan = options.shakeSpan;
    	if (typeof options.shakeDelay != "undefined")
    	    shakeDelay = options.shakeDelay;
    }

    var callBacks = { "success" : successCallback, "fail" : errorCallback };
    this.shakeListeners[shakeUID] = callBacks;

    Accel.addShakeListener(shakeUID, changeMag, shakeSpan, shakeDelay);

    var that = this;
    return function() {
    	if (typeof that.shakeListeners[shakeUID] != "undefined") {
	    delete that.shakeListeners[shakeUID];
	}

	Accel.removeShakeListener(shakeUID);
    	
	//    	var index = that.shakeListeners.indexOf(callBacks);
	//    	Console.println("Removing the shake listener at index " + index);
	//    	if (index != -1)
	//   	    that.shakeListeners.splice(index, 1);

    	if (navigator.accelerometer.allListenersEmpty()) {
    	    Console.println("Stopping Accel");
    	    Accel.onStop();
    	}
    };
}

Accelerometer.prototype.gotShaken = function() {
    //    Console.println("gotShaken called");

    var shakenUIDs = Args.get("shakeIDs");

    for (var i = 0; i < shakenUIDs.size(); i++) {
	var callback = this.shakeListeners[shakenUIDs.get(i)];

	if (typeof callback != "undefined") {
	    callback.success();
	}
    }

    //    for (var i = 0; i < this.shakeListeners.length; i++) {
    //    	this.shakeListeners[i].success();
    //    }
}

//Accelerometer.prototype.stopShakeWatch = function(shakeId) {
//    if (this.shakeListeners && this.shakeListeners[shakeId]) {
//	this.shakeListners[shakeId] = { "success" : function() {}, "fail" : function() {} };
//    }
//    Accel.stop();
//}

//Accelerometer.prototype.stopAllShakeWatches = function() {
//    this.shakeListeners = [];
//    if (navigator.accelerometer.allListenersEmpty())
//    	Accel.stop();
//}

Accelerometer.prototype.getCurrentAcceleration = function(successCallback, errorCallback, options) {
    //    Console.println("getting current acceleration");
    
    var accel = new Acceleration( Accel.getX(), Accel.getY(), Accel.getZ() );
    Accelerometer.lastAcceleration = accel;
    successCallback(accel);
}

Accelerometer.prototype.getCurrentOrientation = function(successCallback, errorCallback, options) {
    //    Console.println("getting current orientation");
    
    var orient = new Orientation( Accel.getAzimuth(), Accel.getPitch(), Accel.getRoll() );
    Accelerometer.lastOrientation = orient;
    successCallback(Accel.getAzimuth(), Accel.getPitch(), Accel.getRoll());
}

//Accelerometer.base_method = Accelerometer.prototype.watchAcceleration
Accelerometer.prototype.watchAcceleration = function(successCallback, errorCallback, options)
{
    Accel.start();

    navigator.accelerometer.getCurrentAcceleration(successCallback, errorCallback, options);
    var frequency = (options != undefined)? options.frequency : 100;

    var id = setInterval(function() {
    	navigator.accelerometer.getCurrentAcceleration(successCallback, errorCallback, options);
    }, frequency);
    this.accelListeners.push(id);
    
    var that = this;
    return function () {Accelerometer.clearTypeWatch(that.accelListeners, id);};
}

//Accelerometer.base_orient_method = Accelerometer.prototype.watchOrientation;
Accelerometer.prototype.watchOrientation = function(successCallback, errorCallback, options)
{
    if (typeof Accel === 'undefined' || typeof(navigator.accelerometer) === 'undefined') {
	alert("No accelerometer found");
	return function() {
	    alert("watchOrientation off");
	};
    }

//    alert(navigator.accelerometer);
//     alert("calling getCurrentOrientation");
//     navigator.accelerometer.getCurrentOrientation(successCallback, errorCallback, options);
//     var frequency = (options != undefined)? options.frequency : 100;
//     navigator.accelerometer.getCurrentOrientation(successCallback, errorCallback, options);
     var frequency = (options != undefined)? options.frequency : 100;
    var id = setInterval(function() {
 	navigator.accelerometer.getCurrentOrientation(successCallback, errorCallback, options);
    }, frequency);

    this.orientListeners.push(id);
    
    var that = this;
    return function () {Accelerometer.clearTypeWatch(that.orientListeners, id);};
}

Accelerometer.clearTypeWatch = function(list, watchId) {
    clearInterval(watchId);
    var index = list.indexOf(watchId);
    if (index != -1)
    	list.splice(index, 1);

    if (navigator.accelerometer.allListenersEmpty()) {
    	Console.println("Stopping accel");
    	Accel.onStop();
    }
}



Accelerometer.prototype.clearAllWatches = function() {
    Console.println("clearAllWatches");

    // clear the shake listeners
    this.shakeListeners = {};
    
    // now clear acceleration listeners
    for (var i = 0; i < this.accelListeners.length; i++) {
    	clearInterval(this.accelListeners[i]);
    }
    
    // now clear orientation watches
    for (var j = 0; j < this.orientListeners.length; j++) {
    	clearInterval(this.orientListeners[j]);
    }
    
    // finally, stop the accelerometer
    Accel.onStop();
}





/**
 * Used to keep the phone awake while the app is running
 */
function Power() {
}

Power.prototype.finish = function() {
    Device.finish();
}

Power.FULL_WAKE_LOCK = 26;
Power.PARTIAL_WAKE_LOCK = 1;
Power.SCREEN_BRIGHT_WAKE_LOCK = 10;
Power.SCREEN_DIM_WAKE_LOCK = 6;


// setWakeLock: (or undefined number) -> void
Power.prototype.setWakeLock = function(flags) {
    //    Console.println("Setting a wake lock");
    var lockType = (typeof flags == "undefined")? Power.SCREEN_DIM_WAKE_LOCK : flags;
    Device.setWakeLock(lockType);
    //    Console.println("WakeLock set");
}

Power.prototype.releaseWakeLock = function() {
    //    Console.println("Releasing wake lock");
    Device.releaseWakeLock();
}

if (typeof navigator.power == "undefined") navigator.power = new Power();









function DialogPickers() {
    this.listeners = [];
}

DialogPickers.prototype.pickPlaylist = function(callback) {
    PlaylistPicker.requestPickPlaylist();
    this.listeners.push(callback);
};


DialogPickers.prototype.notifyPlaylistPicked = function() {
    var record = PlaylistPicker.getPlaylistRecord();
    for (var i = 0; i < this.listeners.length; i++) {
	this.listeners[i](record);
    }
    this.listeners = [];
}

DialogPickers.prototype.notifyPlaylistCanceled = function() {
    this.listeners = [];
}


if (typeof navigator.dialogPickers == 'undefined') {
    navigator.dialogPickers = new DialogPickers();
}
