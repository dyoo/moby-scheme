goog.provide('plt.permission');


(function() {

    plt.permission = {};

    plt.Kernel.invokeModule("moby/runtime/permission-struct");
    var PEXPORTS = plt.Kernel.invokeModule('moby/runtime/permission-struct').EXPORTS;



    var isLocationP = function(p) {
	return PEXPORTS["permission:location?"](p);
    };

    var isSendSmsP = function(p) {
	return PEXPORTS["permission:send-sms?"](p);
    };

    var isReceiveSmsP = function(p) {
	return PEXPORTS["permission:receive-sms?"](p);
    };

    var isTiltP = function(p) {
	return PEXPORTS["permission:tilt?"](p);
    };

    var isShakeP = function(p) {
	return PEXPORTS["permission:shake?"](p);
    };

    var isInternetP = function(p) {
	return PEXPORTS["permission:internet?"](p);
    };

    var isTelephonyP = function(p) {
	return PEXPORTS["permission:telephony?"](p);
    };

    var isWakeLockP = function(p) {
	return PEXPORTS["permission:wake-lock?"](p);
    };

    var isForeignFunctionInterfaceP = function(p) {
	return PEXPORTS["permission:foreign-function-interface?"](p);
    };

    var isOpenImageUrlP = function(p) {
	return PEXPORTS["permission:open-image-url?"](p);
    };

    var isUniverseP = function(p) {
	return PEXPORTS["permission:universe?"](p);
    };


    // startupAllPermissions: arrayof permission (-> void) -> void
    // Evaluates all of the startup codes, and only after everything's
    // done do we evaluate the thunk.  Asynchronous.
    plt.permission.startupAllPermissions = function(perms, thunk) {
	if (perms.length > 0) {
	    var p = perms.pop();
	    plt.permission._runStartupCode(p, perms, thunk);
	} else {
	    thunk();
	}
    };


    plt.permission._runStartupCode = function(p, otherPerms, thunk) {
	var platform = plt.platform.Platform.getInstance();
	var stimuli = plt.world.stimuli;

	var keepGoing = function() {
	    setTimeout(function() {
		plt.permission.startupAllPermissions(otherPerms, thunk); 
	    },
		       0);
	};
	
	if (isLocationP(p)) {
	    platform.getLocationService().startService();
	    platform.getLocationService().addLocationChangeListener(
		function(lat, lng) {
		    stimuli.onLocation(lat, lng);
		});
	    keepGoing();
	}
	
	else if (isSendSmsP(p)) {
	    // Do nothing
	    keepGoing();
	}
		
	else if (isReceiveSmsP(p)) {
	    platform.getSmsService().startService();
	    platform.getSmsService().addListener(function(sender, msg) {
		    stimuli.onSmsReceive(sender, msg);
		});
	    keepGoing();
	}

	else if (isTiltP(p)) {
	    platform.getTiltService().startService();
	    platform.getTiltService().addOrientationChangeListener(
		function(azimuth, pitch, roll) {
		    stimuli.onTilt(azimuth, pitch, roll);
		});

	    platform.getTiltService().addAccelerationChangeListener(
		function(x, y, z) {
		    stimuli.onAcceleration(x, y, z);
		});
	    keepGoing();
	}
	
	else if (isShakeP(p)) {
	    platform.getShakeService().startService();
	    platform.getShakeService().addListener(
		function() {
		    stimuli.onShake();
		});
	    keepGoing();
	}
	
	else if (isInternetP(p)) {
	    // Do nothing
	    keepGoing();
	}
	
	else if (isTelephonyP(p)) {
	    // Do nothing
	    keepGoing();
	}
	
	else if (isWakeLockP(p)) {
	    // Do nothing
	    keepGoing();
	}

	else if (isForeignFunctionInterfaceP(p)) {
	    // FIXME:
	    // We should pop up a warning about this, or check
	    // if the user will permit?
	    keepGoing();
	}

	else if (isOpenImageUrlP(p)) {
	    var path = PEXPORTS['permission:open-image-url-url'](p);
	    var img = new Image();
	    var loadHandler = function() {
		// Detach, because looping animated gifs are defined to call onload
		// on every loop
		plt.Kernel.detachEvent(img, 'load', loadHandler);
		plt.world.Kernel.FileImage.installInstance(path, img);
		keepGoing();
	    };
	    plt.Kernel.attachEvent(img, "load", loadHandler);
	    // If something goes wrong, we should show
	    // some default image.
	    plt.Kernel.attachEvent(img, 
				   "error",
				   function() {
				       // Note that the image loading doesn't work, and
				       // keep going.
				       plt.world.Kernel.FileImage.installBrokenImage(path);
				       keepGoing();
				   });
	    img.src = path;

	} else {
	    keepGoing();
	}
    };



    plt.permission.runShutdownCode = function(p) {
	var platform = plt.platform.Platform.getInstance();


	if (isLocationP(p)) {
	    platform.getLocationService().shutdownService();
	}
	if (isSendSmsP(p)) {
	}
		
	if (isReceiveSmsP(p)) {
	}	 
	
	if (isTiltP(p)){
	    platform.getTiltService().shutdownService();
	}
	
	if (isShakeP(p)){
	}
	
	if (isInternetP(p)){
	}
	
	if (isTelephonyP(p)){
	}
	
	if (isWakeLockP(p)){
	}
	
	if (isForeignFunctionInterfaceP(p)) {
	}

	if (isOpenImageUrlP(p)) {
	    // Do nothing
	}
    };

    
})();