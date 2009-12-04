goog.provide('plt.permission');


(function() {

    plt.permission = {};




    var isLocationP = function(p) {
	return permission_colon_location_question_(p);
    };

    var isSendSmsP = function(p) {
	return permission_colon_send_dash_sms_question_(p);
    };

    var isReceiveSmsP = function(p) {
	return permission_colon_receive_dash_sms_question_(p);
    };

    var isTiltP = function(p) {
	return permission_colon_tilt_question_(p);
    };

    var isShakeP = function(p) {
	return permission_colon_shake_question_(p);
    };

    var isInternetP = function(p) {
	return permission_colon_internet_question_(p);
    };

    var isTelephonyP = function(p) {
	return permission_colon_telephony_question_(p);
    };

    var isWakeLockP = function(p) {
	return permission_colon_wake_dash_lock_question_(p);
    };

    var isOpenImageUrlP = function(p) {
	return permission_colon_open_dash_image_dash_url_question_(p);
    };

    var isUniverseP = function(p) {
	return permission_colon_universe_question_(p);
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
	    // Do nothing
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

	else if (isOpenImageUrlP(p)) {
	    var path = permission_colon_open_dash_image_dash_url_dash_url(p);
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

	if (isOpenImageUrlP(p)) {
	    // Do nothing
	}
    };

    
})();