var plt = plt || {};


(function() {

    plt.permission = {};




    function isLocationP(p) {
	return permission_colon_location_question_(p);
    }

    function isSendSmsP(p) {
	return permission_colon_send_dash_sms_question_(p);
    }

    function isReceiveSmsP(p) {
	return permission_colon_receive_dash_sms_question_(p);
    }

    function isTiltP(p) {
	return permission_colon_tilt_question_(p);
    }

    function isShakeP(p) {
	return permission_colon_shake_question_(p);
    }

    function isInternetP(p) {
	return permission_colon_internet_question_(p);
    }

    function isTelephonyP(p) {
	return permission_colon_telephony_question_(p);
    }

    function isWakeLockP(p) {
	return permission_colon_wake_dash_lock_question_(p);
    }

    function isOpenImageUrlP(p) {
	return permission_colon_open_dash_image_dash_url_question_(p);
    }



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
	    platform.getShakeService().addShakeListener(
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
	    img.onload = function() {
		plt.world.Kernel.FileImage.installInstance(path, img);
		keepGoing();
	    };
	    // If something goes wrong, we should show
	    // some default image.
	    img.onerror = function() {
		img.onerror = function() {
		    keepGoing();
		};
		img.src = "http://www.wescheme.org/images/broken.png";
	    };
	    img.src = path;
	} 

	else {
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