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




    plt.permission.runStartupCode = function(p) {
	var platform = plt.platform.Platform.getInstance();
	var stimuli = plt.world.stimuli;

	
	if (isLocationP(p)) {
	    platform.getLocationService().startService();
	    platform.getLocationService().addLocationChangeListener(
		function(lat, lng) {
		    alert("location simuli");
		    stimuli.onLocation(lat, lng);
		});
	}
	
	if (isSendSmsP(p)) {
	    // Do nothing
	}
		
	if (isReceiveSmsP(p)) {
	    // Do nothing
	}

	if (isTiltP(p)) {
	    platform.getTiltService().startService();
	    platform.getTiltService().addOrientationChangeListener(
		function(azimuth, pitch, roll) {
		    stimuli.onTilt(azimuth, pitch, roll);
		});

	    platform.getTiltService().addAccelerationChangeListener(
		function(x, y, z) {
		    stimuli.onAcceleration(x, y, z);
		});

	}
	
	if (isShakeP(p)) {
	    platform.getShakeService().startService();
	    platform.getShakeService().addShakeListener(
		function(azimuth, pitch, roll) {
		    stimuli.onTilt(azimuth, pitch, roll);
		});
	}
	
	if (isInternetP(p)) {
	    // Do nothing
	}
	
	if (isTelephonyP(p)) {
	    // Do nothing
	}
	
	if (isWakeLockP(p)) {
	    // Do nothing
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
    };

    
})();