// Feeds stimuli inputs into the world.  The functions here
// are responsible for converting to Scheme values.
goog.provide('plt.world.stimuli');

(function() {
    var stimuli = {}
    plt.world.stimuli = stimuli;


    // doStimuli: (world -> effect) (world -> world) -> void
    //
    // Processes a stimuli by compute the effect and applying it, and
    // computing a new world to replace the old.
    var doStimuli = function(computeEffectF, computeWorldF, restArgs) {
	var effectUpdaters = [];
	try {
	    change(function(w) {
		var args = [w].concat(restArgs);
		if (computeEffectF) {
		    var effect = computeEffectF(args);
		    effectUpdaters = applyEffect(effect);
		}    
		if (computeWorldF) {
		    return computeWorldF(args);
		} else {
		    return w;
		}
	    });
	    
	    for (var i = 0; i < effectUpdaters.length; i++) {
		change(effectUpdaters[i]);
	    }
	} catch (e) { 
	    stimuli.onShutdown();
	}
    }


    // Orientation change
    stimuli.onTilt = function(azimuth, pitch, roll) {
	var onTilt = lookup("onTilt");
	var onTiltEffect = lookup("onTiltEffect");
	doStimuli(onTiltEffect, 
		  onTilt,
		  [flt(azimuth), flt(pitch), flt(roll)]);
    };


    // Accelerations
    stimuli.onAcceleration = function(x, y, z) {
	var onAcceleration = lookup('onAcceleration');
	var onAccelerationEffect = lookup('onAccelerationEffect');
	doStimuli(onAccelerationEffect, onAcceleration, [flt(x), flt(y), flt(z)]);
    };


    // Shakes
    stimuli.onShake = function() {
	var onShake = lookup('onShake');
	var onShakeEffect = lookup('onShakeEffect');
	doStimuli(onShakeEffect, onShake, []);
    };


    // Sms receiving
    stimuli.onSmsReceive = function(sender, message) {
	alert("in stimuli.onSmsReceive");
	alert("message = " + message);
	alert("sender = " + sender);
	var onSmsReceive = lookup('onSmsReceive');
	var onSmsReceiveEffect = lookup('onSmsReceiveEffect');
	doStimuli(onSmsReceiveEffect, onSmsReceive, [sender.toString(),
						     message.toString()]);
    };


    // Locations
    stimuli.onLocation = function(lat, lng) {
	var onLocationChange = lookup('onLocationChange');
	var onLocationChangeEffect = lookup('onLocationChangeEffect');
	doStimuli(onLocationChangeEffect, onLocationChange, [flt(lat), flt(lng)]);
    };



    // Keystrokes
    stimuli.onKey = function(e) {
	// getKeyCodeName: keyEvent -> String
	// Given an event, try to get the name of the key.
	var getKeyCodeName = function(e) {
	    var code = e.charCode || e.keyCode;
	    var keyname;
	    if (code == 37) {
		keyname = "left";
	    } else if (code == 38) {
		keyname = "up";
	    } else if (code == 39) {
		keyname = "right";
	    } else if (code == 40) {
		keyname = "down";
	    } else if (code == 32) {
		keyname = "space";
	    } else if (code == 13) {
		keyname = "enter";
	    } else {
		keyname = String.fromCharCode(code); 
	    }
	    return keyname;
	}
	var keyname = getKeyCodeName(e);
	var onKey = lookup('onKey');
	var onKeyEffect = lookup('onKeyEffect');
	doStimuli(onKeyEffect, onKey, [keyname]);
    };



    // Time ticks
    stimuli.onTick = function() {
	var onTick = lookup('onTick');
	var onTickEffect = lookup('onTickEffect');
	doStimuli(onTickEffect, onTick, []);
    };



    // Announcements
    stimuli.onAnnounce = function(eventName, vals) {
	var valsList = plt.types.Empty.EMPTY;
	for (var i = 0; i < vals.length; i++) {
	    valsList = plt.types.Cons.makeInstance(vals[vals.length - i - 1], valsList);
	}

	var onAnnounce = lookup('onAnnounce');
	var onAnnounceEffect = lookup('onAnnounceEffect');	
	doStimuli(onAnnounce, onAnnounceEffect, [eventName, valsList]);
    };



    // The shutdown stimuli: special case that forces a world computation to quit.
    stimuli.onShutdown = function() {	
	var shutdownWorld = lookup('shutdownWorld');
	if (shutdownWorld) {
	    shutdownWorld();
	}
    }




    //////////////////////////////////////////////////////////////////////
    // Helpers
    var flt = plt.types.FloatPoint.makeInstance;
    
    var lookup = function(k) {
	return plt.world.config.CONFIG.lookup(k);
    }

    var change = function(f) {
	if (lookup('changeWorld')) {
	    lookup('changeWorld')(f);
	}
    }

    // applyEffect: compound-effect: (arrayof (world -> world))
    var applyEffect = function(e) {
	return plt.world.Kernel.applyEffect(e);
    }

    //////////////////////////////////////////////////////////////////////

})();