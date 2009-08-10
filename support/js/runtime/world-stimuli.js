// Feeds stimuli inputs into the world.


// Depends on kernel.js, types.js, world-config.js
var plt = plt || {};
plt.world = plt.world || {};


(function() {
    var stimuli = {}
    plt.world.stimuli = stimuli;


    // doStimuli: (world -> effect) (world -> world) -> void
    //
    // Processes a stimuli by compute the effect and applying it, and
    // computing a new world to replace the old.
    function doStimuli(computeEffectF, computeWorldF) {
	alert("doStimuli");
	change(function(w) {
	    if (computeEffectF) {
		var effect = computeEffectF(w);
		applyEffect(effect);
	    }    
	    if (computeWorldF) {
		return computeWorldF(w);
	    } else {
		return w;
	    }
	});
    }


    // Orientation change
    stimuli.onOrientation = function(azimuth, pitch, roll) {
	var onTilt = lookup("onTilt");
	var onTiltEffect = lookup("onTiltEffect");
	doStimuli(function(w) { return onTiltEffect([w, flt(azimuth), flt(pitch), flt(roll)]); },
		  function(w) { return onTilt([w, flt(azimuth), flt(pitch), flt(roll)]); });
    };


    // Accelerations
    stimuli.onAcceleration = function(x, y, z) {
	var onAcceleration = lookup('onAcceleration');
	var onAccelerationEffect = lookup('onAccelerationEffect');
	doStimuli(function(w) { return onAccelerationEffect([w, flt(x), flt(y), flt(z)]); },
		  function(w) { return onAcceleration([w, flt(x), flt(y), flt(z)]); });
    };


    // Shakes
    stimuli.onShake = function() {
	var onShake = lookup('onShake');
	var onShakeEffect = lookup('onShakeEffect');
	doStimuli(function(w) { return onShakeEffect([w]); },
		  function(w) { return onShake([w]); });
    };



    // Locations
    stimuli.onLocation = function(lat, lng) {
	var onLocationChange = lookup('onLocationChange');
	var onLocationChangeEffect = lookup('onLocationChangeEffect');
	doStimuli(function(w) { return onLocationChangeEffect([w, flt(lat), flt(lng)]); },
		  function(w) { return onLocationChange([w, flt(lat), flt(lng)]); });
    };


    // Keystrokes
    stimuli.onKey = function(e) {
	// getKeyCodeName: keyEvent -> String
	// Given an event, try to get the name of the key.
	function getKeyCodeName(e) {
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
	    } else {
		keyname = String.fromCharCode(code); 
	    }
	    return keyname;
	}
	var keyname = getKeyCodeName(e);
	
	var onKey = lookup('onKey');
	var onKeyEffect = lookup('onKeyEffect');
	doStimuli(function(w) { return onKeyEffect([w, keyname]); },
		  function(w) { return onKey([w, keyname]); });
    };


   
    // Shaking
    stimuli.onShake = function() {
	var onShake = lookup('onShake');
	var onShakeEffect = lookup('onShakeEffect');
	doStimuli(function(w) { return onShakeEffect([w]); },
		  function(w) { return onShake([w]); });
    };


    // Time ticks
    stimuli.onTick = function() {
	var onTick = lookup('onTick');
	var onTickEffect = lookup('onTickEffect');
	doStimuli(function(w) { return onTickEffect([w]); },
		  function(w) { return onTick([w]); });
    };



    // Announcements
    stimuli.onAnnounce = function(eventName, vals) {
	var valsList = plt.types.Empty.EMPTY;
	for (var i = 0; i < vals.length; i++) {
	    valsList = plt.types.Cons.makeInstance(vals[vals.length - i - 1], valsList);
	}

	var onAnnounce = lookup('onAnnounce');
	var onAnnounceEffect = lookup('onAnnounceEffect');	
	doStimuli(function(w) { return onTickEffect([w, eventName, valsList]); },
		  function(w) { return onTick([w, eventName, valsList]); });
    };





    //////////////////////////////////////////////////////////////////////
    // Helpers
    var flt = plt.types.FloatPoint.makeInstance;
    
    function change(f) {
	plt.world.config.CONFIG.lookup('changeWorld')(f);
    }

    function lookup(k) {
	return plt.world.config.CONFIG.lookup(k);
    }

    function applyEffect(e) {
	plt.world.Kernel.applyEffect(e);
    }

    //////////////////////////////////////////////////////////////////////

})();