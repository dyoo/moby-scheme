// depends on kernel.js


var plt = plt || {};
plt.world = plt.world || {};
plt.world.config = plt.world.config || {};



(function() {



    // augment: hash hash -> hash
    // Functionally extend a hashtable with another one.
    function augment(o, a) {
	var oo = {};
	for (var e in o) {
	    oo[e] = o[e];
	}
	for (var e in a) {
	    oo[e] = a[e];
	}
	return oo;
    }


    function WorldConfig() {
	// The following handler values are initially false until they're updated
	// by configuration.
      
	// A handler is a function:
	//     handler: world X Y ... -> Z


	this.vals = {
	    changeWorld: false,


	    initialEffect: false,

	    // onRedraw: world -> scene
	    onRedraw: false,

	    // onDraw: world -> (sexpof dom)
	    onDraw: false,

	    // onDrawCss: world -> (sexpof css-style)
	    onDrawCss: false,


	    // tickDelay: number
	    tickDelay: false,
	    // onTick: world -> world
	    onTick: false,
	    // onTickEffect: world -> effect
	    onTickEffect: false,

	    // onKey: world key -> world
	    onKey: false,
	    // onKeyEffect: world key -> effect
	    onKeyEffect : false,

	    // onTilt: world number number number -> world
	    onTilt: false,
	    // onTiltEffect: world number number number -> effect
	    onTiltEffect: false,

	    // onAcceleration: world number number number -> world
	    onAcceleration: false,
	    // onAccelerationEffect: world number number number -> effect
	    onAccelerationEffect: false,

	    // onShake: world -> world
	    onShake: false,
	    // onShakeEffect: world -> effect
	    onShakeEffect: false,

	    // onLocationChange: world number number -> world
	    onLocationChange : false,
	    // onLocationChangeEffect: world number number -> effect
	    onLocationChangeEffect: false,


	    // onAnnounce: world string X ... -> world
	    onAnnounce: false,
	    // onAnnounce: world string X ... -> effect
	    onAnnounceEffect: false,

	    // stopWhen: world -> boolean
	    stopWhen: false,
	    // stopWhenEffect: world -> effect
	    stopWhenEffect: false
	};
    }

  
    // WorldConfig.lookup: string -> handler
    // Looks up a value in the configuration.
    WorldConfig.prototype.lookup = function(key) {
	plt.Kernel.check(key, plt.Kernel.isString, "WorldConfig.lookup: key not a string");
	if (key in this.vals) {
	    return this.vals[key];
	} else {
	    throw Error("Can't find " + key + " in the configuration");
	}
    }
  


    // WorldConfig.updateAll: (hashof string handler) -> WorldConfig
    WorldConfig.prototype.updateAll = function(aHash) {
	var result = new WorldConfig();
	result.vals = augment(this.vals, aHash);
	return result;
    }

  
    plt.world.config.WorldConfig = WorldConfig;

    // The following global variable CONFIG is mutated by either
    // big-bang from the regular world or the one in jsworld.
    plt.world.config.CONFIG = new WorldConfig();


    // A handler is a function that consumes a config and produces a
    // config.


    //////////////////////////////////////////////////////////////////////

    function getNoneEffect() {
	return plt.world.Kernel.make_dash_effect_colon_none();
    }



    //////////////////////////////////////////////////////////////////////

    plt.world.config.Kernel = plt.world.config.Kernel || {};
    plt.world.config.Kernel.onRedraw = function(f) {
	plt.Kernel.check(f, plt.Kernel.isFunction, "on-redraw: first argument needs to be a function");
	return function(config) {
	    return config.updateAll({'onRedraw': f});
	};
    };



    plt.world.config.Kernel.initialEffect = function(effect) {
	// FIXME: add check for effect type.
	return function(config) {
	    return config.updateAll({'initialEffect': effect});
	};
    }


    plt.world.config.Kernel.onDraw = function(domHandler, styleHandler) {
	plt.Kernel.check(domHandler, plt.Kernel.isFunction, "on-draw: first argument needs to be a function");
	plt.Kernel.check(styleHandler, plt.Kernel.isFunction, "on-draw: second argument needs to be a function");
	return function(config) {
	    return config.updateAll({onDraw: domHandler,
				     onDrawCss : styleHandler});
	};
    };


    plt.world.config.Kernel.onTick = function(aDelay, f) {
	plt.Kernel.check(aDelay, plt.Kernel.isNumber, "on-tick: first argument needs to be a number");
	plt.Kernel.check(f, plt.Kernel.isFunction, "on-tick: second argument needs to be a function");
	return plt.world.config.Kernel.onTick_star_(aDelay, 
						    f,
						    function(w) { 
							return getNoneEffect(); });
    };

    plt.world.config.Kernel.onTick_star_ = function(aDelay, handler, effectHandler) {
	plt.Kernel.check(aDelay, plt.Kernel.isNumber, "on-tick*: first argument needs to be a number");
	plt.Kernel.check(handler, plt.Kernel.isFunction, "on-tick*: second argument needs to be a function");
	plt.Kernel.check(effectHandler, plt.Kernel.isFunction, "on-tick*: third argument needs to be a function");
	return function(config) {
	    var newVals = { onTick: handler,
			    onTickEffect: effectHandler,
			    tickDelay: (plt.types.NumberTower.toInteger(
									plt.types.NumberTower.multiply(
												       plt.types.Rational.makeInstance(1000, 1), 
												       aDelay)))
	    };
	    return config.updateAll(newVals);
	};

    };
  
    plt.world.config.Kernel.onTilt = function(handler) {
	plt.Kernel.check(handler, plt.Kernel.isFunction, "on-tilt: first argument needs to be a function");
	return plt.world.config.Kernel.onTilt_star_(handler, 
						    function(w, a, p, r) { 
							return getNoneEffect(); });
    };

    plt.world.config.Kernel.onTilt_star_ = function(handler, effectHandler) {
	plt.Kernel.check(handler, plt.Kernel.isFunction, "on-tilt*: first argument needs to be a function");
	plt.Kernel.check(effectHandler, plt.Kernel.isFunction, "on-tilt*: second argument needs to be a function");
	return function(config) {
	    return config.updateAll({onTilt : handler,
				     onTiltEffect : effectHandler});
	}
    };



    plt.world.config.Kernel.onAnnounce = function(handler) {
	plt.Kernel.check(handler, plt.Kernel.isFunction, "on-announce: first argument needs to be a function");
	return plt.world.config.Kernel.onAnnounce_star_(handler, 
							function(w, eventName, vals) { 
							    return getNoneEffect(); });
    };

    plt.world.config.Kernel.onAnnounce_star_ = function(handler, effectHandler) {
	plt.Kernel.check(handler, plt.Kernel.isFunction, "on-announce*: first argument needs to be a function");
	plt.Kernel.check(effectHandler, plt.Kernel.isFunction, "on-announce*: second argument needs to be a function");
	return function(config) {
	    return config.updateAll({onAnnounce : handler,
				     onAnnounceEffect : effectHandler});
	}
    };



    plt.world.config.Kernel.onAcceleration = function(handler) {
	plt.Kernel.check(handler, plt.Kernel.isFunction, "on-acceleration: first argument needs to be a function");
	return plt.world.config.Kernel.onAcceleration_star_(handler, 
							    function(w, a, p, r) { 
								return getNoneEffect(); });
    };

    plt.world.config.Kernel.onAcceleration_star_ = function(handler, effectHandler) {
	plt.Kernel.check(handler, plt.Kernel.isFunction, "on-acceleration*: first argument needs to be a function");
	plt.Kernel.check(effectHandler, plt.Kernel.isFunction, "on-acceleration*: second argument needs to be a function");
	return function(config) {
	    return config.updateAll({onAcceleration : handler,
				     onAccelerationEffect : effectHandler});
	}
    };


    plt.world.config.Kernel.onShake = function(handler) {
	plt.Kernel.check(handler, plt.Kernel.isFunction, "on-shake: first argument needs to be a function");
	return plt.world.config.Kernel.onShake_star_(handler, 
						     function(w, a, p, r) { 
							 return getNoneEffect(); });
    };

    plt.world.config.Kernel.onShake_star_ = function(handler, effectHandler) {
	plt.Kernel.check(handler, plt.Kernel.isFunction, "on-shake*: first argument needs to be a function");
	plt.Kernel.check(effectHandler, plt.Kernel.isFunction, "on-shake*: second argument needs to be a function");
	return function(config) {
	    return config.updateAll({onShake : handler,
				     onShakeEffect : effectHandler});
	}
    };



    plt.world.config.Kernel.onKey = function(handler) {
	plt.Kernel.check(handler, plt.Kernel.isFunction, "on-key: first argument needs to be a function");
	return plt.world.config.Kernel.onKey_star_(handler,
						   function(w, k) {
						       return getNoneEffect(); });
    };

    plt.world.config.Kernel.onKey_star_ = function(handler, effectHandler) {
	plt.Kernel.check(handler, plt.Kernel.isFunction, "on-key*: first argument needs to be a function");
	plt.Kernel.check(effectHandler, plt.Kernel.isFunction, "on-key*: second argument needs to be a function");
	return function(config) {
	    return config.updateAll({onKey : handler,
				     onKeyEffect: effectHandler});
	};
    };


    plt.world.config.Kernel.onLocationChange = function(handler) {
	plt.Kernel.check(handler, plt.Kernel.isFunction, "on-location-change: first argument needs to be a function");
	return plt.world.config.Kernel.onLocationChange_star_(handler,
							      function(w, latitude, longitude) {
								  return getNoneEffect(); });
    }

    plt.world.config.Kernel.onLocationChange_star_ = function(handler, effectHandler) {
	plt.Kernel.check(handler, plt.Kernel.isFunction, "on-location-change*: first argument needs to be a function");
	plt.Kernel.check(effectHandler, plt.Kernel.isFunction, "on-location-change*: second argument needs to be a function");

	return function(config) {
	    return config.updateAll({onLocationChange: handler,
				     onLocationChangeEffect: effectHandler});
	};
    }



    plt.world.config.Kernel.stopWhen = function(handler) {
	plt.Kernel.check(handler, plt.Kernel.isFunction, "stop-when: first argument needs to be a function");
	return function(config) {
	    return config.updateAll({'stopWhen': handler});
	};
    };

    plt.world.config.Kernel.stopWhen_star_ = function(stopHandler, stopEffect) {
	plt.Kernel.check(handler, plt.Kernel.isFunction, "stop-when*: first argument needs to be a function");
	plt.Kernel.check(effectHandler, plt.Kernel.isFunction, "stop-when*: second argument needs to be a function");

	return function(config) {
	    return config.updateAll({'stopWhen': stopHandler,
				     'stopWhenEffect' : stopEffect});
	};
    };


  




})();



