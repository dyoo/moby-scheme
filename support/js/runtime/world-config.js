// depends on kernel.js


if (typeof(plt) === 'undefined') { var plt = {}; }
plt.world = plt.world || {};
plt.world.config = plt.world.config || {};



(function() {



    // augment: hash hash -> hash
    // Functionally extend a hashtable with another one.
    var augment = function(o, a) {
	var oo = {};
	for (var e in o) {
	    oo[e] = o[e];
	}
	for (var e in a) {
	    oo[e] = a[e];
	}
	return oo;
    }



    var WorldConfig = function() {
	// The following handler values are initially false until they're updated
	// by configuration.
      
	// A handler is a function:
	//     handler: world X Y ... -> Z


	this.vals = {
	    // changeWorld: (world -> world) -> void
	    // When called, this will update the world based on the
	    // updater passed to it.
	    changeWorld: false,

	    // shutdownWorld: -> void
	    // When called, this will shut down the world computation.
	    shutdownWorld: false,

	    // initialEffect: effect
	    // The initial effect to invoke when the world computation
	    // begins.
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
	    stopWhenEffect: false,



	    //////////////////////////////////////////////////////////////////////
	    // For universe game playing

	    // connectToGame: string
	    // Registers with some universe, given an identifier
	    // which is a URL to a Universe server.
	    connectToGame: false,
	    onGameStart: false,
	    onOpponentTurn: false,
	    onMyTurn: false,
	    afterMyTurn: false,
	    onGameFinish: false
	};
    }

  
    // WorldConfig.lookup: string -> handler
    // Looks up a value in the configuration.
    WorldConfig.prototype.lookup = function(key) {
	plt.Kernel.check(key, plt.Kernel.isString, "WorldConfig.lookup", "string", 1);
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

    var getNoneEffect = function() {
	return make_dash_effect_colon_none();
    }



    //////////////////////////////////////////////////////////////////////


    var addStringMethods = function(f, name) {
	f.toWrittenString = function(cache) { return "(" + name + " ...)"; }
	f.toDisplayedString = f.toWrittenString;
	return f;
    }


    plt.world.config.Kernel = plt.world.config.Kernel || {};
    plt.world.config.Kernel.onRedraw = function(f) {
	plt.Kernel.check(f, plt.Kernel.isFunction, "on-redraw", "function", 1);
	return addStringMethods(
	    function(config) {
		return config.updateAll({'onRedraw': f});
	    }, "on-redraw");
    };



    plt.world.config.Kernel.initialEffect = function(effect) {
	// FIXME: add check for effect type.
	return addStringMethods(
	    function(config) {
		return config.updateAll({'initialEffect': effect});
	    }, "initial-effect");
    }


    plt.world.config.Kernel.onDraw = function(domHandler, styleHandler) {
	plt.Kernel.check(domHandler, plt.Kernel.isFunction, "on-draw", "function", 1);
	plt.Kernel.check(styleHandler, plt.Kernel.isFunction, "on-draw", "function", 2);
	return addStringMethods(
	    function(config) {
		return config.updateAll({onDraw: domHandler,
					 onDrawCss : styleHandler});
	    }, "on-draw");
    };


    plt.world.config.Kernel.onTick = function(aDelay, f) {
	plt.Kernel.check(aDelay, plt.Kernel.isNumber, "on-tick", "number", 1);
	plt.Kernel.check(f, plt.Kernel.isFunction, "on-tick", "function", 2);
	return plt.world.config.Kernel.onTick_star_(aDelay, 
						    f,
						    function(w) { 
							return getNoneEffect(); });
    };

    plt.world.config.Kernel.onTick_star_ = function(aDelay, handler, effectHandler) {
	plt.Kernel.check(aDelay, plt.Kernel.isNumber, "on-tick!", "number", 1);
	plt.Kernel.check(handler, plt.Kernel.isFunction, "on-tick!", "function", 2);
	plt.Kernel.check(effectHandler, plt.Kernel.isFunction, "on-tick!","function", 3);
	return addStringMethods(
	    function(config) {
		var newVals = { onTick: handler,
				onTickEffect: effectHandler,
				tickDelay: (plt.types.NumberTower.toFixnum(
				    plt.types.NumberTower.multiply(
					plt.types.Rational.makeInstance(1000, 1), 
					aDelay)))
			      };
		return config.updateAll(newVals);
	    }, "on-tick");
    };

  
    plt.world.config.Kernel.onTilt = function(handler) {
	plt.Kernel.check(handler, plt.Kernel.isFunction, "on-tilt", "function", 1);
	return plt.world.config.Kernel.onTilt_star_(handler, 
						    function(w, a, p, r) { 
							return getNoneEffect(); });
    };

    plt.world.config.Kernel.onTilt_star_ = function(handler, effectHandler) {
	plt.Kernel.check(handler, plt.Kernel.isFunction, "on-tilt!", "function", 1);
	plt.Kernel.check(effectHandler, plt.Kernel.isFunction, "on-tilt!", "function", 2);
	return addStringMethods(
	    function(config) {
		return config.updateAll({onTilt : handler,
					 onTiltEffect : effectHandler});
	    }, "on-tilt");
    };



    plt.world.config.Kernel.onAnnounce = function(handler) {
	plt.Kernel.check(handler, plt.Kernel.isFunction, "on-announce", "function", 1);
	return plt.world.config.Kernel.onAnnounce_star_(handler, 
							function(w, eventName, vals) { 
							    return getNoneEffect(); });
    };

    plt.world.config.Kernel.onAnnounce_star_ = function(handler, effectHandler) {
	plt.Kernel.check(handler, plt.Kernel.isFunction, "on-announce!", "function", 1);
	plt.Kernel.check(effectHandler, plt.Kernel.isFunction, "on-announce!", "function", 2);
	return addStringMethods(
	    function(config) {
		return config.updateAll({onAnnounce : handler,
					 onAnnounceEffect : effectHandler});
	    }, "on-announce");
    };



    plt.world.config.Kernel.onAcceleration = function(handler) {
	plt.Kernel.check(handler, plt.Kernel.isFunction, "on-acceleration", "function", 1);
	return plt.world.config.Kernel.onAcceleration_star_(handler, 
							    function(w, a, p, r) { 
								return getNoneEffect(); });
    };

    plt.world.config.Kernel.onAcceleration_star_ = function(handler, effectHandler) {
	plt.Kernel.check(handler, plt.Kernel.isFunction, "on-acceleration!", "function", 1);
	plt.Kernel.check(effectHandler, plt.Kernel.isFunction, "on-acceleration!", "function", 2);
	return addStringMethods(
	    function(config) {
		return config.updateAll({onAcceleration : handler,
					 onAccelerationEffect : effectHandler});
	    }, "on-acceleration");
    };


    plt.world.config.Kernel.onShake = function(handler) {
	plt.Kernel.check(handler, plt.Kernel.isFunction, "on-shake", "function", 1);
	return plt.world.config.Kernel.onShake_star_(handler, 
						     function(w, a, p, r) { 
							 return getNoneEffect(); });
    };

    plt.world.config.Kernel.onShake_star_ = function(handler, effectHandler) {
	plt.Kernel.check(handler, plt.Kernel.isFunction, "on-shake!", "function", 1);
	plt.Kernel.check(effectHandler, plt.Kernel.isFunction, "on-shake!", "function", 2);
	return addStringMethods(
	    function(config) {
		return config.updateAll({onShake : handler,
					 onShakeEffect : effectHandler});
	    }, "on-shake");
    };



    plt.world.config.Kernel.onKey = function(handler) {
	plt.Kernel.check(handler, plt.Kernel.isFunction, "on-key", "function", 1);
	return plt.world.config.Kernel.onKey_star_(handler,
						   function(w, k) {
						       return getNoneEffect(); });
    };

    plt.world.config.Kernel.onKey_star_ = function(handler, effectHandler) {
	plt.Kernel.check(handler, plt.Kernel.isFunction, "on-key!", "function", 1);
	plt.Kernel.check(effectHandler, plt.Kernel.isFunction, "on-key!", "function", 2);
	return addStringMethods(
	    function(config) {
		return config.updateAll({onKey : handler,
					 onKeyEffect: effectHandler});
	    }, "on-key");
    };


    plt.world.config.Kernel.onLocationChange = function(handler) {
	plt.Kernel.check(handler, plt.Kernel.isFunction, "on-location-change", "function", 1);
	return plt.world.config.Kernel.onLocationChange_star_(handler,
							      function(w, latitude, longitude) {
								  return getNoneEffect(); });
    }

    plt.world.config.Kernel.onLocationChange_star_ = function(handler, effectHandler) {
	plt.Kernel.check(handler, plt.Kernel.isFunction, "on-location-change!", "function", 1);
	plt.Kernel.check(effectHandler, plt.Kernel.isFunction, "on-location-change!", "function", 2);

	return addStringMethods(
	    function(config) {
		return config.updateAll({onLocationChange: handler,
					 onLocationChangeEffect: effectHandler});
	    }, "on-location-change");
    }



    plt.world.config.Kernel.stopWhen = function(handler) {
	plt.Kernel.check(handler, plt.Kernel.isFunction, "stop-when", "function", 1);
	return plt.world.config.Kernel.stopWhen_star_(handler,
						      function(w) {
							  return getNoneEffect(); });
    };


    plt.world.config.Kernel.stopWhen_star_ = function(stopHandler, effectHandler) {
	plt.Kernel.check(stopHandler, plt.Kernel.isFunction, "stop-when!", "function", 1);
	plt.Kernel.check(effectHandler, plt.Kernel.isFunction, "stop-when!", "function", 2);

	return addStringMethods(
	    function(config) {
		return config.updateAll({'stopWhen': stopHandler,
					 'stopWhenEffect' : effectHandler});
	    }, "stop-when");
    };


    // makeSimplePropertyUpdater: (string (X -> boolean) string string) -> (X -> handler)
    var makeSimplePropertyUpdater = function(propertyName,
					     propertyPredicate,
					     propertyTypeName,
					     updaterName) {
	return function(val) {
	    plt.Kernel.check(val, propertyPredicate, updaterName, propertyTypeName, 1);
	    return addStringMethods(
		function(config) {
		    return config.updateAll({propertyName: val });
		}, updaterName);
	}
    };

    // connects to the game
    plt.world.config.Kernel.connect_dash_to_dash_game = 
	makeSimplePropertyUpdater('connectToGame',
				  plt.Kernel.isString,
				  "string",
				  "connect-to-game");


    // Registers a handler for game-start events.
    plt.world.config.Kernel.on_dash_game_dash_start = 
	makeSimplePropertyUpdater('onGameStart',
				  plt.Kernel.isFunction,
				  "function",
				  "on-game-start");


    // Registers a handler for opponent-turn events.
    plt.world.config.Kernel.on_dash_opponent_dash_turn = 
	makeSimplePropertyUpdater('onOpponentTurn',
				  plt.Kernel.isFunction,
				  "function",
				  "on-opponent-turn");


    // Registers a handler for my turn.
    plt.world.config.Kernel.on_dash_my_dash_turn = 
	makeSimplePropertyUpdater('onMyTurn',
				  plt.Kernel.isFunction,
				  "function",
				  "on-my-turn");

    // Register a handler after I make a move.
    plt.world.config.Kernel.after_dash_my_dash_turn = 
	makeSimplePropertyUpdater('afterMyTurn',
				  plt.Kernel.isFunction,
				  "function",
				  "after-my-turn");

    plt.world.config.Kernel.on_dash_game_dash_finish = 
	makeSimplePropertyUpdater('onGameFinish',
				  plt.Kernel.isFunction,
				  "function",
				  "on-game-finish");




})();
