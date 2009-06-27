// depends on kernel.js


var org = org || {};
org.plt = org.plt || {};
org.plt.world = org.plt.world || {};
org.plt.world.config = org.plt.world.config || {};



(function() {


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
    this.vals = {
    changeWorld: false,

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

    // stopWhen: world -> boolean
    stopWhen: false
    };
  }


  WorldConfig.prototype.lookup = function(key) {
    return this.vals[key];
  }
  
  WorldConfig.prototype.update = function(key, val) {
    var result = new WorldConfig();
    var newDict = {};
    newDict[key] = val;
    result.vals = augment(this.vals, newDict);
    return result;
  }

  WorldConfig.prototype.updateAll = function(aHash) {
    var result = new WorldConfig();
    result.vals = augment(this.vals, aHash);
    return result;
  }

  
  org.plt.world.config.WorldConfig = WorldConfig;

  // The following global variable CONFIG is mutated by either
  // big-bang from the regular world or the one in jsworld.
  org.plt.world.config.CONFIG = new WorldConfig();


  function getNoneEffect() {
    return org.plt.world.Kernel.make_dash_effect_colon_none();
  }

  //////////////////////////////////////////////////////////////////////

  org.plt.world.config.Kernel = org.plt.world.config.Kernel || {};
  org.plt.world.config.Kernel.onRedraw = function(handler) {
    return function(config) {
      return config.update('onRedraw', handler);
    };
  };


  org.plt.world.config.Kernel.onDraw = function(domHandler, styleHandler) {
    return function(config) {
      return config.updateAll({onDraw: domHandler,
			       onDrawCss : styleHandler});
    };
  };


  org.plt.world.config.Kernel.onTick = function(aDelay, handler) {
    return org.plt.world.config.Kernel.onTick_star_(aDelay, 
						    handler,
						    function(w) { 
						      return getNoneEffect(); });
  };

  org.plt.world.config.Kernel.onTick_star_ = function(aDelay, handler, effectHandler) {
    return function(config) {
      var newVals = { onTick: handler,
		      onTickEffect: effectHandler,
		      tickDelay: (org.plt.types.NumberTower.toInteger(
				    org.plt.types.NumberTower.multiply(
				      org.plt.types.Rational.makeInstance(1000, 1), 
				      aDelay)))
		    };
      var result = config.updateAll(newVals);
      return result;
    };

  };
  
  org.plt.world.config.Kernel.onTilt = function(handler) {
    return org.plt.world.config.Kernel.onTilt_star_(handler, 
						    function(w, a, p, r) { 
						      return getNoneEffect(); });
  };

  org.plt.world.config.Kernel.onTilt_star_ = function(handler, effectHandler) {
    return function(config) {
      return config.updateAll({onTilt : handler,
                               onTiltEffect : effectHandler});
    }
  };



  org.plt.world.config.Kernel.onAcceleration = function(handler) {
    return org.plt.world.config.Kernel.onAcceleration_star_(handler, 
							    function(w, a, p, r) { 
							      return getNoneEffect(); });
  };

  org.plt.world.config.Kernel.onAcceleration_star_ = function(handler, effectHandler) {
    return function(config) {
      return config.updateAll({onAcceleration : handler,
                               onAccelerationEffect : effectHandler});
    }
  };


  org.plt.world.config.Kernel.onShake = function(handler) {
    return org.plt.world.config.Kernel.onShake_star_(handler, 
						     function(w, a, p, r) { 
						       return getNoneEffect(); });
  };

  org.plt.world.config.Kernel.onShake_star_ = function(handler, effectHandler) {
    return function(config) {
      return config.updateAll({onShake : handler,
                               onShakeEffect : effectHandler});
    }
  };



  org.plt.world.config.Kernel.onKey = function(handler) {
    return org.plt.world.config.Kernel.onKey_star_(handler,
						   function(w, k) {
						     return getNoneEffect(); });
  };

  org.plt.world.config.Kernel.onKey_star_ = function(handler, effectHandler) {
    return function(config) {
      return config.updateAll({onKey : handler,
                               onKeyEffect: effectHandler});
    };
  };


  org.plt.world.config.Kernel.onLocationChange = function(handler) {
    org.plt.world.config.Kernel.onLocationChange_star_(handler,
						       function(w, latitude, longitude) {
							 return getNoneEffect(); });
  }

  org.plt.world.config.Kernel.onLocationChange_star_ = function(handler, effectHandler) {
    return function(config) {
      return config.updateAll({onLocationChange: handler,
                               onLocationChangeEffect: effectHandler});
    };
  }



  org.plt.world.config.Kernel.stopWhen = function(handler) {
    return function() {
      org.plt.world.config.stopWhen = handler;    
    };
  };





})();



