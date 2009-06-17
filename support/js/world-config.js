org.plt.world = {};



org.plt.world.config = {
    // onRedraw: world -> scene
    onRedraw: false,

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



org.plt.world.config.Kernel = {};
org.plt.world.config.Kernel.onRedraw = function(handler) {
    return function() {
	org.plt.world.config.onRedraw = handler;    
    };
};
org.plt.world.config.Kernel.onTick = function(aDelay, handler) {
    return org.plt.world.config.Kernel.onTick_star_(aDelay, 
						    handler,
						    function(w) { 
							return make_dash_effect_colon_none(); });
};

org.plt.world.config.Kernel.onTick_star_ = function(aDelay, handler, effectHandler) {
    return function() {
	org.plt.world.config.tickDelay =
	    org.plt.types.NumberTower.toInteger
	(org.plt.types.NumberTower.multiply(
	    org.plt.types.Rational.makeInstance(1000, 1), 
	    aDelay));
	org.plt.world.config.onTick = handler;    
	org.plt.world.config.onTickEffect = effectHandler;
    };
};

org.plt.world.config.Kernel.onTilt = function(handler) {
    return org.plt.world.config.Kernel.onTilt_star_(handler, 
						    function(w, a, p, r) { 
							return make_dash_effect_colon_none(); });
};

org.plt.world.config.Kernel.onTilt_star_ = function(handler, effectHandler) {
    return function() {
	org.plt.world.config.onTilt = handler;
	org.plt.world.config.onTiltEffect = effectHandler;
    }
};



org.plt.world.config.Kernel.onAcceleration = function(handler) {
    return org.plt.world.config.Kernel.onAcceleration_star_(handler, 
							    function(w, a, p, r) { 
								return make_dash_effect_colon_none(); });
};

org.plt.world.config.Kernel.onAcceleration_star_ = function(handler, effectHandler) {
    return function() {
	org.plt.world.config.onAcceleration = handler;
	org.plt.world.config.onAccelerationEffect = effectHandler;
    }
};


org.plt.world.config.Kernel.onShake = function(handler) {
    return org.plt.world.config.Kernel.onShake_star_(handler, 
						     function(w, a, p, r) { 
							 return make_dash_effect_colon_none(); });
};

org.plt.world.config.Kernel.onShake_star_ = function(handler, effectHandler) {
    return function() {
	org.plt.world.config.onShake = handler;
	org.plt.world.config.onShakeEffect = effectHandler;
    }
};






org.plt.world.config.Kernel.onKey = function(handler) {
    return org.plt.world.config.Kernel.onKey_star_(handler,
						   function(w, k) {
						       return make_dash_effect_colon_none(); });
};

org.plt.world.config.Kernel.onKey_star_ = function(handler, effectHandler) {
    return function() {
	org.plt.world.config.onKey = handler;    
	org.plt.world.config.onKeyEffect = effectHandler; 
    };
};


org.plt.world.config.Kernel.onLocationChange = function(handler) {
    org.plt.world.config.Kernel.onLocationChange_star_(handler,
						       function(w, latitude, longitude) {
							   return make_dash_effect_colon_none(); });
}

org.plt.world.config.Kernel.onLocationChange_star_ = function(handler, effectHandler) {
    return function() {
	org.plt.world.config.onLocationChange = handler;    
	org.plt.world.config.onLocationChange = effectHandler;    
    };
}



org.plt.world.config.Kernel.stopWhen = function(handler) {
    return function() {
	org.plt.world.config.stopWhen = handler;    
    };
};


