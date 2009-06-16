org.plt.WorldKernel = {};
(function() {
    
    var world;
    var worldListeners = [];
    var stopped;
    var timerInterval = false;


    // Inheritance from pg 168: Javascript, the Definitive Guide.
    function heir(p) {
	function f() {}
	f.prototype = p;
	return new f();
    }


    // changeWorld: world -> void
    // Changes the current world to newWorld.
    function changeWorld(newWorld) {
	world = newWorld;
	notifyWorldListeners();
    }


    // updateWorld: (world -> world) -> void
    // Public function: update the world, given the old state of the
    // world.
    org.plt.WorldKernel.updateWorld = function(updater) {
	var newWorld = updater(world);
	changeWorld(newWorld);
    }


    // notifyWorldListeners: -> void
    // Tells all of the world listeners that the world has changed.
    function notifyWorldListeners() {
	var i;
	for (i = 0; i < worldListeners.length; i++) {
	    worldListeners[i](world);
	}
    }

    // addWorldListener: (world -> void) -> void
    // Adds a new world listener: whenever the world is changed, the aListener
    // will be called with that new world.
    function addWorldListener(aListener) {
	worldListeners.push(aListener);
    }
    

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


    // resetWorld: -> void
    // Resets all of the world global values.
    function resetWorld() {
	if (timerInterval) {
	    clearInterval(timerInterval);
	    timerInterval = false;
	}
	stopped = false;
	worldListeners = [];
    }


    function getBigBangWindow(width, height) {
        if (window.document.getElementById("canvas") != undefined) {
	    return window;
	}

        var newWindow = window.open(
	    "big-bang.html",
	    "big-bang");
	    //"toolbar=false,location=false,directories=false,status=false,menubar=false,width="+width+",height="+height);
	if (newWindow == null) { 
            throw new Error("Error: Not allowed to create a new window."); }

	return newWindow;
    }


    // bigBang: number number world (arrayof (-> void)) -> void
    // Begins a world computation.  The initial world is aWorld, and handlers
    // register other reactive functions (timer tick, key press, etc.) which
    // will change the world.
    org.plt.WorldKernel.bigBang = function(width, height, aWorld, handlers) {
	var i;
	var newWindow = getBigBangWindow(width, height);
	var canvas = 
	    newWindow.document.getElementById("canvas");
	canvas.width = width;
	canvas.height = height;

	resetWorld();


	for (i = 0; i < handlers.length; i++) {
	    handlers[i]();
	}

	if (org.plt.world.config.onKey) {
	    newWindow.onkeypress = function(e) {
		if (! stopped) {
		    var keyname = getKeyCodeName(e);
		    if (org.plt.world.config.onKeyEffect) {
			var effect = org.plt.world.config.onKeyEffect([world, keyname]);
			org.plt.WorldKernel.applyEffect(effect);
                    }

		    var newWorld = org.plt.world.config.onKey([world, keyname]);
		    changeWorld(newWorld);
		}
	    }
	}

	if (org.plt.world.config.onShake) {
	    if (typeof(navigator) != 'undefined' &&
		typeof(navigator.accelerometer) != 'undefined' &&
		typeof(navigator.accelerometer.watchShake) != 'undefined') {
		function success() {
		    if (org.plt.world.config.onShakeEffect) {
			var effect = org.plt.world.config.onShakeEffect([world]);
			org.plt.WorldKernel.applyEffect(effect);
                    }
		    var newWorld = org.plt.world.config.onShake([world]);
		    changeWorld(newWorld);
		}
		function fail() {
		}
		navigator.accelerometer.watchShake(success, fail);
	    }
	}


	addWorldListener(function (w) {
	    if (org.plt.world.config.onRedraw) {
		var context = 
		    canvas.getContext("2d");
		var aScene = 
		    org.plt.world.config.onRedraw([w]);
		aScene.render(context,
			      0,
			      0);
	    }

	});
	addWorldListener(function (w) {
	    if (org.plt.world.config.stopWhen) {
		if (org.plt.world.config.stopWhen([w])) {
		    stopped = true;
		}
	    }
	});

 	changeWorld(aWorld);

 	if(org.plt.world.config.onTick) {
 	    scheduleTimerTick(newWindow);
	}
    };

    // scheduleTimerTick: -> void
    // Repeatedly schedules an evaluation of the onTick until the program has stopped.
    function scheduleTimerTick(window) {
	timerInterval = window.setTimeout(
	    function() {
		if (stopped) {
		    window.clearTimeout(timerInterval);
		    timerInterval = false;
		}
		else {
                    if (org.plt.world.config.onTickEffect) {
			var effect = org.plt.world.config.onTickEffect([world]);
			org.plt.WorldKernel.applyEffect(effect);
                    }

		    changeWorld(
			org.plt.world.config.onTick([world]));
		    scheduleTimerTick(window);
		}
	    },
	    org.plt.world.config.tickDelay);
    }



    org.plt.WorldKernel.isKeyEqual = function(key1, key2) {
	return key1.toString() == key2.toString();
    };


    org.plt.WorldKernel.isImage = function(thing) {
	return 'render' in thing;
    };


    org.plt.WorldKernel.imageWidth = function(thing) {
	return org.plt.types.Rational.makeInstance(thing.getWidth(), 1);
    };


    org.plt.WorldKernel.imageHeight = function(thing) {
	return org.plt.types.Rational.makeInstance(thing.getHeight(), 1);
    };


    // placeImage: image number number scene -> scene
    org.plt.WorldKernel.placeImage = function(picture, x, y, aScene) {
	return aScene.add(picture,
			  org.plt.types.NumberTower.toInteger(x),
			  org.plt.types.NumberTower.toInteger(y));
    };

    
    // emptyScene: number number -> scene
    org.plt.WorldKernel.emptyScene = function(width, height) {
	return new SceneImage(
	    org.plt.types.NumberTower.toInteger(width), 
	    org.plt.types.NumberTower.toInteger(height),
	    []);
    };


    // text: string number color -> TextImage
    org.plt.WorldKernel.text = function(aString, aSize, aColor) {
	return new TextImage
	(aString, 
	 org.plt.types.NumberTower.toInteger(aSize), 
	 aColor);
    };


    // circle: number style color -> TextImage
    org.plt.WorldKernel.circle = function(aRadius, aStyle, aColor) {
	return new CircleImage
	(org.plt.types.NumberTower.toInteger(aRadius), 
	 aStyle,
	 aColor);
    };


    org.plt.WorldKernel._kernelCreateImage = function(path) {
	return FileImage.makeInstance(path.toString());
    };


    org.plt.WorldKernel.nwRectangle = function(w, h, s, c) {
	var aRect = new RectangleImage
	(org.plt.types.NumberTower.toInteger(w),
	 org.plt.types.NumberTower.toInteger(h),
	 s,
	 c);
	return updatePinhole(aRect, 0, 0);
    };

    org.plt.WorldKernel.rectangle = function(w, h, s, c) {
	// Fixme: get the pinholes!
	return new RectangleImage(
	    org.plt.types.NumberTower.toInteger(w),
	    org.plt.types.NumberTower.toInteger(h),
	    s,
	    c);
    };


    // Base class for all images.
    function BaseImage(pinholeX, pinholeY) {
	this.pinholeX = pinholeX;
	this.pinholeY = pinholeY;
    }


    function updatePinhole(anImage, x, y) {
	var aCopy = {};
	for (attr in anImage) {
	    aCopy[attr] = anImage[attr];
	}
	aCopy.pinholeX = x;
	aCopy.pinholeY = y;
	return aCopy;
    }


    
    // SceneImage: primitive-number primitive-number (listof image) -> Scene
    function SceneImage(width, height, children) {
	BaseImage.call(this, 0, 0);
	this.width = width;
	this.height = height;
	this.children = children;
    }
    SceneImage.prototype = heir(BaseImage.prototype);


    // add: image primitive-number primitive-number -> Scene
    SceneImage.prototype.add = function(anImage, x, y) {
	return new SceneImage(this.width, 
			      this.height,
			      this.children.concat([[anImage, x, y]]));
    };

    // render: 2d-context primitive-number primitive-number -> void
    SceneImage.prototype.render = function(ctx, x, y) {
	var i;
	var childImage, childX, childY;
	// Clear the scene.
	ctx.clearRect(x - this.pinholeX, y - this.pinholeY, 
 		      this.width, this.height);
	// Then ask every object to render itself.
	for(i = 0; i < this.children.length; i++) {
	    childImage = this.children[i][0];
	    childX = this.children[i][1];
	    childY = this.children[i][2];
	    ctx.save();
	    childImage.render(ctx,
			      childX + x - childImage.pinholeX,
			      childY + y - childImage.pinholeY);
	    ctx.restore();
	}
    };

    SceneImage.prototype.getWidth = function() {
	return this.width;
    };

    SceneImage.prototype.getHeight = function() {
	return this.height;
    };


    
    function FileImage(path) {
	BaseImage.call(this, 0, 0);
	var self = this;
	this.isLoaded = false;
	// fixme: we may want to do something blocking here for
	// onload, since we don't know at this time what the file size
	// should be, nor will drawImage do the right thing until the
	// file is loaded.
	this.img = new Image();
	this.img.onload = function() {
	    self.isLoaded = true;
	    self.pinholeX = self.img.width / 2;
	    self.pinholeY = self.img.height / 2;
	};
	this.img.src = path;
    }
    FileImage.prototype = heir(BaseImage.prototype);
    
    var imageCache = {};
    FileImage.makeInstance = function(path) {
	if (! (path in imageCache)) {
	    imageCache[path] = new FileImage(path);
	} 
	return imageCache[path];
    };


    FileImage.prototype.render = function(ctx, x, y) {
	ctx.drawImage(this.img, x, y);
    };


    FileImage.prototype.getWidth = function() {
	return this.img.width;
    };


    FileImage.prototype.getHeight = function() {
	return this.img.height;
    };




    function RectangleImage(width, height, style, color) {
	BaseImage.call(this, width/2, height/2);
	this.width = width;
	this.height = height;
	this.style = style;
	this.color = color;
    }
    RectangleImage.prototype = heir(BaseImage.prototype);


    RectangleImage.prototype.render = function(ctx, x, y) {
	ctx.fillStyle = this.color;
	if (this.style.toLowerCase() == "outline") {
	    ctx.strokeRect(x, y, this.width, this.height);
	} else {
	    ctx.fillRect(x, y, this.width, this.height);
	}
    };

    RectangleImage.prototype.getWidth = function() {
	return this.width;
    };


    RectangleImage.prototype.getHeight = function() {
	return this.height;
    };



    
    function TextImage(msg, size, color) {
	BaseImage.call(this, 0, 0);
	this.msg = msg;
	this.size = size;
	this.color = color;
	this.font = "Verdana";
    }
    TextImage.prototype = heir(BaseImage.prototype);

    TextImage.prototype.render = function(ctx, x, y) {
	// Fixme: not quite right yet.
	if ('mozDrawText' in ctx) {
	    ctx.mozTextStyle=this.size+"pt "+this.font;
	    // Fix me: I don't quite know how to get the
	    // baseline right.
	    ctx.translate(x, y + this.size);
	    ctx.fillStyle = this.color;
	    ctx.mozDrawText(this.msg);
	} else {
	    ctx.font.color = this.color;
	    ctx.font.size = this.size + "px";
	    ctx.fillText(this.msg, x, y);
	}
    };
    
    TextImage.prototype.getWidth = function() {
	// Fixme: we need the font metrics to do this right...
	return this.size * this.msg.length;
    };

    TextImage.prototype.getHeight = function() {
	return 10;
	// Fixme: we need the font metrics to do this right...
    };


    function CircleImage(radius, style, color) {
	BaseImage.call(this, radius, radius);
	this.radius = radius;
	this.style = style;
	this.color = color;
    }
    CircleImage.prototype = heir(BaseImage.prototype);

    CircleImage.prototype.render = function(ctx, x, y) {
	ctx.translate(0, 0);
	ctx.beginPath();
	ctx.fillStyle = this.color;
	ctx.arc(x + this.radius,
		y + this.radius, 
		this.radius, 0, 2*Math.PI, false);
	if (this.style.toLowerCase() == "outline")
	    ctx.stroke();
	else
	    ctx.fill();
	ctx.closePath();
    };
    
    CircleImage.prototype.getWidth = function() {
	return this.radius * 2;
    };

    CircleImage.prototype.getHeight = function() {
	return this.radius * 2;
    };






    //////////////////////////////////////////////////////////////////////
    // Effects
    org.plt.WorldKernel.applyEffect = function(anEffect) {
	anEffect.run();
    }



    function effect_question_(thing) { 
	return ((effect_colon_none_question_(thing))||
		(effect_colon_beep_question_(thing))||
		(effect_colon_play_dash_dtmf_dash_tone_question_(thing))||
		(effect_colon_send_dash_sms_question_(thing))||
		(effect_colon_play_dash_sound_dash_url_question_(thing))); 
    }
    org.plt.WorldKernel.effect_question_ = effect_question_;
    
    function effect_colon_none() {  }
    effect_colon_none.prototype = new org.plt.Kernel.Struct();
    effect_colon_none.prototype.isEqual = function(other) {
	if (other instanceof effect_colon_none) {
	    return org.plt.types.Logic.TRUE;
	} else {
	    return false;
	}
    };

    effect_colon_none.prototype.run = function() {
	// Do nothing.
    };

    function make_dash_effect_colon_none() { return new effect_colon_none(); }
    org.plt.WorldKernel.make_dash_effect_colon_none = make_dash_effect_colon_none;



    function effect_colon_none_question_(obj) { 
	return obj instanceof effect_colon_none; }
    org.plt.WorldKernel.effect_colon_none_question_ = effect_colon_none_question_;
   
    function effect_colon_beep() {  }

    effect_colon_beep.prototype = new org.plt.Kernel.Struct();

    effect_colon_beep.prototype.isEqual = function(other) {
	if (other instanceof effect_colon_beep) {
	    return org.plt.types.Logic.TRUE;
	} else {
	    return false;
	}
    } 

    effect_colon_beep.prototype.run = function() {
	if (typeof navigator != "undefined" &&
	    typeof navigator.notification != "undefined") {
	    navigator.notification.beep(1);
        } else {
	    alert("Beep");  // FIXME: fill me in.
        }
    };

    function make_dash_effect_colon_beep() { return new effect_colon_beep(); }
    org.plt.WorldKernel.make_dash_effect_colon_beep = make_dash_effect_colon_beep;




    function effect_colon_beep_question_(obj) { 
	return obj instanceof effect_colon_beep; }
    org.plt.WorldKernel.effect_colon_beep_question_ = effect_colon_beep_question_;
    function effect_colon_send_dash_sms(address,msg) { 
	this.address = address;
	this.msg = msg; }
    
    effect_colon_send_dash_sms.prototype = new org.plt.Kernel.Struct();
    effect_colon_send_dash_sms.prototype.isEqual = function(other) {
	if (other instanceof effect_colon_send_dash_sms) {
	    return ((org.plt.Kernel.equal_question_((effect_colon_send_dash_sms_dash_msg(this)),
						    (effect_colon_send_dash_sms_dash_msg(other))))&&
		    ((org.plt.Kernel.equal_question_((effect_colon_send_dash_sms_dash_address(this)),
						     (effect_colon_send_dash_sms_dash_address(other))))&&
		     org.plt.types.Logic.TRUE));
	} else {
	    return false;
	}
    } 
    effect_colon_send_dash_sms.prototype.run = function() {
	// FIXME: fill me in
    };


    //////////////////////////////////////////////////////////////////////
    // dtmf tones
    
    function effect_colon_play_dash_dtmf_dash_tone(tone,duration) { this.tone = tone;
	this.duration = duration; }
    effect_colon_play_dash_dtmf_dash_tone.prototype = new org.plt.Kernel.Struct();
    effect_colon_play_dash_dtmf_dash_tone.prototype.isEqual = function(other) {
	if (other instanceof effect_colon_play_dash_dtmf_dash_tone) {
	    return ((org.plt.Kernel.equal_question_((effect_colon_play_dash_dtmf_dash_tone_dash_duration(this)),(effect_colon_play_dash_dtmf_dash_tone_dash_duration(other))))&&((org.plt.Kernel.equal_question_((effect_colon_play_dash_dtmf_dash_tone_dash_tone(this)),(effect_colon_play_dash_dtmf_dash_tone_dash_tone(other))))&&org.plt.types.Logic.TRUE));
	} else {
	    return false;
	}
    } 

    effect_colon_play_dash_dtmf_dash_tone.prototype.run = function() {
	if (typeof navigator != "undefined" &&
	    typeof navigator.media != "undefined") {
	    var tone = this.tone.toInteger();
            var duration = this.duration.toInteger();
	    navigator.media.playDTMF(tone);
            setTimeout(function() { navigator.media.stopDTMF() },
                       duration);
        } else {
	    alert("dtmf tone");
        }
    };


    function make_dash_effect_colon_play_dash_dtmf_dash_tone(id0,id1) { return new effect_colon_play_dash_dtmf_dash_tone(id0,id1); }
    function effect_colon_play_dash_dtmf_dash_tone_dash_tone(obj) { return obj.tone; }
    function effect_colon_play_dash_dtmf_dash_tone_dash_duration(obj) { return obj.duration; }
    function effect_colon_play_dash_dtmf_dash_tone_question_(obj) { 
	return obj instanceof effect_colon_play_dash_dtmf_dash_tone; }

    //////////////////////////////////////////////////////////////////////





    function make_dash_effect_colon_send_dash_sms(id0,id1) { return new effect_colon_send_dash_sms(id0,id1); }
    org.plt.WorldKernel.make_dash_effect_colon_send_dash_sms = make_dash_effect_colon_send_dash_sms;
    function effect_colon_send_dash_sms_dash_address(obj) { return obj.address; }
    
    function effect_colon_send_dash_sms_dash_msg(obj) { return obj.msg; }
    org.plt.WorldKernel.effect_colon_send_dash_sms_dash_msg = effect_colon_send_dash_sms_dash_msg;

    function effect_colon_send_dash_sms_question_(obj) { 
	return obj instanceof effect_colon_send_dash_sms; }
    org.plt.WorldKernel.effect_colon_send_dash_sms_question_ = effect_colon_send_dash_sms_question_;




    function effect_colon_play_dash_sound_dash_url(url,string) { this.url = url;
	this.string = string; }
    effect_colon_play_dash_sound_dash_url.prototype = new org.plt.Kernel.Struct();

    effect_colon_play_dash_sound_dash_url.prototype.isEqual = function(other) {
	if (other instanceof effect_colon_play_dash_sound_dash_url) {
	    return ((org.plt.Kernel.equal_question_((effect_colon_play_dash_sound_dash_url_dash_string(this)),
						    (effect_colon_play_dash_sound_dash_url_dash_string(other))))&&
		    ((org.plt.Kernel.equal_question_((effect_colon_play_dash_sound_dash_url_dash_url(this)),
						     (effect_colon_play_dash_sound_dash_url_dash_url(other))))&&
		     org.plt.types.Logic.TRUE));
	} else {
	    return false;
	}
    } 

    effect_colon_play_dash_sound_dash_url.prototype.run = function() {
	// FIXME: fill me in
	//window.document.getElementById("dummy").innerHTML=("<embed src='"+soundfile+"' hidden='true' autostart='true' loop='false' />");
    };
    

    function make_dash_effect_colon_play_dash_sound_dash_url(id0,id1) { return new effect_colon_play_dash_sound_dash_url(id0,id1); }
    org.plt.WorldKernel.make_dash_effect_colon_play_dash_sound_dash_url = make_dash_effect_colon_play_dash_sound_dash_url;function effect_colon_play_dash_sound_dash_url_dash_url(obj) { return obj.url; }

    function effect_colon_play_dash_sound_dash_url_dash_string(obj) { return obj.string; }
    org.plt.WorldKernel.effect_colon_play_dash_sound_dash_url_dash_string = effect_colon_play_dash_sound_dash_url_dash_string;

    function effect_colon_play_dash_sound_dash_url_question_(obj) { 
	return obj instanceof effect_colon_play_dash_sound_dash_url; }
    org.plt.WorldKernel.effect_colon_play_dash_sound_dash_url_question_ = effect_colon_play_dash_sound_dash_url_question_;
//////////////////////////////////////////////////////////////////////



 



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



})();
