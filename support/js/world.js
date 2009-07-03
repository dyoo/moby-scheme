
// Depends on kernel.js, world-config.js
var plt = plt || {};
plt.world = plt.world || {};
plt.world.Kernel = plt.world.Kernel || {};
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



    var announceListeners = [];
    plt.world.Kernel.addAnnounceListener = function(listener) {
	announceListeners.push(listener);
    };
    plt.world.Kernel.removeAnnounceListener = function(listener) {
	var idx = announceListeners.indexOf(listener);
	if (idx != -1) {
	    announceListeners.splice(idx, 1);
	}
    };
    plt.world.Kernel.announce = function(eventName, vals) {
	for (var i = 0; i < announceListeners.length; i++) {
	    try {
		announceListeners[i](eventName, vals);
	    } catch (e) {}
	}
    };





    // changeWorld: world -> void
    // Changes the current world to newWorld.
    function changeWorld(newWorld) {
	world = newWorld;
	notifyWorldListeners();
    }


    // updateWorld: (world -> world) -> void
    // Public function: update the world, given the old state of the
    // world.
    plt.world.Kernel.updateWorld = function(updater) {
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
    plt.world.Kernel.bigBang = function(width, height, aWorld, handlers) {
	var i;
	var newWindow = getBigBangWindow(width, height);
	var canvas = 
	    newWindow.document.getElementById("canvas");
	canvas.width = width;
	canvas.height = height;

	resetWorld();

	var config = new plt.world.config.WorldConfig();
	for (i = 0; i < handlers.length; i++) {
	    config = handlers[i](config);
	}
	config = config.updateAll({'changeWorld': plt.world.Kernel.updateWorld});
	plt.world.config.CONFIG = config;

	if (config.lookup('initialEffect')) {
	    plt.world.Kernel.applyEffect(config.lookup('initialEffect'));
	}

	if (config.lookup('onKey')) {
	    newWindow.onkeypress = function(e) {
		if (! stopped) {
		    var keyname = getKeyCodeName(e);
		    if (config.lookup('onKeyEffect')) {
		      var effect = config.lookup('onKeyEffect')([world, keyname]);
			plt.world.Kernel.applyEffect(effect);
                    }

		    var newWorld = config.lookup('onKey')([world, keyname]);
		    changeWorld(newWorld);
		}
	    }
	}

	if (config.lookup('onShake')) {
	    if (typeof(navigator) != 'undefined' &&
		typeof(navigator.accelerometer) != 'undefined' &&
		typeof(navigator.accelerometer.watchShake) != 'undefined') {
		function success() {
		  if (config.lookup('onShakeEffect')) {
		        var effect = config.lookup('onShakeEffect')([world]);
			plt.world.Kernel.applyEffect(effect);
                    }
		  var newWorld = config.lookup('onShake')([world]);
		    changeWorld(newWorld);
		}
		function fail() {
		}
		navigator.accelerometer.watchShake(success, fail);
	    }
	}


       if (config.lookup('onRedraw')) {
	   addWorldListener(function (w) {
		var context = 
		    canvas.getContext("2d");
		var aScene = 
		  config.lookup('onRedraw')([w]);
		aScene.render(context,
			      0,
			      0);
	   });
       }


	addWorldListener(function (w) {
	  if (config.lookup('stopWhen')) {
	    if (config.lookup('stopWhen')([w])) {
		    stopped = true;
	    }
	  }
	});

 	changeWorld(aWorld);

 	if(config.lookup('onTick')) {
	  scheduleTimerTick(newWindow, config);
	}
    };

    // scheduleTimerTick: -> void
    // Repeatedly schedules an evaluation of the onTick until the program has stopped.
    function scheduleTimerTick(window, config) {
	timerInterval = window.setInterval(
	    function() {
		if (stopped) {
		    window.clearTimeout(timerInterval);
		    timerInterval = false;
		}
		else {
		  if (config.lookup('onTickEffect')) {
		    var effect = config.lookup('onTickEffect')([world]);
			plt.world.Kernel.applyEffect(effect);
                    }
		    changeWorld(
		      config.lookup('onTick')([world]));
		}
	    },
	    config.lookup('tickDelay'));
    }



    plt.world.Kernel.isKeyEqual = function(key1, key2) {
	return key1.toString() == key2.toString();
    };


    plt.world.Kernel.isImage = function(thing) {
	return 'render' in thing;
    };


    plt.world.Kernel.imageWidth = function(thing) {
	return plt.types.Rational.makeInstance(thing.getWidth(), 1);
    };


    plt.world.Kernel.imageHeight = function(thing) {
	return plt.types.Rational.makeInstance(thing.getHeight(), 1);
    };


    // placeImage: image number number scene -> scene
    plt.world.Kernel.placeImage = function(picture, x, y, aScene) {
	return aScene.add(picture,
			  plt.types.NumberTower.toInteger(x),
			  plt.types.NumberTower.toInteger(y));
    };

    
    // emptyScene: number number -> scene
    plt.world.Kernel.emptyScene = function(width, height) {
	return new SceneImage(
	    plt.types.NumberTower.toInteger(width), 
	    plt.types.NumberTower.toInteger(height),
	    []);
    };


    // text: string number color -> TextImage
    plt.world.Kernel.text = function(aString, aSize, aColor) {
	return new TextImage
	(aString, 
	 plt.types.NumberTower.toInteger(aSize), 
	 aColor);
    };


    // circle: number style color -> TextImage
    plt.world.Kernel.circle = function(aRadius, aStyle, aColor) {
	return new CircleImage
	(plt.types.NumberTower.toInteger(aRadius), 
	 aStyle,
	 aColor);
    };


    plt.world.Kernel._kernelCreateImage = function(path) {
	return FileImage.makeInstance(path.toString());
    };


    plt.world.Kernel.nwRectangle = function(w, h, s, c) {
	var aRect = new RectangleImage
	(plt.types.NumberTower.toInteger(w),
	 plt.types.NumberTower.toInteger(h),
	 s,
	 c);
	return updatePinhole(aRect, 0, 0);
    };

    plt.world.Kernel.rectangle = function(w, h, s, c) {
	// Fixme: get the pinholes!
	return new RectangleImage(
	    plt.types.NumberTower.toInteger(w),
	    plt.types.NumberTower.toInteger(h),
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
	    //ctx.font.color = this.color;
	    //ctx.font.size = this.size + "px";
	    ctx.fillText(this.msg, x, y);
	    // FIXME.
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

    /**
     * applyEffect applies all of the effects
     * @param aCompEffect a compound effect is either a scheme list of compound effects or a single primitive effect
     */
    plt.world.Kernel.applyEffect = function(aCompEffect) {
    	if ( plt.Kernel.pair_question_(aCompEffect) ) {
    	    plt.world.Kernel.applyEffect(aCompEffect.first());
    	    plt.world.Kernel.applyEffect(aCompEffect.rest());
    	}
    	else if ( plt.Kernel.empty_question_(aCompEffect) ) {
    	    // Do Nothing
    	}
    	else {
    	    aCompEffect.run();
    	}
    }



    function effect_question_(thing) { 
	return ((effect_colon_none_question_(thing))||
		(effect_colon_beep_question_(thing))||
		(effect_colon_play_dash_dtmf_dash_tone_question_(thing))||
		(effect_colon_send_dash_sms_question_(thing))||
		(effect_colon_play_dash_sound_dash_url_question_(thing))); 
    }
    plt.world.Kernel.effect_question_ = effect_question_;
    
    function effect_colon_none() {  }
    effect_colon_none.prototype = new plt.Kernel.Struct();
    effect_colon_none.prototype.isEqual = function(other) {
	if (other instanceof effect_colon_none) {
	    return plt.types.Logic.TRUE;
	} else {
	    return false;
	}
    };

    effect_colon_none.prototype.run = function() {
	// Do nothing.
    };

    function make_dash_effect_colon_none() { return new effect_colon_none(); }
    plt.world.Kernel.make_dash_effect_colon_none = make_dash_effect_colon_none;



    function effect_colon_none_question_(obj) { 
	return obj instanceof effect_colon_none; }
    plt.world.Kernel.effect_colon_none_question_ = effect_colon_none_question_;
   
    function effect_colon_beep() {  }

    effect_colon_beep.prototype = new plt.Kernel.Struct();

    effect_colon_beep.prototype.isEqual = function(other) {
	if (other instanceof effect_colon_beep) {
	    return plt.types.Logic.TRUE;
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
    plt.world.Kernel.make_dash_effect_colon_beep = make_dash_effect_colon_beep;




    function effect_colon_beep_question_(obj) { 
	return obj instanceof effect_colon_beep; }
    plt.world.Kernel.effect_colon_beep_question_ = effect_colon_beep_question_;
    function effect_colon_send_dash_sms(address,msg) { 
	this.address = address;
	this.msg = msg; }
    
    effect_colon_send_dash_sms.prototype = new plt.Kernel.Struct();
    effect_colon_send_dash_sms.prototype.isEqual = function(other) {
	if (other instanceof effect_colon_send_dash_sms) {
	    return ((plt.Kernel.equal_question_((effect_colon_send_dash_sms_dash_msg(this)),
						    (effect_colon_send_dash_sms_dash_msg(other))))&&
		    ((plt.Kernel.equal_question_((effect_colon_send_dash_sms_dash_address(this)),
						     (effect_colon_send_dash_sms_dash_address(other))))&&
		     plt.types.Logic.TRUE));
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
    effect_colon_play_dash_dtmf_dash_tone.prototype = new plt.Kernel.Struct();
    effect_colon_play_dash_dtmf_dash_tone.prototype.isEqual = function(other) {
	if (other instanceof effect_colon_play_dash_dtmf_dash_tone) {
	    return ((plt.Kernel.equal_question_((effect_colon_play_dash_dtmf_dash_tone_dash_duration(this)),(effect_colon_play_dash_dtmf_dash_tone_dash_duration(other))))&&((plt.Kernel.equal_question_((effect_colon_play_dash_dtmf_dash_tone_dash_tone(this)),(effect_colon_play_dash_dtmf_dash_tone_dash_tone(other))))&&plt.types.Logic.TRUE));
	} else {
	    return false;
	}
    } 

    effect_colon_play_dash_dtmf_dash_tone.prototype.run = function() {
	if (typeof (navigator) != "undefined" &&
	    typeof (navigator.audio) != "undefined") {
	    var tone = this.tone.toInteger();
            var duration = this.duration.toInteger();
	    navigator.audio.playDTMF(tone);
            setTimeout(function() { navigator.audio.stopDTMF() },
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


    plt.world.Kernel.make_dash_effect_colon_play_dash_dtmf_dash_tone = make_dash_effect_colon_play_dash_dtmf_dash_tone;
    //////////////////////////////////////////////////////////////////////





    function make_dash_effect_colon_send_dash_sms(id0,id1) {
    	return new effect_colon_send_dash_sms(id0,id1);
    }

    plt.world.Kernel.make_dash_effect_colon_send_dash_sms = make_dash_effect_colon_send_dash_sms;

    function effect_colon_send_dash_sms_dash_address(obj) {
    	return obj.address;
    }
    
    function effect_colon_send_dash_sms_dash_msg(obj) {
    	return obj.msg;
    }

    plt.world.Kernel.effect_colon_send_dash_sms_dash_msg = effect_colon_send_dash_sms_dash_msg;

    function effect_colon_send_dash_sms_question_(obj) { 
	return obj instanceof effect_colon_send_dash_sms;
    }

    plt.world.Kernel.effect_colon_send_dash_sms_question_ = effect_colon_send_dash_sms_question_;




    function effect_colon_play_dash_sound_dash_url(url, string) {
    	this.url = url;
	this.string = string;
    }

    effect_colon_play_dash_sound_dash_url.prototype = new plt.Kernel.Struct();

    effect_colon_play_dash_sound_dash_url.prototype.isEqual = function(other) {
	if (other instanceof effect_colon_play_dash_sound_dash_url) {
	    return ((plt.Kernel.equal_question_((effect_colon_play_dash_sound_dash_url_dash_string(this)),
						    (effect_colon_play_dash_sound_dash_url_dash_string(other))))&&
		    ((plt.Kernel.equal_question_((effect_colon_play_dash_sound_dash_url_dash_url(this)),
						     (effect_colon_play_dash_sound_dash_url_dash_url(other))))&&
		     plt.types.Logic.TRUE));
	} else {
	    return false;
	}
    } 

    effect_colon_play_dash_sound_dash_url.prototype.run = function() {
    	navigator.audio.playMusic(this.url);
    };

    function make_dash_effect_colon_play_dash_sound_dash_url(id0, id1) {
    	return new effect_colon_play_dash_sound_dash_url(id0, id1);
    }

    plt.world.Kernel.make_dash_effect_colon_play_dash_sound_dash_url = make_dash_effect_colon_play_dash_sound_dash_url;

    function effect_colon_play_dash_sound_dash_url_dash_url(obj) {
    	return obj.url;
    }

    function effect_colon_play_dash_sound_dash_url_dash_string(obj) {
    	return obj.string;
    }

    plt.world.Kernel.effect_colon_play_dash_sound_dash_url_dash_string = effect_colon_play_dash_sound_dash_url_dash_string;

    function effect_colon_play_dash_sound_dash_url_question_(obj) { 
	return obj instanceof effect_colon_play_dash_sound_dash_url;
    }

    plt.world.Kernel.effect_colon_play_dash_sound_dash_url_question_ = effect_colon_play_dash_sound_dash_url_question_;
//////////////////////////////////////////////////////////////////////////

    function effect_colon_stop_dash_sound_dash_url(url, string) {
    	this.url = url;
	this.string = string;
    }

    effect_colon_stop_dash_sound_dash_url.prototype = new plt.Kernel.Struct();

    effect_colon_stop_dash_sound_dash_url.prototype.isEqual = function(other) {
	if (other instanceof effect_colon_stop_dash_sound_dash_url) {
	    return ((plt.Kernel.equal_question_((effect_colon_stop_dash_sound_dash_url_dash_string(this)),
						    (effect_colon_stop_dash_sound_dash_url_dash_string(other))))&&
		    ((plt.Kernel.equal_question_((effect_colon_stop_dash_sound_dash_url_dash_url(this)),
						     (effect_colon_stop_dash_sound_dash_url_dash_url(other))))&&
		     plt.types.Logic.TRUE));
	} else {
	    return false;
	}
    } 

    effect_colon_stop_dash_sound_dash_url.prototype.run = function() {
    	navigator.audio.stopMusic(this.url);
    };

    function make_dash_effect_colon_stop_dash_sound_dash_url(id0, id1) {
    	return new effect_colon_stop_dash_sound_dash_url(id0, id1);
    }

    plt.world.Kernel.make_dash_effect_colon_stop_dash_sound_dash_url = make_dash_effect_colon_stop_dash_sound_dash_url;

    function effect_colon_stop_dash_sound_dash_url_dash_url(obj) {
    	return obj.url;
    }

    function effect_colon_stop_dash_sound_dash_url_dash_string(obj) {
    	return obj.string;
    }

    plt.world.Kernel.effect_colon_stop_dash_sound_dash_url_dash_string = effect_colon_stop_dash_sound_dash_url_dash_string;

    function effect_colon_stop_dash_sound_dash_url_question_(obj) { 
	return obj instanceof effect_colon_stop_dash_sound_dash_url;
    }

    plt.world.Kernel.effect_colon_stop_dash_sound_dash_url_question_ = effect_colon_stop_dash_sound_dash_url_question_;
//////////////////////////////////////////////////////////////////////

    function effect_colon_pause_dash_sound_dash_url(url, string) {
    	this.url = url;
	this.string = string;
    }

    effect_colon_pause_dash_sound_dash_url.prototype = new plt.Kernel.Struct();

    effect_colon_pause_dash_sound_dash_url.prototype.isEqual = function(other) {
	if (other instanceof effect_colon_pause_dash_sound_dash_url) {
	    return ((plt.Kernel.equal_question_((effect_colon_pause_dash_sound_dash_url_dash_string(this)),
						    (effect_colon_pause_dash_sound_dash_url_dash_string(other))))&&
		    ((plt.Kernel.equal_question_((effect_colon_pause_dash_sound_dash_url_dash_url(this)),
						     (effect_colon_pause_dash_sound_dash_url_dash_url(other))))&&
		     plt.types.Logic.TRUE));
	} else {
	    return false;
	}
    } 

    effect_colon_pause_dash_sound_dash_url.prototype.run = function() {
    	navigator.audio.pauseMusic(this.url);
    };

    function make_dash_effect_colon_pause_dash_sound_dash_url(id0, id1) {
    	return new effect_colon_pause_dash_sound_dash_url(id0, id1);
    }

    plt.world.Kernel.make_dash_effect_colon_pause_dash_sound_dash_url = make_dash_effect_colon_pause_dash_sound_dash_url;

    function effect_colon_pause_dash_sound_dash_url_dash_url(obj) {
    	return obj.url;
    }

    function effect_colon_pause_dash_sound_dash_url_dash_string(obj) {
    	return obj.string;
    }

    plt.world.Kernel.effect_colon_pause_dash_sound_dash_url_dash_string = effect_colon_pause_dash_sound_dash_url_dash_string;

    function effect_colon_pause_dash_sound_dash_url_question_(obj) { 
	return obj instanceof effect_colon_pause_dash_sound_dash_url;
    }

    plt.world.Kernel.effect_colon_pause_dash_sound_dash_url_question_ = effect_colon_pause_dash_sound_dash_url_question_;
//////////////////////////////////////////////////////////////////////

    function effect_colon_set_dash_sound_dash_volume(volume) {
    	this.volume = volume;
    }

    effect_colon_set_dash_sound_dash_volume.prototype = new plt.Kernel.Struct();

    effect_colon_set_dash_sound_dash_volume.prototype.isEqual = function(other) {
    	if (other instanceof effect_colon_set_dash_sound_dash_volume) {
    	    return ((plt.Kernel.equal_question_((effect_colon_set_dash_sound_dash_volume_dash_volume(this)),
     	                                            (effect_colon_set_dash_sound_dash_volume_dash_volume(other))))&&
    	            plt.types.Logic.TRUE);
    	} else {
    	     return false;
    	}
    }

    effect_colon_set_dash_sound_dash_volume.prototype.run = function() {
    	navigator.audio.setMusicVolume(this.volume);
    }

    function make_dash_effect_colon_set_dash_sound_dash_volume(id0) {
    	return new effect_colon_set_dash_sound_dash_volume(id0);
    }

    plt.world.Kernel.make_dash_effect_colon_set_dash_sound_dash_volume = make_dash_effect_colon_set_dash_sound_dash_volume;

    function effect_colon_set_dash_sound_dash_volume_dash_volume(obj) {
    	return obj.volume;
    }

    function effect_colon_set_dash_sound_dash_volume_question_(obj) {
    	return obj instanceof effect_colon_set_dash_sound_dash_volume;
    }
/////////////////////////////////////////////////////////////////////////

    function effect_colon_raise_dash_sound_dash_volume() {  }

    effect_colon_raise_dash_sound_dash_volume.prototype = new plt.Kernel.Struct();

    effect_colon_raise_dash_sound_dash_volume.prototype.run = function() {
    	navigator.audio.increaseMusicVolume();
    }


    effect_colon_raise_dash_sound_dash_volume.prototype.isEqual = function(other) {
    	if (other instanceof effect_colon_raise_dash_sound_dash_volume) {
    	    return plt.types.Logic.TRUE;
    	} else {
    	    return false;
    	}
    }

    function make_dash_effect_colon_raise_dash_sound_dash_volume() {
    	return new effect_colon_raise_dash_sound_dash_volume();
    }

    plt.world.Kernel.make_dash_effect_colon_raise_dash_sound_dash_volume = make_dash_effect_colon_raise_dash_sound_dash_volume;

    function effect_colon_raise_dash_sound_dash_volume_question_(obj) {
    	return obj instanceof effect_colon_raise_dash_sound_dash_volume;
    }
////////////////////////////////////////////////////////////////////////



    function effect_colon_lower_dash_sound_dash_volume() {  }

    effect_colon_lower_dash_sound_dash_volume.prototype = new plt.Kernel.Struct();

    effect_colon_lower_dash_sound_dash_volume.prototype.run = function() {
    	navigator.audio.decreaseMusicVolume();
    }



    effect_colon_lower_dash_sound_dash_volume.prototype.isEqual = function(other) {
    	if (other instanceof effect_colon_lower_dash_sound_dash_volume) {
    	    return plt.types.Logic.TRUE;
    	} else {
    	    return false;
    	}
    }

    function make_dash_effect_colon_lower_dash_sound_dash_volume() {
    	return new effect_colon_lower_dash_sound_dash_volume();
    }

    plt.world.Kernel.make_dash_effect_colon_lower_dash_sound_dash_volume = make_dash_effect_colon_lower_dash_sound_dash_volume;

    function effect_colon_lower_dash_sound_dash_volume_question_(obj) {
    	return obj instanceof effect_colon_lower_dash_sound_dash_volume;
    }



//////////////////////////////////////////////////////////////////////

    // Loading DHTML files dynamically

    function effect_colon_js_dash_load_dash_script(url) { this.url = url; }
    effect_colon_js_dash_load_dash_script.prototype = new plt.Kernel.Struct();

    effect_colon_js_dash_load_dash_script.prototype.run = function() {
	var e = document.createElement("script");
	e.src = this.url;
	e.type = "text/javascript";
	window.document.getElementsByTagName("head")[0].appendChild(e);
    }

    effect_colon_js_dash_load_dash_script.prototype.isEqual = function(other) {
        if (other instanceof effect_colon_js_dash_load_dash_script) {
            return ((plt.Kernel.equal_question_((effect_colon_js_dash_load_dash_script_dash_url(this)),(effect_colon_js_dash_load_dash_script_dash_url(other))))&&plt.types.Logic.TRUE);
        } else {
            return false;
        }
    } 
    function make_dash_effect_colon_js_dash_load_dash_script(id0) { return new effect_colon_js_dash_load_dash_script(id0); }

    plt.world.Kernel.make_dash_effect_colon_js_dash_load_dash_script = make_dash_effect_colon_js_dash_load_dash_script;


    function effect_colon_js_dash_load_dash_script_dash_url(obj) { return obj.url; }
    function effect_colon_js_dash_load_dash_script_question_(obj) { 
        return obj instanceof effect_colon_js_dash_load_dash_script; }



//////////////////////////////////////////////////////////////////////

    // Dynamic javascript string evaluation
    
    function effect_colon_js_dash_exec_dash_string(cmd) { this.cmd = cmd; }
    effect_colon_js_dash_exec_dash_string.prototype = new plt.Kernel.Struct();

    effect_colon_js_dash_exec_dash_string.prototype.run = function() {
	eval(this.cmd);
    }

    effect_colon_js_dash_exec_dash_string.prototype.isEqual = function(other) {
        if (other instanceof effect_colon_js_dash_exec_dash_string) {
            return ((plt.Kernel.equal_question_((effect_colon_js_dash_exec_dash_string_dash_cmd(this)),(effect_colon_js_dash_exec_dash_string_dash_cmd(other))))&&plt.types.Logic.TRUE);
        } else {
            return false;
        }
    } 
    function make_dash_effect_colon_js_dash_exec_dash_string(id0) { return new effect_colon_js_dash_exec_dash_string(id0); }

    plt.world.Kernel.make_dash_effect_colon_js_dash_exec_dash_string = make_dash_effect_colon_js_dash_exec_dash_string;


    function effect_colon_js_dash_exec_dash_string_dash_cmd(obj) { return obj.cmd; }
    function effect_colon_js_dash_exec_dash_string_question_(obj) { 
        return obj instanceof effect_colon_js_dash_exec_dash_string; }




///////////////////////////////////////////////////////////////////////////

    var currentLockFlags = -1;

    function effect_colon_set_dash_wake_dash_lock(flags) {
    	this.flags = flags;
    }

    effect_colon_set_dash_wake_dash_lock.prototype = new plt.Kernel.Struct();

    effect_colon_set_dash_wake_dash_lock.prototype.run = function() {
    	if (this.flags != currentLockFlags) {
    	    navigator.power.setWakeLock(this.flags);
    	    currentLockFlags = this.flags;
    	}
    }

    effect_colon_set_dash_wake_dash_lock.prototype.isEqual = function(other) {
    	if (other instanceof effect_colon_set_dash_wake_dash_lock) {
    	    return ((plt.Kernel.equal_question_((effect_colon_set_dash_wake_dash_lock_dash_flags(this)),
    	                                            (effect_colon_set_dash_wake_dash_lock_dash_flags(other))))&&
    	            plt.types.Logic.TRUE);
    	} else {
    	    return false;
    	}
    }
 
    function make_dash_effect_colon_set_dash_wake_dash_lock(id0) {
    	return new effect_colon_set_dash_wake_dash_lock(id0);
    }

    plt.world.Kernel.make_dash_effect_colon_set_dash_wake_dash_lock = make_dash_effect_colon_set_dash_wake_dash_lock;

    function effect_colon_set_dash_wake_dash_lock_dash_flags(obj) {
    	return obj.flags;
    }

    function effect_colon_set_dash_wake_dash_lock_question_(obj) { 
    	return obj instanceof effect_colon_set_dash_wake_dash_lock;
    }
//////////////////////////////////////////////////////////////////////////

    function effect_colon_release_dash_wake_dash_lock() {  }

    effect_colon_release_dash_wake_dash_lock.prototype = new plt.Kernel.Struct();

    effect_colon_release_dash_wake_dash_lock.prototype.isEqual = function(other) {
    	if (other instanceof effect_colon_release_dash_wake_dash_lock) {
    	    return plt.types.Logic.TRUE;
    	} else {
    	    return false;
    	}
    }

    effect_colon_release_dash_wake_dash_lock.prototype.run = function() {
    	if (currentLockFlags != -1) {
    	    navigator.power.releaseWakeLock();
    	    currentLockFlags = -1;
    	}
    }

    function make_dash_effect_colon_release_dash_wake_dash_lock() {
    	return new effect_colon_release_dash_wake_dash_lock();
    }

    plt.world.Kernel.make_dash_effect_colon_release_dash_wake_dash_lock = make_dash_effect_colon_release_dash_wake_dash_lock;

    function effect_colon_release_dash_wake_dash_lock_question_(obj) { 
    	return obj instanceof effect_colon_release_dash_wake_dash_lock;
    }
//////////////////////////////////////////////////////////////////////////
 


})();
