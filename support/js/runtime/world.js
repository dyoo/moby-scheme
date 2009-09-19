
// Depends on kernel.js, world-config.js, effect-struct.js
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


    plt.world.Kernel.shutdownWorld = function() {
	stopped = true;
    };


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
	canvas.width = width.toInteger();
	canvas.height = height.toInteger();

	resetWorld();

	var config = new plt.world.config.WorldConfig();
	for (i = 0; i < handlers.length; i++) {
	    config = handlers[i](config);
	}
	config = config.updateAll({'changeWorld': plt.world.Kernel.updateWorld,
				   'shutdownWorld': plt.world.Kernel.shutdownWorld});
	plt.world.config.CONFIG = config;


	if (config.lookup('onKey')) {
	    newWindow.onkeypress = function(e) {
		plt.world.stimuli.onKey(e);
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


 	if(config.lookup('onTick')) {
	  scheduleTimerTick(newWindow, config);
	}


 	changeWorld(aWorld);

	if (config.lookup('initialEffect')) {
	    var updaters = plt.world.Kernel.applyEffect(
		config.lookup('initialEffect'));
	    for (var i = 0; i < updaters.length; i++) {
		if (! stopped) {
		    updateWorld(updaters);
		}
	    }
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
		    plt.world.stimuli.onTick();
		}
	    },
	    config.lookup('tickDelay'));
    }



    plt.world.Kernel.isKeyEqual = function(key1, key2) {
	var result = (key1.toString().toLowerCase() == key2.toString().toLowerCase());
	return result;
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


    plt.world.Kernel.openImageUrl = function(path) {
	return FileImage.makeInstance(path.toString());
    };


    plt.world.Kernel.nwRectangle = function(w, h, s, c) {
	var aRect = new RectangleImage
	(plt.types.NumberTower.toInteger(w),
	 plt.types.NumberTower.toInteger(h),
	 s,
	 c);
	return aRect.updatePinhole(0, 0);
    };

    plt.world.Kernel.rectangle = function(w, h, s, c) {
	// Fixme: get the pinholes!
	return new RectangleImage(
	    plt.types.NumberTower.toInteger(w),
	    plt.types.NumberTower.toInteger(h),
	    s,
	    c);
    };


    var BaseImage = plt.Kernel.BaseImage;

    
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


   
    function FileImage(src, rawImage) {
	BaseImage.call(this, 0, 0);
	var self = this;
	this.isLoaded = false;
	if (rawImage && rawImage.complete) { 
	    this.img = rawImage;
	    this.isLoaded = true;
	    this.pinholeX = self.img.width / 2;
	    this.pinholeY = self.img.height / 2;
	} else {
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
	    this.img.onerror = function(e) {
		self.img.onerror = "";
		self.img.src = "http://www.wescheme.org/images/broken.png";
	    }
	    this.img.src = src;
	}
    }
    FileImage.prototype = heir(BaseImage.prototype);
    plt.world.Kernel.FileImage = FileImage;

 
    var imageCache = {};
    FileImage.makeInstance = function(path) {
	if (! (path in imageCache)) {
	    imageCache[path] = new FileImage(path);
	} 
	return imageCache[path];
    };
    FileImage.installInstance = function(path, rawImage) {
	imageCache[path] = new FileImage(path, rawImage);
    }


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
     * applyEffect: compound-effect -> (arrayof (world -> world))

     applyEffect applies all of the effects

     @param aCompEffect a compound effect is either a scheme list of
     compound effects or a single primitive effect */

    plt.world.Kernel.applyEffect = function(aCompEffect) {
	var results = [];
	if (plt.Kernel.empty_question_(aCompEffect)) {
    	    // Do Nothing
    	} else if (plt.Kernel.pair_question_(aCompEffect)) {
    	    results = results.concat(
		plt.world.Kernel.applyEffect(aCompEffect.first()));
    	    results = results.concat(
		plt.world.Kernel.applyEffect(aCompEffect.rest()));
    	} else {
	    var newResult = aCompEffect.run();
	    if (newResult) {
		results = results.concat(newResult);
	    }
    	}
	return results;
    }


    effect_colon_none.prototype.run = function() {
	// Do nothing.
    };
    effect_colon_beep.prototype.run = function() {
	plt.platform.Platform.getInstance().getSoundService().beep();
    };
    effect_colon_play_dash_dtmf_dash_tone.prototype.run = function() {
	plt.platform.Platform.getInstance().getSoundService().playDtmfTone(
	    this._fields[0].toInteger(),
	    this._fields[1].toInteger());
    };
    effect_colon_send_dash_sms.prototype.run = function() {
	plt.platform.Platform.getInstance().getSmsService().send(
	    this._fields[0], this._fields[1]);
    };
    effect_colon_play_dash_sound.prototype.run = function() {
	if (plt.Kernel.isString(this._fields[0])) {
	    plt.platform.Platform.getInstance().getSoundService().playSoundUrl(
		this._fields[0]);
	} else if (playlist_dash_sound_question_(this._fields[0])) {
	    plt.platform.Platform.getInstance().getSoundService().playPlaylist(
		this._fields[0]._fields[0]);
	}
    };
    effect_colon_pause_dash_sound.prototype.run = function() {
	if (plt.Kernel.isString(this._fields[0])) {
	    plt.platform.Platform.getInstance().getSoundService().pauseSoundUrl(
		this._fields[0]);
	} else if (playlist_dash_sound_question_(this._fields[0])) {
	    plt.platform.Platform.getInstance().getSoundService().pausePlaylist(
		this._fields[0]._fields[0]);
	}
    };
    effect_colon_stop_dash_sound.prototype.run = function() {
	if (plt.Kernel.isString(this._fields[0])) {
	    plt.platform.Platform.getInstance().getSoundService().stopSoundUrl(
		this._fields[0]);
	} else if (playlist_dash_sound_question_(this._fields[0])) {
	    plt.platform.Platform.getInstance().getSoundService().stopPlaylist(
		this._fields[0]._fields[0]);
	}
    };
    effect_colon_set_dash_sound_dash_volume.prototype.run = function() {
	plt.platform.Platform.getInstance().getSoundService().setVolume(
	    this._fields[0].toInteger());
    };
    effect_colon_raise_dash_sound_dash_volume.prototype.run = function() {
	plt.platform.Platform.getInstance().getSoundService.raiseVolume();
    };
    effect_colon_lower_dash_sound_dash_volume.prototype.run = function() {
	plt.platform.Platform.getInstance().getSoundService.lowerVolume();
    };
    effect_colon_set_dash_wake_dash_lock.prototype.run = function() {
	plt.platform.Platform.getInstance().getPowerService().setWakeLock(
	    this._fields[0].toInteger());
    };
    effect_colon_release_dash_wake_dash_lock.prototype.run = function() {
	plt.platform.Platform.getInstance().getPowerService().releaseWakeLock();
    };
    effect_colon_pick_dash_playlist.prototype.run = function() {
	var updater = this._fields[0];
	var callback = function(playlist) {
	    var playlistSound = make_dash_playlist_dash_sound(playlist);
	    setTimeout(function() {
		var changeWorld = plt.world.config.CONFIG.lookup("changeWorld");
		changeWorld(function(w) {
		    return updater([w, playlistSound]);
		});
	    }, 0);
	}
	plt.platform.Platform.getInstance().getPickPlaylistService().pickPlaylist(callback);
    };
    effect_colon_pick_dash_random.prototype.run = function() {
	var aRandomNumber =
	    plt.types.Rational.makeInstance(
		Math.floor(plt.types.NumberTower.toInteger(this._fields[0]) * 
			   Math.random()),
		1);
	var callback = this._fields[1];
	return function(w) { return callback([w, aRandomNumber]) }
    };

    

//////////////////////////////////////////////////////////////////////////
 


})();
