
// Depends on kernel.js, world-config.js, effect-struct.js
if (typeof(plt) == 'undefined') { plt = {}; }
plt.world = plt.world || {};
plt.world.Kernel = plt.world.Kernel || {};
(function() {
    
    var world;
    var worldListeners = [];
    var stopped;
    var timerInterval = false;


    // Inheritance from pg 168: Javascript, the Definitive Guide.
    var heir = function(p) {
	var f = function() {}
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
    var changeWorld = function(newWorld) {
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
    var notifyWorldListeners = function() {
	var i;
	for (i = 0; i < worldListeners.length; i++) {
	    worldListeners[i](world);
	}
    }

    // addWorldListener: (world -> void) -> void
    // Adds a new world listener: whenever the world is changed, the aListener
    // will be called with that new world.
    var addWorldListener = function(aListener) {
	worldListeners.push(aListener);
    }
    

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
	} else {
	    keyname = String.fromCharCode(code); 
	}
	return keyname;
    }


    // resetWorld: -> void
    // Resets all of the world global values.
    var resetWorld = function() {
	if (timerInterval) {
	    clearInterval(timerInterval);
	    timerInterval = false;
	}
	stopped = false;
	worldListeners = [];
    }


    var getBigBangWindow = function(width, height) {
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
	plt.Kernel.check(width, plt.Kernel.isNumber, "big-bang", "number", 1);
	plt.Kernel.check(height, plt.Kernel.isNumber, "big-bang", "number", 2);
	plt.Kernel.arrayEach(args, function(x, i) { 
	    plt.Kernel.check(x, plt.Kernel.isFunction, "big-bang", "handler", i+4) });
	

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
	    newWindow.onkeydown = function(e) {
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
    var scheduleTimerTick = function(window, config) {
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
	plt.Kernel.check(thing, isImage, "image-width", "image", 1);
	return plt.types.Rational.makeInstance(thing.getWidth(), 1);
    };


    plt.world.Kernel.imageHeight = function(thing) {
	plt.Kernel.check(thing, isImage, "image-height", "image", 1);
	return plt.types.Rational.makeInstance(thing.getHeight(), 1);
    };


    // placeImage: image number number image -> scene
    plt.world.Kernel.placeImage = function(picture, x, y, background) {
	plt.Kernel.check(picture, 
			 isImage,
			 "place-image",
			 "image",
			 1);
	plt.Kernel.check(x, plt.Kernel.isNumber, "place-image", "number", 2);
	plt.Kernel.check(y, plt.Kernel.isNumber, "place-image", "number", 3);
	plt.Kernel.check(background,
			 function(x) { return isScene(x) || isImage(x) },
			 "place-image", "image", 4);
	if (isScene(background)) {
	    return background.add(picture,
				  plt.types.NumberTower.toInteger(x),
				  plt.types.NumberTower.toInteger(y));
	} else {
	    var newScene = new SceneImage(background.getWidth(),
					  background.getHeight(),
					  []);
	    newScene = newScene.add(background, 0, 0);
	    newScene = newScene.add(picture, 
				    plt.types.NumberTower.toInteger(x),
				    plt.types.NumberTower.toInteger(y));
	    return newScene;
	}
    };

    
    // emptyScene: number number -> scene
    plt.world.Kernel.emptyScene = function(width, height) {
	plt.Kernel.check(width, plt.Kernel.isNumber, "empty-scene", "number", 1);
	plt.Kernel.check(height, plt.Kernel.isNumber, "empty-scene", "number", 2);
	return new SceneImage(
	    plt.types.NumberTower.toInteger(width), 
	    plt.types.NumberTower.toInteger(height),
	    []);
    };



    // isColor: any -> boolean

    // Produces true if the thing is considered a color object.
    var isColor = function(thing) {
	return typeof(colorDb.get(thing)) != 'undefined';
    };



    // text: string number color -> TextImage
    plt.world.Kernel.text = function(aString, aSize, aColor) {
	plt.Kernel.check(aString, plt.Kernel.isString, "text", "string", 1);
	plt.Kernel.check(aSize, plt.Kernel.isNumber, "text", "number", 2);
	plt.Kernel.check(aColor, isColor, "text", "color", 3);

	if (colorDb.get(aColor)) {
	    aColor = colorDb.get(aColor);
	}
	return new TextImage
	(aString, 
	 plt.types.NumberTower.toInteger(aSize), 
	 aColor);
    };


    // circle: number style color -> TextImage
    plt.world.Kernel.circle = function(aRadius, aStyle, aColor) {
	plt.Kernel.check(aRadius, plt.Kernel.isNumber, "circle", "number", 1);
	plt.Kernel.check(aStyle, plt.Kernel.isString, "circle", "string", 2);
	plt.Kernel.check(aColor, isColor, "circle", "color", 3);


	if (colorDb.get(aColor)) {
	    aColor = colorDb.get(aColor);
	}
	return new CircleImage
	(plt.types.NumberTower.toInteger(aRadius), 
	 aStyle,
	 aColor);
    };


    plt.world.Kernel.openImageUrl = function(path) {
	plt.Kernel.check(path, plt.Kernel.isString, "open-image-url", "string", 1);
	return FileImage.makeInstance(path.toString());
    };


    plt.world.Kernel.nwRectangle = function(w, h, s, c) {
	plt.Kernel.check(w, plt.Kernel.isNumber, "nw:rectangle", "number", 1);
	plt.Kernel.check(h, plt.Kernel.isNumber, "nw:rectangle", "number", 2);
	plt.Kernel.check(s, plt.Kernel.isString, "nw:rectangle", "string", 3);
	plt.Kernel.check(c, isColor, "nw:rectangle", "color", 4);

	if (colorDb.get(c)) {
	    c = colorDb.get(c);
	}
	var aRect = new RectangleImage
	(plt.types.NumberTower.toInteger(w),
	 plt.types.NumberTower.toInteger(h),
	 s,
	 c);
	return aRect.updatePinhole(0, 0);
    };

    plt.world.Kernel.rectangle = function(w, h, s, c) {
	plt.Kernel.check(w, plt.Kernel.isNumber, "rectangle", "number", 1);
	plt.Kernel.check(h, plt.Kernel.isNumber, "rectangle", "number", 2);
	plt.Kernel.check(s, plt.Kernel.isString, "rectangle", "string", 3);
	plt.Kernel.check(c, isColor, "rectangle", "color", 4);

	if (colorDb.get(c)) {
	    c = colorDb.get(c);
	}
	// Fixme: get the pinholes!
	return new RectangleImage(
	    plt.types.NumberTower.toInteger(w),
	    plt.types.NumberTower.toInteger(h),
	    s,
	    c);
    };





    // Base class for all images.
    var BaseImage = function(pinholeX, pinholeY) {
	this.pinholeX = pinholeX;
	this.pinholeY = pinholeY;
    }
    plt.world.Kernel.BaseImage = BaseImage;


    var isImage = function(thing) {
	return ((thing != null) && (thing != undefined)
		&& (thing instanceof BaseImage));
    }
    plt.world.Kernel.isImage = isImage;


    plt.world.Kernel.put_dash_pinhole = function(img, x, y) {
	plt.Kernel.check(img, isImage, "put-pinhole", "image", 1);
	plt.Kernel.check(x, plt.Kernel.isNumber, "put-pinhole", "number", 2);
	plt.Kernel.check(y, plt.Kernel.isNumber, "put-pinhole", "number", 3);
	return img.updatePinhole(x.toInteger(), y.toInteger());
    };


    BaseImage.prototype.updatePinhole = function(x, y) {
	var aCopy = {};
	for (attr in this) {
	    aCopy[attr] = this[attr];
	}
	aCopy.__proto__ = this.__proto__;
	aCopy.pinholeX = x;
	aCopy.pinholeY = y;
	return aCopy;
    };


    BaseImage.prototype.render = function(ctx, x, y) {
	throw new MobyRuntimeError("Unimplemented method render");
    };


    // makeCanvas: number number -> canvas
    // Constructs a canvas object of a particular width and height.
    plt.world.Kernel.makeCanvas = function(width, height) {
	var canvas = document.createElement("canvas");
 	canvas.width = width;
 	canvas.height = height;
 	canvas.style.width = canvas.width + "px";
 	canvas.style.height = canvas.height + "px";
	
	// KLUDGE: IE compatibility uses /js/excanvas.js, and dynamic
	// elements must be marked this way.
	if (window && typeof window.G_vmlCanvasManager != 'undefined') {
	    canvas.style.display = 'none';
	    document.body.appendChild(canvas);
	    canvas = window.G_vmlCanvasManager.initElement(canvas);
	    document.body.removeChild(canvas);
	    canvas.style.display = '';
	}
	return canvas;
    };


    BaseImage.prototype.toDomNode = function(cache) {
	var that = this;
	var width = plt.world.Kernel.imageWidth(that).toInteger();
	var height = plt.world.Kernel.imageHeight(that).toInteger();
	var canvas = plt.world.Kernel.makeCanvas(width, height);

	// KLUDGE: some of the rendering functions depend on a context
	// where the canvas is attached to the DOM tree.  So we temporarily
	// make it invisible, attach it to the tree, render, and then rip it out
	// again.
	var oldDisplay = canvas.style.display;
	canvas.style.display = 'none';
	document.body.appendChild(canvas);
 	var ctx = canvas.getContext("2d");
	that.render(ctx, 0, 0) 
	document.body.removeChild(canvas);
	canvas.style.display = '';

	return canvas;
    };
    BaseImage.prototype.toWrittenString = function(cache) { return "<image>"; }
    BaseImage.prototype.toDisplayedString = function(cache) { return "<image>"; }



    plt.world.Kernel.image_question_ = function(thing) {
	return isImage(thing);
    };


    plt.world.Kernel.image_equal__question_ = function(thing, other) {
	check(thing, isImage, "image=?", "image", 1);
	check(other, isImage, "image=?", "image", 2);
	return thing == other ? plt.types.Logic.TRUE : plt.types.Logic.FALSE;
    };


    
    // isScene: any -> boolean
    // Produces true when x is a scene.
    var isScene = function(x) {
	return ((x != undefined) && (x != null) && (x instanceof SceneImage));
    };

    // SceneImage: primitive-number primitive-number (listof image) -> Scene
    var SceneImage = function(width, height, children) {
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


   
    var FileImage = function(src, rawImage) {
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

    // Override toDomNode: we don't need a full-fledged canvas here.
    FileImage.prototype.toDomNode = function(cache) {
	return this.img.cloneNode(true);
    };



    var RectangleImage = function(width, height, style, color) {
	BaseImage.call(this, width/2, height/2);
	this.width = width;
	this.height = height;
	this.style = style;
	this.color = color;
    };
    RectangleImage.prototype = heir(BaseImage.prototype);


    RectangleImage.prototype.render = function(ctx, x, y) {
	ctx.fillStyle = this.color.toString();
	if (this.style.toString().toLowerCase() == "outline") {
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



    
    var TextImage = function(msg, size, color) {
	BaseImage.call(this, 0, 0);
	this.msg = msg;
	this.size = size;
	this.color = color;
	this.font = "Optimer";
    }
    TextImage.prototype = heir(BaseImage.prototype);

    TextImage.prototype.render = function(ctx, x, y) {
	if(ctx.fillText) {
	    ctx.font = this.size +"px Optimer";
	    ctx.fillStyle = this.color.toRGBAString();
	    ctx.fillText(this.msg, x, y);
	}
	else if ('mozDrawText' in ctx) {
	    ctx.mozTextStyle=this.size+"px "+this.font;
	    // Fix me: I don't quite know how to get the
	    // baseline right.
	    ctx.translate(x, y + this.size);
	    ctx.fillStyle = this.color;
	    ctx.mozDrawText(this.msg);
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


    var CircleImage = function(radius, style, color) {
	BaseImage.call(this, radius, radius);
	this.radius = radius;
	this.style = style;
	this.color = color;
    }
    CircleImage.prototype = heir(BaseImage.prototype);

    CircleImage.prototype.render = function(ctx, x, y) {
	ctx.translate(0, 0);
	ctx.beginPath();
	ctx.fillStyle = this.color.toString();
	ctx.arc(x + this.radius,
		y + this.radius, 
		this.radius, 0, 2*Math.PI, false);
	if (this.style.toString().toLowerCase() == "outline")
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









    // Color database
    var ColorDb = function() {
	this.colors = {};
    }
    ColorDb.prototype.put = function(name, color) {
	this.colors[name] = color;
    };

    ColorDb.prototype.get = function(name) {
	return this.colors[name.toString().toUpperCase()];
    };

    var ColorRecord = function(r, g, b) {
	this.r = r;
	this.g = g;
	this.b = b;
    };

    ColorRecord.prototype.toString = function() {
	return "rgb(" + this.r + "," + this.g + "," + this.b + ")";
    };

    ColorRecord.prototype.toRGBAString = function() {
	return "rgba(" + this.r + "," + this.g + "," + this.b + ", 1)";
    }

    var colorDb = new ColorDb();
    colorDb.put("ORANGE", new ColorRecord(255, 165, 0));
    colorDb.put("RED", new ColorRecord(255, 0, 0));
    colorDb.put("ORANGERED", new ColorRecord(255, 69, 0));
    colorDb.put("TOMATO", new ColorRecord(255, 99, 71));
    colorDb.put("DARKRED", new ColorRecord(139, 0, 0));
    colorDb.put("RED", new ColorRecord(255, 0, 0));
    colorDb.put("FIREBRICK", new ColorRecord(178, 34, 34));
    colorDb.put("CRIMSON", new ColorRecord(220, 20, 60));
    colorDb.put("DEEPPINK", new ColorRecord(255, 20, 147));
    colorDb.put("MAROON", new ColorRecord(176, 48, 96));
    colorDb.put("INDIAN RED", new ColorRecord(205, 92, 92));
    colorDb.put("INDIANRED", new ColorRecord(205, 92, 92));
    colorDb.put("MEDIUM VIOLET RED", new ColorRecord(199, 21, 133));
    colorDb.put("MEDIUMVIOLETRED", new ColorRecord(199, 21, 133));
    colorDb.put("VIOLET RED", new ColorRecord(208, 32, 144));
    colorDb.put("VIOLETRED", new ColorRecord(208, 32, 144));
    colorDb.put("LIGHTCORAL", new ColorRecord(240, 128, 128));
    colorDb.put("HOTPINK", new ColorRecord(255, 105, 180));
    colorDb.put("PALEVIOLETRED", new ColorRecord(219, 112, 147));
    colorDb.put("LIGHTPINK", new ColorRecord(255, 182, 193));
    colorDb.put("ROSYBROWN", new ColorRecord(188, 143, 143));
    colorDb.put("PINK", new ColorRecord(255, 192, 203));
    colorDb.put("ORCHID", new ColorRecord(218, 112, 214));
    colorDb.put("LAVENDERBLUSH", new ColorRecord(255, 240, 245));
    colorDb.put("SNOW", new ColorRecord(255, 250, 250));
    colorDb.put("CHOCOLATE", new ColorRecord(210, 105, 30));
    colorDb.put("SADDLEBROWN", new ColorRecord(139, 69, 19));
    colorDb.put("BROWN", new ColorRecord(132, 60, 36));
    colorDb.put("DARKORANGE", new ColorRecord(255, 140, 0));
    colorDb.put("CORAL", new ColorRecord(255, 127, 80));
    colorDb.put("SIENNA", new ColorRecord(160, 82, 45));
    colorDb.put("ORANGE", new ColorRecord(255, 165, 0));
    colorDb.put("SALMON", new ColorRecord(250, 128, 114));
    colorDb.put("PERU", new ColorRecord(205, 133, 63));
    colorDb.put("DARKGOLDENROD", new ColorRecord(184, 134, 11));
    colorDb.put("GOLDENROD", new ColorRecord(218, 165, 32));
    colorDb.put("SANDYBROWN", new ColorRecord(244, 164, 96));
    colorDb.put("LIGHTSALMON", new ColorRecord(255, 160, 122));
    colorDb.put("DARKSALMON", new ColorRecord(233, 150, 122));
    colorDb.put("GOLD", new ColorRecord(255, 215, 0));
    colorDb.put("YELLOW", new ColorRecord(255, 255, 0));
    colorDb.put("OLIVE", new ColorRecord(128, 128, 0));
    colorDb.put("BURLYWOOD", new ColorRecord(222, 184, 135));
    colorDb.put("TAN", new ColorRecord(210, 180, 140));
    colorDb.put("NAVAJOWHITE", new ColorRecord(255, 222, 173));
    colorDb.put("PEACHPUFF", new ColorRecord(255, 218, 185));
    colorDb.put("KHAKI", new ColorRecord(240, 230, 140));
    colorDb.put("DARKKHAKI", new ColorRecord(189, 183, 107));
    colorDb.put("MOCCASIN", new ColorRecord(255, 228, 181));
    colorDb.put("WHEAT", new ColorRecord(245, 222, 179));
    colorDb.put("BISQUE", new ColorRecord(255, 228, 196));
    colorDb.put("PALEGOLDENROD", new ColorRecord(238, 232, 170));
    colorDb.put("BLANCHEDALMOND", new ColorRecord(255, 235, 205));
    colorDb.put("MEDIUM GOLDENROD", new ColorRecord(234, 234, 173));
    colorDb.put("MEDIUMGOLDENROD", new ColorRecord(234, 234, 173));
    colorDb.put("PAPAYAWHIP", new ColorRecord(255, 239, 213));
    colorDb.put("MISTYROSE", new ColorRecord(255, 228, 225));
    colorDb.put("LEMONCHIFFON", new ColorRecord(255, 250, 205));
    colorDb.put("ANTIQUEWHITE", new ColorRecord(250, 235, 215));
    colorDb.put("CORNSILK", new ColorRecord(255, 248, 220));
    colorDb.put("LIGHTGOLDENRODYELLOW", new ColorRecord(250, 250, 210));
    colorDb.put("OLDLACE", new ColorRecord(253, 245, 230));
    colorDb.put("LINEN", new ColorRecord(250, 240, 230));
    colorDb.put("LIGHTYELLOW", new ColorRecord(255, 255, 224));
    colorDb.put("SEASHELL", new ColorRecord(255, 245, 238));
    colorDb.put("BEIGE", new ColorRecord(245, 245, 220));
    colorDb.put("FLORALWHITE", new ColorRecord(255, 250, 240));
    colorDb.put("IVORY", new ColorRecord(255, 255, 240));
    colorDb.put("GREEN", new ColorRecord(0, 255, 0));
    colorDb.put("LAWNGREEN", new ColorRecord(124, 252, 0));
    colorDb.put("CHARTREUSE", new ColorRecord(127, 255, 0));
    colorDb.put("GREEN YELLOW", new ColorRecord(173, 255, 47));
    colorDb.put("GREENYELLOW", new ColorRecord(173, 255, 47));
    colorDb.put("YELLOW GREEN", new ColorRecord(154, 205, 50));
    colorDb.put("YELLOWGREEN", new ColorRecord(154, 205, 50));
    colorDb.put("MEDIUM FOREST GREEN", new ColorRecord(107, 142, 35));
    colorDb.put("OLIVEDRAB", new ColorRecord(107, 142, 35));
    colorDb.put("MEDIUMFORESTGREEN", new ColorRecord(107, 142, 35));
    colorDb.put("DARK OLIVE GREEN", new ColorRecord(85, 107, 47));
    colorDb.put("DARKOLIVEGREEN", new ColorRecord(85, 107, 47));
    colorDb.put("DARKSEAGREEN", new ColorRecord(143, 188, 139));
    colorDb.put("LIME", new ColorRecord(0, 255, 0));
    colorDb.put("DARK GREEN", new ColorRecord(0, 100, 0));
    colorDb.put("DARKGREEN", new ColorRecord(0, 100, 0));
    colorDb.put("LIME GREEN", new ColorRecord(50, 205, 50));
    colorDb.put("LIMEGREEN", new ColorRecord(50, 205, 50));
    colorDb.put("FOREST GREEN", new ColorRecord(34, 139, 34));
    colorDb.put("FORESTGREEN", new ColorRecord(34, 139, 34));
    colorDb.put("SPRING GREEN", new ColorRecord(0, 255, 127));
    colorDb.put("SPRINGGREEN", new ColorRecord(0, 255, 127));
    colorDb.put("MEDIUM SPRING GREEN", new ColorRecord(0, 250, 154));
    colorDb.put("MEDIUMSPRINGGREEN", new ColorRecord(0, 250, 154));
    colorDb.put("SEA GREEN", new ColorRecord(46, 139, 87));
    colorDb.put("SEAGREEN", new ColorRecord(46, 139, 87));
    colorDb.put("MEDIUM SEA GREEN", new ColorRecord(60, 179, 113));
    colorDb.put("MEDIUMSEAGREEN", new ColorRecord(60, 179, 113));
    colorDb.put("AQUAMARINE", new ColorRecord(112, 216, 144));
    colorDb.put("LIGHTGREEN", new ColorRecord(144, 238, 144));
    colorDb.put("PALE GREEN", new ColorRecord(152, 251, 152));
    colorDb.put("PALEGREEN", new ColorRecord(152, 251, 152));
    colorDb.put("MEDIUM AQUAMARINE", new ColorRecord(102, 205, 170));
    colorDb.put("MEDIUMAQUAMARINE", new ColorRecord(102, 205, 170));
    colorDb.put("TURQUOISE", new ColorRecord(64, 224, 208));
    colorDb.put("LIGHTSEAGREEN", new ColorRecord(32, 178, 170));
    colorDb.put("MEDIUM TURQUOISE", new ColorRecord(72, 209, 204));
    colorDb.put("MEDIUMTURQUOISE", new ColorRecord(72, 209, 204));
    colorDb.put("HONEYDEW", new ColorRecord(240, 255, 240));
    colorDb.put("MINTCREAM", new ColorRecord(245, 255, 250));
    colorDb.put("ROYALBLUE", new ColorRecord(65, 105, 225));
    colorDb.put("DODGERBLUE", new ColorRecord(30, 144, 255));
    colorDb.put("DEEPSKYBLUE", new ColorRecord(0, 191, 255));
    colorDb.put("CORNFLOWERBLUE", new ColorRecord(100, 149, 237));
    colorDb.put("STEEL BLUE", new ColorRecord(70, 130, 180));
    colorDb.put("STEELBLUE", new ColorRecord(70, 130, 180));
    colorDb.put("LIGHTSKYBLUE", new ColorRecord(135, 206, 250));
    colorDb.put("DARK TURQUOISE", new ColorRecord(0, 206, 209));
    colorDb.put("DARKTURQUOISE", new ColorRecord(0, 206, 209));
    colorDb.put("CYAN", new ColorRecord(0, 255, 255));
    colorDb.put("AQUA", new ColorRecord(0, 255, 255));
    colorDb.put("DARKCYAN", new ColorRecord(0, 139, 139));
    colorDb.put("TEAL", new ColorRecord(0, 128, 128));
    colorDb.put("SKY BLUE", new ColorRecord(135, 206, 235));
    colorDb.put("SKYBLUE", new ColorRecord(135, 206, 235));
    colorDb.put("CADET BLUE", new ColorRecord(96, 160, 160));
    colorDb.put("CADETBLUE", new ColorRecord(95, 158, 160));
    colorDb.put("DARK SLATE GRAY", new ColorRecord(47, 79, 79));
    colorDb.put("DARKSLATEGRAY", new ColorRecord(47, 79, 79));
    colorDb.put("LIGHTSLATEGRAY", new ColorRecord(119, 136, 153));
    colorDb.put("SLATEGRAY", new ColorRecord(112, 128, 144));
    colorDb.put("LIGHT STEEL BLUE", new ColorRecord(176, 196, 222));
    colorDb.put("LIGHTSTEELBLUE", new ColorRecord(176, 196, 222));
    colorDb.put("LIGHT BLUE", new ColorRecord(173, 216, 230));
    colorDb.put("LIGHTBLUE", new ColorRecord(173, 216, 230));
    colorDb.put("POWDERBLUE", new ColorRecord(176, 224, 230));
    colorDb.put("PALETURQUOISE", new ColorRecord(175, 238, 238));
    colorDb.put("LIGHTCYAN", new ColorRecord(224, 255, 255));
    colorDb.put("ALICEBLUE", new ColorRecord(240, 248, 255));
    colorDb.put("AZURE", new ColorRecord(240, 255, 255));
    colorDb.put("MEDIUM BLUE", new ColorRecord(0, 0, 205));
    colorDb.put("MEDIUMBLUE", new ColorRecord(0, 0, 205));
    colorDb.put("DARKBLUE", new ColorRecord(0, 0, 139));
    colorDb.put("MIDNIGHT BLUE", new ColorRecord(25, 25, 112));
    colorDb.put("MIDNIGHTBLUE", new ColorRecord(25, 25, 112));
    colorDb.put("NAVY", new ColorRecord(36, 36, 140));
    colorDb.put("BLUE", new ColorRecord(0, 0, 255));
    colorDb.put("INDIGO", new ColorRecord(75, 0, 130));
    colorDb.put("BLUE VIOLET", new ColorRecord(138, 43, 226));
    colorDb.put("BLUEVIOLET", new ColorRecord(138, 43, 226));
    colorDb.put("MEDIUM SLATE BLUE", new ColorRecord(123, 104, 238));
    colorDb.put("MEDIUMSLATEBLUE", new ColorRecord(123, 104, 238));
    colorDb.put("SLATE BLUE", new ColorRecord(106, 90, 205));
    colorDb.put("SLATEBLUE", new ColorRecord(106, 90, 205));
    colorDb.put("PURPLE", new ColorRecord(160, 32, 240));
    colorDb.put("DARK SLATE BLUE", new ColorRecord(72, 61, 139));
    colorDb.put("DARKSLATEBLUE", new ColorRecord(72, 61, 139));
    colorDb.put("DARKVIOLET", new ColorRecord(148, 0, 211));
    colorDb.put("DARK ORCHID", new ColorRecord(153, 50, 204));
    colorDb.put("DARKORCHID", new ColorRecord(153, 50, 204));
    colorDb.put("MEDIUMPURPLE", new ColorRecord(147, 112, 219));
    colorDb.put("CORNFLOWER BLUE", new ColorRecord(68, 64, 108));
    colorDb.put("MEDIUM ORCHID", new ColorRecord(186, 85, 211));
    colorDb.put("MEDIUMORCHID", new ColorRecord(186, 85, 211));
    colorDb.put("MAGENTA", new ColorRecord(255, 0, 255));
    colorDb.put("FUCHSIA", new ColorRecord(255, 0, 255));
    colorDb.put("DARKMAGENTA", new ColorRecord(139, 0, 139));
    colorDb.put("VIOLET", new ColorRecord(238, 130, 238));
    colorDb.put("PLUM", new ColorRecord(221, 160, 221));
    colorDb.put("LAVENDER", new ColorRecord(230, 230, 250));
    colorDb.put("THISTLE", new ColorRecord(216, 191, 216));
    colorDb.put("GHOSTWHITE", new ColorRecord(248, 248, 255));
    colorDb.put("WHITE", new ColorRecord(255, 255, 255));
    colorDb.put("WHITESMOKE", new ColorRecord(245, 245, 245));
    colorDb.put("GAINSBORO", new ColorRecord(220, 220, 220));
    colorDb.put("LIGHT GRAY", new ColorRecord(211, 211, 211));
    colorDb.put("LIGHTGRAY", new ColorRecord(211, 211, 211));
    colorDb.put("SILVER", new ColorRecord(192, 192, 192));
    colorDb.put("GRAY", new ColorRecord(190, 190, 190));
    colorDb.put("DARK GRAY", new ColorRecord(169, 169, 169));
    colorDb.put("DARKGRAY", new ColorRecord(169, 169, 169));
    colorDb.put("DIM GRAY", new ColorRecord(105, 105, 105));
    colorDb.put("DIMGRAY", new ColorRecord(105, 105, 105));
    colorDb.put("BLACK", new ColorRecord(0, 0, 0));









})();
