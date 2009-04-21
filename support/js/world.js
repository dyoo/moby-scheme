org.plt.WorldKernel = {};
(function() {
    
    var world;
    var worldListeners = [];
    var stopped;
    
    function changeWorld(newWorld) {
	world = newWorld;
	notifyWorldListeners();
    }

    function notifyWorldListeners() {
	var i;
	for (i = 0; i < worldListeners.length; i++) {
	    worldListeners[i](world);
	}
    }
    
    function addWorldListener(aListener) {
	worldListeners.push(aListener);
    }
    


    org.plt.WorldKernel.bigBang = function(width, height, aWorld, handlers) {
	var i;
	stopped = false;

	for (i = 0; i < handlers.length; i++) {
	    handlers[i]();
	}
	
	window.onKeyDown = function(e) {
	    alert("I see you");
	}

	addWorldListener(function (w) {
	    if (org.plt.world.config.onRedraw) {
		var canvas = 
		    window.document.getElementById("canvas");
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
	    var anInterval = window.setInterval(
		function() {
		    if (stopped)
			window.clearInterval(anInterval);
		    else {
			changeWorld(
			    org.plt.world.config.onTick([world]));
		    }
		},
		org.plt.world.config.tickDelay);
	}

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
	return new FileImage(path.toString());
    };


    org.plt.WorldKernel.nwRectangle = function(w, h, s, c) {
	return new RectangleImage(
	    org.plt.types.NumberTower.toInteger(w),
	    org.plt.types.NumberTower.toInteger(h),
	    s,
	    c);
    }


    
    // SceneImage: primitive-number primitive-number (listof image) -> Scene
    function SceneImage(width, height, children) {
	this.width = width;
	this.height = height;
	this.children = children;
    }
    

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
	ctx.save();
	ctx.fillStyle = "white";
	ctx.fillRect(x, y, this.width, this.height);
	ctx.fillStyle = "black";
	ctx.strokeRect(x, y, this.width, this.height);
	ctx.restore();
	ctx.globalCompositeOperation = 'source-over';
	// Then ask every object to render itself.
	for(i = 0; i < this.children.length; i++) {
	    childImage = this.children[i][0];
	    childX = this.children[i][1];
	    childY = this.children[i][2];
	    ctx.save();
	    childImage.render(ctx,
			      childX + x,
			      childY + y);
	    ctx.restore();
	}
    };

    
    function FileImage(path) {
	this.img = new Image();
	this.img.src = path;
	// We should do something blocking here
	// for onload, since we don't know at
	// this time what the file size should be, nor
	// will drawImage do the right thing until the
	// file is loaded.
    }

    FileImage.prototype.render = function(ctx, x, y) {
	ctx.drawImage(this.img, x, y);
    };



    function RectangleImage(width, height, style, color) {
	this.width = width;
	this.height = height;
	this.style = style;
	this.color = color;
    }

    RectangleImage.prototype.render = function(ctx, x, y) {
	ctx.beginPath();
	ctx.fillStyle = this.color;
	ctx.rect(x, y, this.width, this.height);
	if (this.style.toLowerCase() == "outline") {
	    ctx.stroke();
	} else {
	    ctx.fill();
	}
	ctx.closePath();
    };



    
    function TextImage(msg, size, color) {
	this.msg = msg;
	this.size = size;
	this.color = color;
	this.font = "Verdana";
    }

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
    

    function CircleImage(radius, style, color) {
	this.radius = radius;
	this.style = style;
	this.color = color;
    }

    CircleImage.prototype.render = function(ctx, x, y) {
	ctx.translate(0, 0);
	ctx.beginPath();
	ctx.fillStyle = this.color;
	ctx.arc(x, y, this.radius, 0, 2*Math.PI, false);
	if (this.style.toLowerCase() == "outline")
	    ctx.stroke();
	else
	    ctx.fill();
	ctx.closePath();
    };
    


 
 

 
})();


org.plt.world = {};
org.plt.world.config = {
    // onRedraw: world -> scene
    onRedraw: false,
    tickDelay: false,
    onTick: false,
    onKey: false,
    stopWhen: false
};

org.plt.world.config.Kernel = {};
org.plt.world.config.Kernel.onRedraw = function(handler) {
    return function() {
	org.plt.world.config.onRedraw = handler;    
    };
};
org.plt.world.config.Kernel.onTick = function(aDelay, handler) {
    return function() {
	org.plt.world.config.tickDelay =
	    org.plt.types.NumberTower.toInteger
	(org.plt.types.NumberTower.multiply(
	    org.plt.types.Rational.makeInstance(1000, 1), 
	    aDelay));
	org.plt.world.config.onTick = handler;    
    };
};

org.plt.world.config.Kernel.stopWhen = function(handler) {
    return function() {
	org.plt.world.config.stopWhen = handler;    
    };
};


org.plt.world.config.Kernel.onKey = function(handler) {
    return function() {
	org.plt.world.config.onKey = handler;    
    };
};
