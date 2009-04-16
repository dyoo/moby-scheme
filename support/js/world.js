org.plt.WorldKernel = {};
(function() {
    
    var world;
    var worldListeners = [];
    
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
	for (i = 0; i < handlers.length; i++) {
	    handlers[i]();
	}

	addWorldListener(function (w) {
	    if (org.plt.world.config.onRedraw) {
		var canvas = 
		    window.document.getElementById("canvas");
		var context = 
		    canvas.getContext("2d");
		var aScene = 
		    org.plt.world.config.onRedraw(w);
		aScene.render(context,
			      0,
			      0);
	    }

	});
	
	changeWorld(aWorld);
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
			   this.children + [[anImage, x, y]]);
 };

 // render: 2d-context primitive-number primitive-number -> void
 SceneImage.prototype.render = function(context, x, y) {
     var i;
     var childImage, childX, childY;
     // Clear the scene.
     // Then ask every object to render itself.
     for(i = 0; i < this.children.length; i++) {
	 childImage = this.children[i][0];
	 childX = this.children[i][1];
	 childY = this.children[i][2];
	 childImage.render(context,
			   org.plt.types.NumberTower.add(childX, x),
			   org.plt.types.NumberTower.add(childY, y));
     }
 };

 
 function TextImage() {
     return 0;
 }

 TextImage.prototype.render = function(context, x, y) {
     alert("I'm drawing.");
 };
 
 

 
 
})();


org.plt.world = {};
org.plt.world.config = {
    // onRedraw: world -> scene
    onRedraw: false
};

org.plt.world.config.Kernel = {};
org.plt.world.config.Kernel.onRedraw = function(handler) {
    return function() {
	org.plt.world.config.onRedraw = handler;    
    };
};