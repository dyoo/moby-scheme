org.plt.WorldKernel = {};
(function() {
    org.plt.WorldKernel.bigBang = function(width, height, delay, handlers) {
	alert("Big bang!");
	var canvas = window.document.getElementById("canvas");
    }

})();


org.plt.world = {};
org.plt.world.config = {
    onRedraw: function(world) {
        alert("onRedraw not defined");
    }
};

org.plt.world.config.Kernel = {};
org.plt.world.config.Kernel.onRedraw = function(handler) {
    org.plt.world.config.onRedraw = handler;
}