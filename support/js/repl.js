var Repl = (function(textarea) {
    function Repl() {
	this.textarea = textarea;
	this.ns = new Namespace();
    };
    
    Repl.prototype.onNotify = function() {
    };


    Repl.prototype.eval = function(s) {
    };


    return Repl;
})();