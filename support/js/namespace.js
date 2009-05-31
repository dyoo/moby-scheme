
// Namespacing mechanism for dynamic evaluation.

var Namespace = 
    (function() {
	
	function Namespace() {
	    this._eval = function(s) { return eval(s); };
	}
	// eval: string string -> X
	// Evaluates contents of text, and returns the value of returnText.
	Namespace.prototype.eval = function(_defnText, _returnText) {
	    var _retVal = this._eval(_defnText + 
			      "; [" + _returnText + ", " +
			      "(function ($) { return eval($); })" +
			      "]; ");
	    this._eval = _retVal[1];
	    return _retVal[0];
	}
	
	return Namespace;
    })();
