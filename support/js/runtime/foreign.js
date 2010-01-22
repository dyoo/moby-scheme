goog.require('plt.Kernel');
goog.require('plt.types');


goog.provide('plt.foreign');


// Foreign function interface to Javascript.


(function() {
    plt.foreign.get_dash_js_dash_object = function(thing, name) {
	plt.Kernel.check(name, 
			 plt.Kernel.isString,
			 "get-js-object", "string", "string", 2);
	return coerseToSchemeValue(name, (thing || window)[name.toString()]);
    };


    //////////////////////////////////////////////////////////////////////




    var coerseToSchemeValue = function(name, x) {
	if (typeof(x) == 'function') {
	    return plt.types.liftToplevelToFunctionValue(
		function(args) { 
		    var newArgs = [];
		    for (var i = 0; i < args.length; i++) {
			newArgs.push(coerseToJavascriptValue(args[i]));
		    }
		    return coerseToSchemeValue(
			x.apply(null, newArgs))}, 
		name,
		0,
		plt.types.Cons.makeInstance(
		    plt.types.Symbol.makeInstance("at-least"),
		    plt.types.Cons.makeInstance(plt.types.Rational.ZERO,
						plt.types.Empty.EMPTY)));
	} else if (typeof(x) == 'number') {
	    return plt.types.FloatPoint.makeInstance(x);
	} else if (typeof(x) == 'undefined') {
	    return x;
	} else if (typeof(x) == 'object') {
	    return x;
	} else if (typeof(x) == 'boolean') {
	    return x ? plt.types.Logic.TRUE: plt.types.Logic.FALSE;
	} else if (typeof(x) == 'string') {
	    return plt.types.Rational.makeInstance(x);
	} else {
	    return x;
	}
    };


    var coerseToJavascriptValue = function(x) {
	// Fixme: we need to coerse
	// string
	// number
	// boolean
	// char
	// symbol
	// list
	// vector
	// struct
	// box
	// hash
	// function
	return x;
    };

}());