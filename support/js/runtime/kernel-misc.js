goog.provide('plt.kernel.misc');

// Miscellaneous functions in the kernel.

(function() {

    var munge = function(name) {
	var C = plt.Kernel.invokeModule("moby/compiler").EXPORTS;
	return C.identifier_dash__greaterthan_munged_dash_java_dash_identifier(name);
    }

    // getModule: string -> module
    // Returns a module that knows how to map scheme names to javascript
    // names.
    var getModule = function(name) {
	var theModule = plt.Kernel.invokeModule(name);
	var exports = theModule.EXPORTS;
	return {
	    theModule: theModule,
	    getFunction: function(name) {
		return exports[munge(name)];
	    }};
    }


    // locHashToLoc: (hash | undefined) -> Loc
    // Converts the location hashes that are constructed by compiled
    // code and translates them back into Loc structures.
    var locHashToLoc = function(locHash) {
	var stxModule = getModule("moby/runtime/stx");
	var makeLoc = stxModule.getFunction('make-Loc');

	if (locHash === undefined) {
	    return makeLoc(plt.types.Rational.makeInstance(0),
			   plt.types.Rational.makeInstance(0),
			   plt.types.Rational.makeInstance(0),
			   plt.types.Rational.makeInstance(0),
			   "<undefined>");
	} else {
	    return makeLoc(plt.types.Rational.makeInstance(locHash.offset),
			   plt.types.Rational.makeInstance(locHash.line),
			   plt.types.Rational.makeInstance(locHash.column),
			   plt.types.Rational.makeInstance(locHash.span),
			   locHash.id);
	}
    };


    // throwCondExhaustedError: hash -> void
    // Throws a cond exhausted error
    plt.kernel.misc.throwCondExhaustedError = function(locHash) {
	var errorStruct = getModule("moby/runtime/error-struct");
	var makeMobyError = errorStruct.getFunction(
	    "make-moby-error");
	var makeCondExhaustedType = errorStruct.getFunction(
	    "conditional-exhausted");
	throw makeMobyError(locHashToLoc(plt.Kernel.lastLoc),
			    makeCondExhaustedType());
    };
}());
