goog.provide('plt.kernel.misc');

// Miscellaneous functions in the kernel.

(function() {

    // munge: string -> string
    var munge = function(name) {
	var C = plt.Kernel.invokeModule("moby/compiler").EXPORTS;
	return (C.identifier_dash__greaterthan_munged_dash_java_dash_identifier(
	    plt.types.Symbol.makeInstance(name))).toString();
    }

    // getModule: string -> module
    // Returns a module that knows how to map scheme names to javascript
    // names.
    var getModule = function(name) {
	var theModule = plt.Kernel.invokeModule(name);
	var exports = theModule.EXPORTS;
	return {
	    theModule: theModule,
	    getFunction: function(n) {
		return exports[munge(n)];
	    }};
    }



    // throwCondExhaustedError: sexp -> void
    // Throws a cond exhausted error
    plt.kernel.misc.throwCondExhaustedError = function(locSexp) {
	var stxStruct = getModule("moby/runtime/stx");
	var errorStruct = getModule("moby/runtime/error-struct");
	var makeMobyError = errorStruct.getFunction(
	    "make-moby-error");
	var makeCondExhaustedType = errorStruct.getFunction(
	    "make-moby-error-type:conditional-exhausted");
	var aLoc = stxStruct.getFunction("sexp->Loc")(locSexp);
	var err = makeMobyError(aLoc, makeCondExhaustedType());
	throw err;
    };


    // verifyBooleanBranchValue: any loc-sexp -> boolean
    // Verifies that aVal is a boolean.
    // If not, throws branch-value-not-boolean.
    plt.kernel.misc.verifyBooleanBranchValue = function(aVal, locSexp) {
	if (aVal === plt.types.Logic.TRUE || 
	    aVal === plt.types.Logic.FALSE) {
	    return aVal;
	}
	var stxStruct = getModule("moby/runtime/stx");
	var errorStruct = getModule("moby/runtime/error-struct");
	var makeMobyError = errorStruct.getFunction(
	    "make-moby-error");
	var makeErrorType = errorStruct.getFunction(
	    "make-moby-error:->branch-value-not-boolean");
	var aLoc = stxStruct.getFunction("sexp->Loc")(locSexp);
	var err = makeMobyError(aLoc, makeErrorType());
	throw err;
    };



}());
