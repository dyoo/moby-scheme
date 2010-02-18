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
	    "make-moby-error-type:branch-value-not-boolean");
	var aLoc = stxStruct.getFunction("sexp->Loc")(locSexp);
	var err = makeMobyError(aLoc, makeErrorType(aVal));
	throw err;
    };


    // isMobyError: error -> boolean
    // Returns true if the error is a Moby error.
    plt.kernel.misc.isMobyError = function(err) {
	var errorStruct = getModule("moby/runtime/error-struct");
	return errorStruct.getFunction("moby-error?")(err);
    };


    // mobyErrorToDom: moby-error -> dom-element
    plt.kernel.misc.mobyErrorToDom = function(err) {
	var errorDomStruct = getModule("moby/runtime/error-struct-to-dom");
	return plt.kernel.misc.sexpToDom(
	    errorDomStruct.getFunction("error-struct->dom-sexp")(err));
    };


    // mobyErrorToString: moby-error -> string
    plt.kernel.misc.mobyErrorToString = function(err) {
	var toDomStruct = getModule("moby/runtime/dom-helpers");
	var errorDomStruct = getModule("moby/runtime/error-struct-to-dom");
	return (toDomStruct.getFunction("dom-string-content")(
	    errorDomStruct.getFunction("error-struct->dom-sexp")(err)));
    };



    // sexpToDom: sexp -> dom-element
    plt.kernel.misc.sexpToDom = function(anSexp) {
	if (typeof(anSexp) === 'undefined') {
	    return document.createTextNode("undefined");
	}
	if (typeof(anSexp) === 'string') {
	    return document.createTextNode(anSexp)
	} else if (anSexp.hasOwnProperty('nodeType')) {
	    return anSexp;
	} else {
	    var nodeType = 
		plt.Kernel.symbol_dash__greaterthan_string(
		    plt.Kernel.list_dash_ref(anSexp, plt.types.Rational.ZERO));
	    var nodeAttrList =
		plt.Kernel.list_dash_ref(anSexp, plt.types.Rational.ONE);
	    var nodeChildList =
		plt.Kernel.rest(plt.Kernel.rest(anSexp));
	    
	    var newNode = document.createElement(nodeType);
	    while (! plt.Kernel.empty_question_(nodeAttrList)) {
		var attrName = 
		    plt.Kernel.symbol_dash__greaterthan_string(
			plt.Kernel.first(plt.Kernel.first(nodeAttrList)));
		var attrValue = 
		    plt.Kernel.second(plt.Kernel.first(nodeAttrList));
		if (attrName === "style") {
		    newNode.style.cssText = attrValue;
		} else if (attrName === "class") {
		    newNode.className = attrValue;
		} else {
		    newNode[attrName] = attrValue;
		}

		nodeAttrList = plt.Kernel.rest(nodeAttrList);
	    }

	    while (! plt.Kernel.empty_question_(nodeChildList)) {
		newNode.appendChild(plt.kernel.misc.sexpToDom(plt.Kernel.first(nodeChildList)));
		nodeChildList = plt.Kernel.rest(nodeChildList);
	    }

	    
	    return newNode;
	}
    };



    plt.kernel.misc.checkOperatorIsFunction = function(opVal, who, locSexp) {
	if (typeof(opVal) === 'function') {
	    return opVal;
	} else {
	    var stxStruct = getModule("moby/runtime/stx");
	    var errorStruct = getModule("moby/runtime/error-struct");
	    var makeMobyError = errorStruct.getFunction(
		"make-moby-error");
	    var makeApplicationOperatorNotFunction = errorStruct.getFunction(
		"make-moby-error-type:application-operator-not-a-function");
	    var aLoc = stxStruct.getFunction("sexp->Loc")(locSexp);
	    var err = makeMobyError(aLoc, makeApplicationOperatorNotFunction(who, opVal));
	    throw err;
	}
    };


    // Returns the last value passed into returnLastArgument.
    // Used to implement semantics of BEGIN.
    plt.kernel.misc.returnLastArgument = function() {
	return arguments[arguments.length -1];
    };
}());
