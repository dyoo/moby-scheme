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



var errorStructModule = 
    getModule("moby/runtime/error-struct");

var errorStructToDomModule = 
    getModule("moby/runtime/error-struct-to-dom");


// Returns true if the thing is a moby error
var isMobyError = function(x) {
    var result = errorStructModule.getFunction("moby-error?")(x);
    return result;
}
var mobyErrorType = function(x) {
    var result = errorStructModule.getFunction("moby-error-error-type")(x);
    return result;
}


//////////////////////////////////////////////////////////////////////


// errorType -> boolean
var isUnclosedLexicalToken = function(x) {
    return errorStructModule.getFunction(
	"moby-error-type:unclosed-lexical-token?")(x);
}

// errorType -> boolean
var isUnrecognizedLexicalToken = function(x) {
    return errorStructModule.getFunction(
	"moby-error-type:unrecognized-lexical-token?")(x);
}


// errorType -> boolean
var isUnsupportedLexicalToken = function(x) {
    return errorStructModule.getFunction(
	"moby-error-type:unsupported-lexical-token?")(x);
}


// errorType -> boolean
var isUnsupportedExpressionForm = function(x) {
    return errorStructModule.getFunction(
	"moby-error-type:unsupported-expression-form?")(x);
}

// errorType -> boolean
var isUnclosedParentheses = function(x) {
    return errorStructModule.getFunction(
	"moby-error-type:unclosed-parentheses?")(x);
}

// errorType -> boolean
var isMissingExpression = function(x) {
    return errorStructModule.getFunction(
	"moby-error-type:missing-expression?")(x);
}

// errorType -> boolean
var isDuplicateIdentifier = function(x) {
    return errorStructModule.getFunction(
	"moby-error-type:duplicate-identifier?")(x);
}

// errorType -> boolean
var isExpectedIdentifier = function(x) {
    return errorStructModule.getFunction(
	"moby-error-type:expected-identifier?")(x);
}

// errorType -> boolean
var isUndefinedIdentifier = function(x) {
    return errorStructModule.getFunction(
	"moby-error-type:undefined-identifier?")(x);
}

// errorType -> boolean
var isStructureIdentifierNotExpression = function(x) {
    return errorStructModule.getFunction(
	"moby-error-type:structure-identifier-not-expression?")(x);
}

// errorType -> boolean
var isProvidedNameNotDefined = function(x) {
    return errorStructModule.getFunction(
	"moby-error-type:provided-name-not-defined?")(x);
}

// errorType -> boolean
var isProvidedStructureNotStructure = function(x) {
    return errorStructModule.getFunction(
	"moby-error-type:provided-structure-not-structure?")(x);
}

// errorType -> boolean
var isUnknownModule = function(x) {
    return errorStructModule.getFunction(
	"moby-error-type:unknown-module?")(x);
}



// errorType -> boolean
var isApplicationArity = function(x) {
    return errorStructModule.getFunction(
	"moby-error-type:application-arity?")(x);
}


// errorType -> boolean
var isApplicationOperatorNotAFunction = function(x) {
    return errorStructModule.getFunction(
	"moby-error-type:application-operator-not-a-function?")(x);
}


// errorType -> boolean
var isTypeMismatch = function(x) {
    return errorStructModule.getFunction(
	"moby-error-type:type-mismatch?")(x);
}


// errorType -> boolean
var isIndexOutOfBounds = function(x) {
    return errorStructModule.getFunction(
	"moby-error-type:index-out-of-bounds?")(x);
}


// errorType -> boolean
var isConditionalExhausted = function(x) {
    return errorStructModule.getFunction(
	"moby-error-type:conditional-exhausted?")(x);
}

// errorType -> boolean
var isConditionalMissingQuestionAnswer = function(x) {
    return errorStructModule.getFunction(
	"moby-error-type:conditional-missing-question-answer")(x);
}


// errorType -> boolean
var isGenericRuntimeError = function(x) {
    return errorStructModule.getFunction(
	"moby-error-type:generic-runtime-error?")(x);
}

// errorType -> boolean
var isGenericSyntacticError = function(x) {
    return errorStructModule.getFunction(
	"moby-error-type:generic-syntactic-error?")(x);
}




//////////////////////////////////////////////////////////////////////


// Monkey patch an assertMobyRaise function.
JsUnitTest.Unit.Testcase.prototype.assertMobyRaise = function(predicate, thunk) {
    try {
	thunk();
	this.fail();
    } catch(e) {
	this.assert(isMobyError(e));
	this.assert(predicate(mobyErrorType(e)));
    }
};

