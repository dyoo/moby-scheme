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

var isMobyErrorType = function(x) {
    var result = errorStructModule.getFunction("moby-error-type?")(x);
    return result;
}

var mobyErrorType = function(x) {
    var result = errorStructModule.getFunction("moby-error-error-type")(x);
    return result;
}

var mobyErrorLoc = function(x) {
    var result = errorStructModule.getFunction("moby-error-location")(x);
    return result;
}


var extract = function(functionName) {
    return errorStructModule.getFunction(functionName);
} 






//////////////////////////////////////////////////////////////////////


var isUnclosedLexicalToken = 
    extract("moby-error-type:unclosed-lexical-token?");


var isUnrecognizedLexicalToken = 
    extract("moby-error-type:unrecognized-lexical-token?");
var isUnsupportedLexicalToken =
    extract("moby-error-type:unsupported-lexical-token?");
var isUnsupportedExpressionForm =
    extract("moby-error-type:unsupported-expression-form?");
var isUnclosedParentheses =
    extract("moby-error-type:unclosed-parentheses?");
var isClosingParenthesisBeforeOpener =
    extract("moby-error-type:closing-parenthesis-before-opener?");
var isUnbalancedParenthesis =
    extract("moby-error-type:unbalanced-parentheses?");
var isMissingExpression =
    extract("moby-error-type:missing-expression?");
var isDuplicateIdentifier = 
    extract("moby-error-type:duplicate-identifier?");
var isExpectedIdentifier =
    extract("moby-error-type:expected-identifier?");
var isUndefinedIdentifier =
    extract("moby-error-type:undefined-identifier?");
var isStructureIdentifierNotExpression = 
    extract("moby-error-type:structure-identifier-not-expression?");
var isProvidedNameNotDefined =
    extract("moby-error-type:provided-name-not-defined?");
var isProvidedStructureNotStructure =
    extract("moby-error-type:provided-structure-not-structure?");
var isUnknownModule =
    extract("moby-error-type:unknown-module?");
var isApplicationArity = 
    extract("moby-error-type:application-arity?");
var isApplicationOperatorNotAFunction =
    extract("moby-error-type:application-operator-not-a-function?");
var isTypeMismatch = 
    extract("moby-error-type:type-mismatch?");
var isIndexOutOfBounds = 
    extract("moby-error-type:index-out-of-bounds?");
var isConditionalExhausted = 
    extract("moby-error-type:conditional-exhausted?");
var isConditionalMissingQuestionAnswer = 
    extract("moby-error-type:conditional-missing-question-answer?");
var isConditionalMalformedClause = 
    extract("moby-error-type:conditional-malformed-clause?");
var isConditionalClauseTooFewElements = 
    extract("moby-error-type:conditional-clause-too-few-elements?");
var isConditionalClauseTooManyElements = 
    extract("moby-error-type:conditional-clause-too-many-elements?");
var isGenericRuntimeError = 
    extract("moby-error-type:generic-runtime-error?");
var isGenericSyntacticError = 
    extract("moby-error-type:generic-syntactic-error?");
var isBranchValueNotBoolean = 
    extract("moby-error-type:branch-value-not-boolean?");
var isIfTooFewElements = 
    extract("moby-error-type:if-too-few-elements?");
var isIfTooManyElements = 
    extract("moby-error-type:if-too-many-elements?");
var isBeginBodyEmpty = 
    extract("moby-error-type:begin-body-empty?");
var isBooleanChainTooFewElements = 
    extract("moby-error-type:boolean-chain-too-few-elements?");
var isExpectedListOfIdentifiers =
    extract("moby-error-type:expected-list-of-identifiers?");
var isLambdaTooFewElements =
    extract("moby-error-type:lambda-too-few-elements?");
var isLambdaTooManyElements =
    extract("moby-error-type:lambda-too-many-elements?");
var isQuoteTooFewElements = 
    extract("moby-error-type:quote-too-few-elements?");
var isQuoteTooManyElements = 
    extract("moby-error-type:quote-too-many-elements?");
var isQuasiquoteTooFewElements = 
    extract("moby-error-type:quasiquote-too-few-elements?");
var isQuasiquoteTooManyElements = 
    extract("moby-error-type:quasiquote-too-many-elements?");
var isUnquoteTooFewElements = 
    extract("moby-error-type:unquote-too-few-elements?");
var isUnquoteTooManyElements = 
    extract("moby-error-type:unquote-too-many-elements?");
var isUnquoteSplicingTooFewElements = 
    extract("moby-error-type:unquote-splicing-too-few-elements?");
var isUnquoteSplicingTooManyElements = 
    extract("moby-error-type:unquote-splicing-too-many-elements?");
var isCheckExpect = 
    extract("moby-error-type:check-expect?");
var isCheckWithin = 
    extract("moby-error-type:check-within?");
var isCheckError = 
    extract("moby-error-type:check-error?");
var isCheckErrorNoError = 
    extract("moby-error-type:check-error-no-error?");
var isSyntaxNotApplied = 
    extract("moby-error-type:syntax-not-applied?");




//////////////////////////////////////////////////////////////////////


// Monkey patch an assertMobyRaise function.
JsUnitTest.Unit.Testcase.prototype.assertMobyRaise = function(predicate, thunk) {
    try {
	thunk();
	this.fail("assertMobyRaise: expected exception hasn't been raised");
    } catch(e) {
	this.assert(isMobyError(e), "not a moby error");
	this.assert(isMobyErrorType(mobyErrorType(e)), "not a moby error type");
	this.assert(predicate(mobyErrorType(e)), "predicate fails");
    }
};

