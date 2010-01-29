var errorStructModule = 
    plt.Kernel.invokeModule("moby/runtime/error-struct");

var errorStructToDomModule = 
    plt.Kernel.invokeModule("moby/runtime/error-struct-to-dom");


// Returns true if the thing is a moby error
var isMobyError = function(x) {
    return errorStructModule.EXPORTS.moby_dash_error_question_(x);
}
var mobyErrorType = function(x) {
    return errorStructModule.EXPORTS.moby_dash_error_dash_error_dash_type(x);
}


//////////////////////////////////////////////////////////////////////


// errorType -> boolean
var isUnclosedLexicalToken = function(x) {
    return errorStructModule.EXPORTS.moby_dash_error_dash_type_colon_unclosed_dash_lexical_dash_token_question_(x);
}

// errorType -> boolean
var isUnrecognizedLexicalToken = function(x) {
    return errorStructModule.EXPORTS.moby_dash_error_dash_type_colon_unrecognized_dash_lexical_dash_token_question_(x);
}


// errorType -> boolean
var isUnsupportedLexicalToken = function(x) {
    return errorStructModule.EXPORTS.moby_dash_error_dash_type_colon_unsupported_dash_lexical_dash_token_question_(x);
}


// errorType -> boolean
var isUnsupportedExpressionForm = function(x) {
    return errorStructModule.EXPORTS.moby_dash_error_dash_type_colon_unsupported_dash_expression_dash_form_question_(x);
}

// errorType -> boolean
var isUnclosedParentheses = function(x) {
    return errorStructModule.EXPORTS.moby_dash_error_dash_type_colon_unclosed_dash_parentheses_question_(x);
}

// errorType -> boolean
var isMissingExpression = function(x) {
    return errorStructModule.EXPORTS.moby_dash_error_dash_type_colon_missing_dash_expression_question_(x);
}

// errorType -> boolean
var isDuplicateIdentifier = function(x) {
    return errorStructModule.EXPORTS.moby_dash_error_dash_type_colon_duplicate_dash_identifier_question_(x);
}

// errorType -> boolean
var isExpectedIdentifier = function(x) {
    return errorStructModule.EXPORTS.moby_dash_error_dash_type_colon_expected_dash_identifier_question_(x);
}

// errorType -> boolean
var isUndefinedIdentifier = function(x) {
    return errorStructModule.EXPORTS.moby_dash_error_dash_type_colon_undefined_dash_identifier_question_(x);
}

// errorType -> boolean
var isStructureIdentifierNotExpression = function(x) {
    return errorStructModule.EXPORTS.moby_dash_error_dash_type_colon_structure_dash_identifier_dash_not_dash_expression_question_(x);
}

// errorType -> boolean
var isProvidedNameNotDefined = function(x) {
    return errorStructModule.EXPORTS.moby_dash_error_dash_type_colon_provided_dash_name_dash_not_dash_defined_question_(x);
}

// errorType -> boolean
var isProvidedStructureNotStructure = function(x) {
    return errorStructModule.EXPORTS.moby_dash_error_dash_type_colon_provided_dash_structure_dash_not_dash_structure_question_(x);
}

// errorType -> boolean
var isUnknownModule = function(x) {
    return errorStructModule.EXPORTS.moby_dash_error_dash_type_colon_unknown_dash_module_question_(x);
}



// errorType -> boolean
var isApplicationArity = function(x) {
    return errorStructModule.EXPORTS.moby_dash_error_dash_type_colon_application_dash_arity_question_(x);
}


// errorType -> boolean
var isApplicationOperatorNotAFunction = function(x) {
    return errorStructModule.EXPORTS.moby_dash_error_dash_type_colon_application_dash_operator_dash_not_dash_a_dash_function_question_(x);
}


// errorType -> boolean
var isTypeMismatch = function(x) {
    return errorStructModule.EXPORTS.moby_dash_error_dash_type_colon_type_dash_mismatch_question_(x);
}


// errorType -> boolean
var isIndexOutOfBounds = function(x) {
    return errorStructModule.EXPORTS.moby_dash_error_dash_type_colon_index_dash_out_dash_of_dash_bounds_question_(x);
}


// errorType -> boolean
var isConditionalExhausted = function(x) {
    return errorStructModule.EXPORTS.moby_dash_error_dash_type_colon_conditional_dash_exhausted_question_(x);
}


// errorType -> boolean
var isGenericRuntimeError = function(x) {
    return errorStructModule.EXPORTS.moby_dash_error_dash_type_colon_generic_dash_runtime_dash_error_question_(x);
}

// errorType -> boolean
var isGenericSyntacticError = function(x) {
    return errorStructModule.EXPORTS.moby_dash_error_dash_type_colon_generic_dash_syntactic_dash_error_question_(x);
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

