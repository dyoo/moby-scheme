#lang s-exp "../../../private/restricted-runtime-scheme.ss"

(require "arity-struct.ss")
(require "error-struct.ss")
(require "stx.ss")
(require "scheme-value-to-dom.ss")
(require "dom-helpers.ss")
(require "dom-parameters.ss")


;; Error structure to dom code.
;; These functions produce DOMs out of the values in error-struct,
;; ready to be styled.



;; error-struct-to-dom-sexp: dom (dom-parameters | false) -> sexp
;; Convert an error structure to a dom-sexp.  Optionally provide a dom-parameters
;; that defines custom dom converters.
(define (error-struct->dom-sexp an-error maybe-dom-parameters)
  (local [(define embedded-location (moby-error-location an-error))
          (define error-type (moby-error-error-type an-error))
          
          (define (add-toplevel-dom-error-wrapper a-dom)
            `(span ((class "Error"))
                   ,a-dom
                   (span ((class "Error.location"))
                         ,(Loc->dom-sexp embedded-location))))]
    
    (add-toplevel-dom-error-wrapper
     (cond
       [(moby-error-type:unclosed-lexical-token? error-type)
        `(span ((class "Error-UnclosedLexicalToken"))
               (span ((class "Error.reason"))
                     "I saw "
                     ,(scheme-value->dom-sexp (moby-error-type:unclosed-lexical-token-opener error-type)
                                              maybe-dom-parameters)
                     " to start a "
                     ,(moby-error-type:unclosed-lexical-token-type error-type)
                     ", but no "
                     ,(scheme-value->dom-sexp (moby-error-type:unclosed-lexical-token-closer error-type)
                                              maybe-dom-parameters)
                     " to close it.")
               
               (span ((class "Error-UnclosedLexicalToken.type")
                      (style "display:none"))
                     ,(moby-error-type:unclosed-lexical-token-type error-type))
               (span ((class "Error-UnclosedLexicalToken.opener")
                      (style "display:none"))
                     ,(symbol->string (moby-error-type:unclosed-lexical-token-opener error-type)))
               (span ((class "Error-UnclosedLexicalToken.closer")
                      (style "display:none"))
                     ,(symbol->string (moby-error-type:unclosed-lexical-token-closer error-type))))]
       
       
       [(moby-error-type:unrecognized-lexical-token? error-type)
        `(span ((class "Error-UnrecognizedLexicalToken"))
               (span ((class "Error.reason"))
                     "I saw "
                     ,(scheme-value->dom-sexp (moby-error-type:unrecognized-lexical-token-token error-type)
                                              maybe-dom-parameters)
                     " which I don't recognize as a program element.")
               (span ((class "Error-UnrecognizedLexicalToken.token")
                      (style "display:none"))
                     ,(symbol->string (moby-error-type:unrecognized-lexical-token-token error-type))))]
       
       [(moby-error-type:unsupported-lexical-token? error-type)
        `(span ((class "Error-UnsupportedLexicalToken"))
               (span ((class "Error.reason"))
                     ,(scheme-value->dom-sexp (moby-error-type:unsupported-lexical-token-token error-type)
                                              maybe-dom-parameters)
                     " is currently not supported.")
               (span ((class "Error-UnsupportedLexicalToken.token")
                      (style "display:none"))
                     ,(symbol->string (moby-error-type:unsupported-lexical-token-token error-type))))]
       
       [(moby-error-type:unsupported-expression-form? error-type)
        `(span ((class "Error-UnsupportedExpressionForm"))
               (span ((class "Error.reason"))
                     ,(stx->dom-sexp (moby-error-type:unsupported-expression-form-expr error-type)
                                       maybe-dom-parameters)
                     " is currently not supported.")
               (span ((class "Error-UnsupportedExpressionForm.expr")
                      (style "display:none"))
                     ,(stx->dom-sexp (moby-error-type:unsupported-expression-form-expr error-type)
                                       maybe-dom-parameters)))]
       
       [(moby-error-type:unclosed-parentheses? error-type)
        `(span ((class "Error-UnclosedParentheses"))
               (span ((class "Error.reason"))
                     "I saw "
                     ,(scheme-value->dom-sexp (moby-error-type:unclosed-parentheses-opener error-type)
                                              maybe-dom-parameters)
                     " to start an expression, but no "
                     ,(scheme-value->dom-sexp (moby-error-type:unclosed-parentheses-closer error-type)
                                              maybe-dom-parameters)
                     " to close it.")
               (span ((class "Error-UnclosedParentheses.opener")
                      (style "display:none"))
                     ,(symbol->string (moby-error-type:unclosed-parentheses-opener error-type)))
               (span ((class "Error-UnclosedParentheses.closer")
                      (style "display:none"))
                     ,(symbol->string (moby-error-type:unclosed-parentheses-closer error-type))))]
       
       [(moby-error-type:unbalanced-parentheses? error-type)
        `(span ((class "Error-UnbalancedParentheses"))
               "I saw "
               ,(scheme-value->dom-sexp 
                 (moby-error-type:unbalanced-parentheses-opener error-type)
                 maybe-dom-parameters)
               " earlier, and expected it to be matched with "

               ,(scheme-value->dom-sexp 
                 (moby-error-type:unbalanced-parentheses-closer error-type)
                 maybe-dom-parameters)

               ", but instead I see "
               
               ,(scheme-value->dom-sexp 
                 (moby-error-type:unbalanced-parentheses-observed error-type)
                 maybe-dom-parameters)

                 ".")]

        
       [(moby-error-type:syntax-not-applied? error-type)
        `(span ((class "Error-SyntaxNotApplied"))
               "I saw "
               ,(stx->dom-sexp (moby-error-type:syntax-not-applied-keyword error-type)
                               maybe-dom-parameters)
               ", which isn't supposed to be used as a bare expression.  "
               "Rather, one example of its use is: "
               ,(scheme-value->dom-sexp 
                 (moby-error-type:syntax-not-applied-example error-type)
                 maybe-dom-parameters)
               ".")]
       
        
       [(moby-error-type:closing-parenthesis-before-opener? error-type)
        `(span ((class "Error-ClosingParenthesisBeforeOpener"))
               "I saw "
               ,(scheme-value->dom-sexp 
                 (moby-error-type:closing-parenthesis-before-opener-closer error-type)
                 maybe-dom-parameters)
               " without it being paired with a left parenthesis.")]

       
       [(moby-error-type:duplicate-identifier? error-type)
        `(span ((class "Error-DuplicateIdentifier"))
               (span ((class "Error.reason"))
                     "The identifier "
                     ,(scheme-value->dom-sexp (moby-error-type:duplicate-identifier-id error-type)
                                              maybe-dom-parameters)
                     " has been duplicated.")
               (span ((class "Error-DuplicateIdentifier.secondLocation")
                      (style "display:none"))
                     ,(Loc->dom-sexp (moby-error-type:duplicate-identifier-second-location error-type))))]

       [(moby-error-type:expected-identifier? error-type)
        `(span ((class "Error-ExpectedIdentifier"))
               (span ((class "Error.reason"))
                     "I expected an identifier but received "
                     ,(stx->dom-sexp (moby-error-type:expected-identifier-observed error-type)
                                       maybe-dom-parameters)
                     " instead."))]
       
       [(moby-error-type:expected-list-of-identifiers? error-type)
        `(span ((class "Error-ExpectedListOfIdentifiers"))
               (span ((class "Error.reason"))
                     "Within " ,@(prepend-indefinite-article 
                                  (stx->dom-sexp 
                                   (moby-error-type:expected-list-of-identifiers-who error-type)
                                   maybe-dom-parameters))
                     ", I expected a list of identifiers but received "
                     ,(stx->dom-sexp (moby-error-type:expected-list-of-identifiers-observed error-type)
                                       maybe-dom-parameters)
                     " instead."))]
       
       [(moby-error-type:undefined-identifier? error-type)
        `(span ((class "Error-UndefinedIdentifier"))
               (span ((class "Error.reason"))
                     "I don't know what "
                     ,(scheme-value->dom-sexp (moby-error-type:undefined-identifier-id error-type)
                                              maybe-dom-parameters)
                     " is; it's not defined as an input or a primitive."))]

       
       [(moby-error-type:structure-identifier-not-expression? error-type)
        `(span ((class "Error-StructureIdentifierNotExpression"))
               (span ((class "Error.reason"))
                     "The structure name "
                     ,(scheme-value->dom-sexp (moby-error-type:structure-identifier-not-expression-id
                                                 error-type)
                                              maybe-dom-parameters)
                     " can't be used as an expression."))]

       
       [(moby-error-type:provided-name-not-defined? error-type)
        `(span ((class "Error-ProvidedNameNotDefined"))
               (span ((class "Error.reason"))
                     "The provided name "
                     ,(scheme-value->dom-sexp (moby-error-type:provided-name-not-defined-id error-type)
                                              maybe-dom-parameters)
                     " is not defined in the program."))]

       [(moby-error-type:redefinition-not-allowed? error-type)
        `(span ((class "Error-RedefinitionNotAllowed"))
               (span ((class "Error.reason"))
                     "The defined name "
                     ,(scheme-value->dom-sexp 
                       (moby-error-type:redefinition-not-allowed-id error-type)
                       maybe-dom-parameters)
                     " is being defined in the program, but it already has a definition that is not allowed to be redefined."))]

       [(moby-error-type:unknown-module? error-type)
        `(span ((class "Error-UnknownModule"))
               (span ((class "Error.reason"))
                     "I see a require of the module "
                     ,(scheme-value->dom-sexp (moby-error-type:unknown-module-path error-type)
                                              maybe-dom-parameters)
                     ", but I don't yet know what this module is."))]

       
              [(moby-error-type:unknown-module? error-type)
        `(span ((class "Error-UnknownModule"))
               (span ((class "Error.reason"))
                     "I see a require of the module "
                     ,(scheme-value->dom-sexp (moby-error-type:unknown-module-path error-type)
                                              maybe-dom-parameters)
                     ", but I don't yet know what this module is."))]

       
       [(moby-error-type:conditional-missing-question-answer? error-type)
        `(span ((class "Error-ConditionalMissingQuestionAnswer"))
               "After cond, I expect at least one question-answer branch, but I don't see anything.")]       
       
       [(moby-error-type:conditional-malformed-clause? error-type)
        `(span ((class "Error-ConditionalMalformedClause"))
               "Inside a cond branch, I expect a question and an answer, but I don't "
               "see both things here.")]
       
       [(moby-error-type:conditional-clause-too-few-elements? error-type)
        `(span ((class "Error-ConditionalClauseTooFewElements"))
               "Inside a cond branch, I expect a question and an answer, but I don't see both.")]

       [(moby-error-type:conditional-clause-too-many-elements? error-type)
        `(span ((class "Error-ConditionalClauseTooManyElements"))
               "Inside a cond branch, I expect to see a question and an answer, "
               "but I see more than two things here.")]
       
       [(moby-error-type:branch-value-not-boolean? error-type)
        `(span ((class "Error-BranchValueNotBoolean"))
               "I expected the question's value to be a boolean expression, "
               "(" 
               ,(scheme-value->dom-sexp true maybe-dom-parameters)
               " or " 
               ,(scheme-value->dom-sexp false maybe-dom-parameters)
               "), "
               "but instead I see "
               ,(scheme-value->dom-sexp 
                 (moby-error-type:branch-value-not-boolean-observed error-type)
                 maybe-dom-parameters)
               ".")]
       
       [(moby-error-type:if-too-few-elements? error-type)
        `(span ((class "Error-IfTooFewElements"))
               "I expected a test, a consequence, and an alternative, "
               "but I don't see all these three.")]
       
       [(moby-error-type:if-too-many-elements? error-type)
        `(span ((class "Error-IfTooFewElements"))
               "I expected only a test, a consequence, and an alternative, "
               "but I see more than three of these.")]
       
       [(moby-error-type:begin-body-empty? error-type)
        `(span ((class "Error-BeginBodyEmpty"))
               "Inside a begin, I expect to see a body, but I don't see anything.")]

       [(moby-error-type:boolean-chain-too-few-elements? error-type)
        `(span ((class "Error-BooleanChainTooFewElements"))
               "Inside a " 
               ,(scheme-value->dom-sexp (moby-error-type:boolean-chain-too-few-elements-id error-type)
                                        maybe-dom-parameters)
               ", I expect to see at least two expressions, but I don't see them both.")]

       [(moby-error-type:lambda-too-few-elements? error-type)
        `(span ((class "Error-LambdaTooFewElements"))
               "Inside a lambda, I expect to see a list of arguments and a single body, "
               "but I don't see both of these.")]
       
       [(moby-error-type:lambda-too-many-elements? error-type)
        `(span ((class "Error-LambdaTooManyElements"))
               "Inside a lambda, I expect to see a list of arguments and a single body, "
               "but I see more than these two.")]

       [(moby-error-type:missing-expression-following-quote? error-type)
        `(span ((class "Error-MissingExpressionFollowingQuote"))
               "After a " 
               ,(stx->dom-sexp (moby-error-type:missing-expression-following-quote-quote-stx error-type)
                               maybe-dom-parameters)
               ", I expected to see another expression immediately following it, but I don't see one.")]
       
       [(moby-error-type:quote-too-few-elements? error-type)
        `(span ((class "Error-QuoteTooFewElements"))
               "Inside a quote, I expect to see a single argument, but I don't see one.")]
       
       [(moby-error-type:quote-too-many-elements? error-type)
        `(span ((class "Error-QuoteTooManyElements"))
               "Inside a quote, I expect to single a single element, but I see more than one.")]

       [(moby-error-type:quasiquote-too-few-elements? error-type)
        `(span ((class "Error-QuasiquoteTooFewElements"))
               "Inside a quasiquote, I expect to see a single argument, but I don't see one.")]
       
       [(moby-error-type:quasiquote-too-many-elements? error-type)
        `(span ((class "Error-QuasiquoteTooManyElements"))
               "Inside a quasiquote, I expect to single a single element, but I see more than one.")]

       [(moby-error-type:unquote-too-few-elements? error-type)
        `(span ((class "Error-UnquoteTooFewElements"))
               "Inside an unquote, I expect to see a single argument, but I don't see one.")]
       
       [(moby-error-type:unquote-too-many-elements? error-type)
        `(span ((class "Error-UnquoteTooManyElements"))
               "Inside a unquote, I expect to single a single element, but I see more than one.")]

       [(moby-error-type:unquote-splicing-too-few-elements? error-type)
        `(span ((class "Error-UnquoteTooFewElements"))
               "Inside an unquote-splicing, I expect to see a single argument, but I don't see one.")]
       
       [(moby-error-type:unquote-splicing-too-many-elements? error-type)
        `(span ((class "Error-UnquoteTooManyElements"))
               "Inside a unquote-splicing, I expect to single a single element, but I see more than one.")]

       
       [(moby-error-type:when-no-body? error-type)
        `(span ((class "Error-WhenNoBody"))
               "Inside a " (scheme-value->dom-sexp 'when maybe-dom-parameters)
               ", I expect to see a body, but I don't see one.")]
       
       [(moby-error-type:unless-no-body? error-type)
        `(span ((class "Error-WhenNoBody"))
               "Inside an " (scheme-value->dom-sexp 'unless maybe-dom-parameters) 
               ", I expect to see a body, but I don't see one.")]

       
       [(moby-error-type:check-expect? error-type)
        `(span ((class "Error-CheckExpect"))
               "Inside a " 
               ,(scheme-value->dom-sexp 'check-expect maybe-dom-parameters)
               ", the observed value " 
               ,(scheme-value->dom-sexp (moby-error-type:check-expect-observed error-type) maybe-dom-parameters)
               " does not match the expected value " 
               ,(scheme-value->dom-sexp (moby-error-type:check-expect-expected error-type) maybe-dom-parameters)
               ".")]

       [(moby-error-type:check-within? error-type)
        `(span ((class "Error-CheckWithin"))
               "Inside a " 
               ,(scheme-value->dom-sexp 'check-within maybe-dom-parameters)
               ", the observed value " 
               ,(scheme-value->dom-sexp (moby-error-type:check-within-observed error-type) maybe-dom-parameters)
               " does not match the expected value " 
               ,(scheme-value->dom-sexp (moby-error-type:check-within-expected error-type) maybe-dom-parameters)
               " within the bounds "
               ,(scheme-value->dom-sexp (moby-error-type:check-within-within error-type) maybe-dom-parameters)
               ".")]
       
       [(moby-error-type:check-error? error-type)
        `(span ((class "Error-CheckError"))
               "Inside a " 
               ,(scheme-value->dom-sexp 'check-expect maybe-dom-parameters)
               ", the observed error " 
               ,(scheme-value->dom-sexp (moby-error-type:check-error-observed error-type) maybe-dom-parameters)
               " does not match the expected error " 
               ,(scheme-value->dom-sexp (moby-error-type:check-error-expected error-type) maybe-dom-parameters)
               ".")]
       
       [(moby-error-type:check-error-no-error? error-type)
        `(span ((class "Error-CheckErrorNoError"))
               "I expected an the expected error "
               ,(scheme-value->dom-sexp (moby-error-type:check-error-no-error-expected error-type) maybe-dom-parameters)
               " but instead I received the value "
               ,(scheme-value->dom-sexp (moby-error-type:check-error-no-error-observed error-type) maybe-dom-parameters))]
       
       [(moby-error-type:application-arity? error-type)
        `(span ((class "Error-ApplicationArity"))
               (span ((class "Error.reason"))
                     "The function "
                     ,(scheme-value->dom-sexp (moby-error-type:application-arity-who error-type) maybe-dom-parameters)
                     " expects "
                     ,(arity-to-dom-sexp (moby-error-type:application-arity-expected error-type))
                     " inputs, but instead I see "
                     ,(number->string (moby-error-type:application-arity-observed error-type))
                     " inputs."))]

       [(moby-error-type:application-operator-not-a-function? error-type)
        `(span ((class "Error-ApplicationOperatorNotAFunction"))
               (span ((class "Error.reason"))
                     "The operator "
                     ,(scheme-value->dom-sexp 
                       (moby-error-type:application-operator-not-a-function-who error-type)
                       maybe-dom-parameters)
                     " has a value "
                     ,(scheme-value->dom-sexp 
                       (moby-error-type:application-operator-not-a-function-val error-type)
                       maybe-dom-parameters)
                     ", but this value isn't a function."))]
       
       [(moby-error-type:type-mismatch? error-type)
        `(span ((class "Error-TypeMismatch"))
               (span ((class "Error.reason"))
                     "The function "
                     ,(scheme-value->dom-sexp 
                       (moby-error-type:type-mismatch-who error-type)
                       maybe-dom-parameters)
                     " expects "
                     ,@(prepend-indefinite-article   
                        (expected-value-to-dom-sexp
                         (moby-error-type:type-mismatch-expected error-type)))
                     " as its "
                     ,(number->string
                       (moby-error-type:type-mismatch-position error-type))
                     ,(ordinal-ending (moby-error-type:type-mismatch-position error-type))
                     " argument, but instead I see "
                     ,(scheme-value->dom-sexp 
                       (moby-error-type:type-mismatch-observed error-type)
                       maybe-dom-parameters)
                     "."))]

       [(moby-error-type:index-out-of-bounds? error-type)
        `(span ((class "Error-IndexOutOfBounds"))
               (span ((class "Error.reason"))
                     "The index "
                     ,(scheme-value->dom-sexp
                       (moby-error-type:index-out-of-bounds-observed error-type)
                       maybe-dom-parameters)
                     " is not within the expected boundary ["
                     ,(scheme-value->dom-sexp 
                       (moby-error-type:index-out-of-bounds-minimum error-type)
                       maybe-dom-parameters)
                     ", "
                     ,(scheme-value->dom-sexp 
                       (moby-error-type:index-out-of-bounds-maximum error-type)
                       maybe-dom-parameters)
                     "]"
                     ))]
       
       [(moby-error-type:conditional-exhausted? error-type)
        `(span ((class "Error-ConditionalExhausted"))
               (span ((class "Error.reason"))
                     "All of the questions inside a cond were false, "
                     "and at least one of them has to be true."))]
       
       [(moby-error-type:generic-runtime-error? error-type)
        `(span ((class "Error-GenericRuntimeError"))
               (span ((class "Error.reason"))
                     ,(moby-error-type:generic-runtime-error-reason error-type)))]

       [(moby-error-type:generic-syntactic-error? error-type)
        `(span ((class "Error-GenericSyntacticError"))
               (span ((class "Error.reason"))
                     ,(moby-error-type:generic-syntactic-error-reason error-type))
               (span ((class "Error-GenericSyntacticError.otherLocations"))
                     ,@(map Loc->dom-sexp 
                            (moby-error-type:generic-syntactic-error-other-locations error-type))))]
       
       [(moby-error-type:generic-read-error? error-type)
        `(span ((class "Error-GenericReadError"))
               (span ((class "Error.reason"))
                     ,(moby-error-type:generic-read-error-message error-type))
               (span ((class "Error-GenericReadError.locations"))
                     ,@(map Loc->dom-sexp
                            (moby-error-type:generic-read-error-locations error-type))))]))))
  


;; Loc->dom-sexp: loc -> sexp
;; Given a location, produce a dom representation of that location.
(define (Loc->dom-sexp a-loc)
  `(span ((class "location-reference")
          (style "display:none"))
         (span ((class "location-offset")) ,(number->string (Loc-offset a-loc)))
         (span ((class "location-line")) ,(number->string (Loc-line a-loc)))
         (span ((class "location-column")) ,(number->string (Loc-column a-loc)))
         (span ((class "location-span")) ,(number->string (Loc-span a-loc)))
         (span ((class "location-id")) ,(Loc-id a-loc))))
         

;; separate-with-br-elements: (listof dom) -> (listof dom)
;; Splice in br elements between each dom.
(define (separate-with-br-elements doms)
  (cond
    [(empty? doms)
     empty]
    [(empty? (rest doms))
     (list (first doms))]
    [else
     (cons (first doms)
           '(br ())
           (separate-with-br-elements (rest doms)))]))


;; ordinal-ending: natural-number -> string
;; Produces the ordinal ending of a number.  For example, 1 => st, 4 => th.
(define (ordinal-ending n)
  (cond
    [(= (modulo (quotient n 10) 10) 1)
     "th"]
    [else
     (list-ref '("th" "st" "nd" "rd" "th" 
                      "th" "th" "th" "th" "th")
               (modulo n 10))]))



;; prepend-indefinite-article: dom -> (listof dom)
;; Produces a list containting the appropriate indefinite article and the dom.
(define (prepend-indefinite-article a-dom)
  (list (indefinite-article (dom-string-content a-dom))
        " "
        a-dom))


;; indefinite-article: string -> string
;; Tries to get the indefinite article of a word.
(define (indefinite-article a-word)
  (cond
    [(begins-with-vowel-sound? a-word)
     "an"]
    [else 
     "a"]))


;; begins-with-vowel-sound?: string -> boolean
;; Tries to produces true if there's a vowel sound at the beginning of the
;; word.
;; This is not quite right because it doesn't use a dictionary.
(define (begins-with-vowel-sound? a-word)
  (cond
    [(= 0 (string-length a-word))
     false]
    [(vowel-character? (string-ref a-word 0))
     true]
    ;; Check to see if it's a "y" vowel sound
    [(and (> (string-length a-word) 2)
          (char=? (string-ref a-word 0) #\y)
          (not (vowel-character? (string-ref a-word 1))))
     true]
    [else
     false]))
  

;; vowel-character?: char -> boolean
;; Produces true if the given character is a vowel character.
(define (vowel-character? a-char)
  (member a-char '(#\a #\e #\i #\o #\u)))
    



;; stx-to-dom-sexp: stx -> dom
;; Converts a stx to a dom s-expression.
(define (stx->dom-sexp a-stx maybe-dom-parameters)
  (scheme-value->dom-sexp (stx->datum a-stx) maybe-dom-parameters))



;; expected-value-to-dom-sexp: moby-expected -> dom
;; Translates an expectation to a dom.
(define (expected-value-to-dom-sexp expected)
  (cond 
    [(moby-expected:string? expected)
     `(span ((class "Expected-String"))
            "string")]
    [(moby-expected:integer? expected)
     `(span ((class "Expected-Integer"))
            "integer")]
    [(moby-expected:natural? expected)
     `(span ((class "Expected-Natural"))
            "natural")]
    [(moby-expected:rational? expected)
     `(span ((class "Expected-Rational"))
            "rational")]
    [(moby-expected:real? expected)
     `(span ((class "Expected-Real"))
            "real")]
    [(moby-expected:complex? expected)
    `(span ((class "Expected-Complex"))
           "complex")]
    [(moby-expected:number? expected)
     `(span ((class "Expected-Number"))
            "number")]
    [(moby-expected:boolean? expected)
     `(span ((class "Expected-Boolean"))
            "boolean")]
    [(moby-expected:char? expected)
     `(span ((class "Expected-Char"))
            "char")]
    [(moby-expected:symbol? expected)
     `(span ((class "Expected-Symbol"))
            "symbol")]
    [(moby-expected:list? expected)
     `(span ((class "Expected-List"))
            "list")]
    [(moby-expected:listof? expected)
     `(span ((class "Expected-Listof"))
            "list of "
            (expected-value-to-dom-sexp (moby-expected:listof-thing expected))
            "" )]
    [(moby-expected:vector? expected)
     `(span ((class "Expected-Vector"))
            "vector")]
    [(moby-expected:struct? expected)
     `(span ((class "Expected-Struct"))
            "struct")]
    [(moby-expected:box? expected)
     `(span ((class "Expected-Box"))
            "box")]
    [(moby-expected:hash? expected)
     `(span ((class "Expected-Hash"))
            "hash")]
    [(moby-expected:function? expected)
     `(span ((class "Expected-Function"))
            "function")]
    [(moby-expected:something? expected)
     `(span ((class "Expected-Something"))
            ,(moby-expected:something-description expected))]))



;; Converts an arity to a dom sexpression.
(define (arity-to-dom-sexp an-arity)
  (cond
    [(arity:fixed? an-arity)
     `(span ((class "Arity-Fixed"))
            ,(number->string (arity:fixed-n an-arity)))]

    [(arity:variable? an-arity)
     `(span ((class "Arity-Variable"))
            (span ((class "Arity-Variable.minimum"))
                  ,(number->string (arity:variable-min an-arity)))
            (span ((class "Arity-Variable.maximum"))
                  ,(number->string (arity:variable-max an-arity))))]

    [(arity:mixed? an-arity)
     `(span ((class "Arity-Mixed"))
            ,@(map (lambda (a)
                     `(span ((class "Arity-Mixed.item"))
                            ,(arity-to-dom-sexp a)))))]))

    

(provide/contract 
 [error-struct->dom-sexp (any/c (or/c false/c dom-parameters?) . -> . any)])