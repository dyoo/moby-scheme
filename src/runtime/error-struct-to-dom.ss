#lang s-exp "private/restricted-runtime-scheme.ss"

(require "error-struct.ss")
(require "stx.ss")

;; Error structure to dom code.
;; These functions produce DOMs out of the values in error-struct,
;; ready to be styled.



;; error-struct-to-dom-sexp: dom -> sexp
(define (moby-error-struct-to-dom-sexp an-error)
  (local [(define embedded-reason (moby-error-reason an-error))
          (define embedded-location (moby-error-location an-error))
          (define error-type (moby-error-error-type an-error))]

    (cond
      [(moby-error-type:unclosed-lexical-token? error-type)
       `(span ((class "Error:UnclosedLexicalToken"))
              (span ((class "Error:UnclosedLexicalToken.reason"))
                    "I saw "
                    ,(symbol->string 
                      (moby-error-type:unclosed-lexical-token-opener error-type))
                    " to start a "
                    ,(moby-error-type:unclosed-lexical-token-type error-type)
                    ", but no "
                    (span ((class "MobyLexicalToken"))
                          ,(symbol->string 
                            (moby-error-type:unclosed-lexical-token-closer error-type)))
                    "to close it.")
              
              (span ((class "Error:UnclosedLexicalToken.type"))
                    ,(symbol->string (moby-error-type:unclosed-lexical-token-type error-type)))
              (span ((class "Error:UnclosedLexicalToken.opener"))
                    ,(symbol->string (moby-error-type:unclosed-lexical-token-opener error-type)))
              (span ((class "Error:UnclosedLexicalToken.closer"))
                    ,(symbol->string (moby-error-type:unclosed-lexical-token-closer error-type)))
              (span ((class "Error:UnclosedLexicalToken.location"))
                    ,(Loc->dom-sexp embedded-location)))]

      [(moby-error-type:unrecognized-lexical-token? error-type)
       `(span ((class "Error:UnrecognizedLexicalToken"))
              (span ((class "Error:UnrecognizedLexicalToken.reason"))
                    "I saw "
                    ,(symbol->string (moby-error-type:unrecognized-lexical-token-token error-type))
                    " which I don't recognize as a program element.")
              (span ((class "Error:UnrecognizedLexicalToken.token"))
                    (symbol->string (moby-error-type:unrecognized-lexical-token-token error-type)))
              (span ((class "Error:UnrecognizedLexicalToken.location"))
                    ,(Loc->dom-sexp embedded-location)))]
      
      [(moby-error-type:unsupported-lexical-token? error-type)
       `(span ((class "Error:UnsupportedLexicalToken"))
              (span ((class "Error:UnsupportedLexicalToken.reason"))
                    ,(symbol->string (moby-error-type:unsupported-lexical-token-token error-type))
                    " is currently not supported.")
              (span ((class "Error:UnsupportedLexicalToken.token"))
                    ,(symbol->string (moby-error-type:unsupported-lexical-token-token error-type)))
              (span ((class "Error:UnsupportedLexicalToken.location"))
                    ,(Loc->dom-sexp embedded-location)))]
      

      [(moby-error-type:unclosed-parentheses? error-type)
       `(span ((class "Error:UnclosedParentheses"))
              (span ((class "Error:UnclosedParentheses.reason"))
                    "I saw "
                    (symbol->string (moby-error-type:unclosed-parentheses error-type))
                    " to start an expression, but no "
                    (symbol->string (moby-error-type:unclosed-parentheses error-type))
                    "to close it.")
              (span ((class "Error:UnclosedParentheses.opener"))
                    (symbol->string (moby-error-type:unclosed-parentheses-opener error-type)))
              (span ((class "Error:UnclosedParentheses.closer"))
                    (symbol->string (moby-error-type:unclosed-parentheses-closer error-type)))
              (span ((class "Error:UnclosedParentheses.location"))
                    ,(Loc->dom-sexp embedded-location)))]
      
      [(moby-error-type:missing-expression? error-type)
       `(span ((class "Error:MissingExpression"))
              (span ((class "Error:MissingExpression.reason"))
                    "I expected an expression following "
                    ,(symbol->string (moby-error-type:missing-expression-token error-type))
                    " but did not find one.")
              (span ((class "Error:MissingExpression.location"))
                    ,(Loc->dom-sexp embedded-location)))]

      [(moby-error-type:duplicate-identifier? error-type)
       "fixme"]
      [(moby-error-type:undefined-identifier? error-type)
       "fixme"]
      [(moby-error-type:application-arity? error-type)
       "fixme"]
      [(moby-error-type:type-mismatch? error-type)
       "fixme"]
      [(moby-error-type:index-out-of-bounds? error-type)
       "fixme"]
      [(moby-error-type:conditional-exhausted? error-type)
       "fixme"]
      [(moby-error-type:generic-runtime-error? error-type)
       "fixme"]
      [(moby-error-type:generic-syntactic-error? error-type)
       "fixme"])))
  


;; Loc->dom-sexp: loc -> sexp
;; Given a location, produce a dom representation of that location.
(define (Loc->dom-sexp a-loc)
  `(span ((class "Location"))
         (span ((class "Location.offset")) ,(number->string (Loc-offset a-loc)))
         (span ((class "Location.line")) ,(number->string (Loc-line a-loc)))
         (span ((class "Location.column")) ,(number->string (Loc-column a-loc)))
         (span ((class "Location.span")) ,(number->string (Loc-span a-loc)))
         (span ((class "Location.id")) ,(Loc-id a-loc))))
         


(define (scheme-value-to-dom-sexp a-scheme-value)
  "fixme")
    
    
  

(provide moby-error-struct-to-dom-sexp)