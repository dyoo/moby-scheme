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
       `(span ((class "Error:UnclosedLexicalTokenError"))
              (span ((class "Error:UnclosedLexicalTokenError.reason"))
                    "I saw "
                    " to start a string, but no "
                    "to close it"
                    )
              (span ((class "Error:UnclosedLexicalTokenError.location"))
                    
                    ,(Loc->dom-sexp embedded-location)))]
      [(moby-error-type:unrecognized-lexical-token? error-type)
       "fixme"]
      [(moby-error-type:unsupported-lexical-token? error-type)
       "fixme"]
      [(moby-error-type:unclosed-parentheses? error-type)
       "fixme"]
      [(moby-error-type:missing-expression? error-type)
       "fixme"]
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