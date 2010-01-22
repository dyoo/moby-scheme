#lang s-exp "private/restricted-runtime-scheme.ss"

(require "error-struct.ss")

;; Error structure to dom stuff
                       

;; error-struct-to-dom: dom -> sexp
(define (moby-error-struct-to-dom an-error-struct)
  (cond
    [(moby-error-type:unclosed-lexical-token? an-error-struct)
     "fixme"]
    [(moby-error-type:unrecognized-lexical-token? an-error-struct)
     "fixme"]
    [(moby-error-type:unsupported-lexical-token? an-error-struct)
     "fixme"]
    [(moby-error-type:unclosed-parentheses? an-error-struct)
     "fixme"]
    [(moby-error-type:missing-expression? an-error-struct)
     "fixme"]
    [(moby-error-type:duplicate-identifier? an-error-struct)
     "fixme"]
    [(moby-error-type:undefined-identifier? an-error-struct)
     "fixme"]
    [(moby-error-type:application-arity? an-error-struct)
     "fixme"]
    [(moby-error-type:type-mismatch? an-error-struct)
     "fixme"]
    [(moby-error-type:index-out-of-bounds? an-error-struct)
     "fixme"]
    [(moby-error-type:conditional-exhausted? an-error-struct)
     "fixme"]
    [(moby-error-type:generic-runtime-error? an-error-struct)
     "fixme"]
    [(moby-error-type:generic-syntactic-error? an-error-struct)
     "fixme"]))
    
    
    
    
  

(provide moby-error-struct-to-dom)