#lang scheme/base

;; The restricted language we're writing our compiler in.
;; Part of the self-hosting.  The Javascript compiler will be written
;; in a small superset of intermediate-level scheme, so we can then
;; bootstrap things.

(require (prefix-in base: scheme/base)
         scheme/path
         scheme/contract
         lang/htdp-intermediate-lambda
         syntax/modresolve)

                     
(provide (except-out (all-from-out lang/htdp-intermediate-lambda)
                     define-struct
                     quote
                     let
                     letrec
                     let*))



(define my-gensym
  (let ([n 0])
    (base:lambda (label)
                 (set! n (add1 n))
                 (string->symbol
                  (string-append (symbol->string label)
                      (number->string n))))))

;; The following primitives will need support in the runtime,
;; or be handled specially by the preprocessor.
(provide (rename-out (base:provide provide)
                     (base:quote quote)
                     (base:define-struct define-struct))

         
         ;; Contract-related stuff: the following will be erased on 
         ;; javascript bootstrapping time.
         provide/contract -> any/c listof list/c or/c false/c natural-number/c hash?

         
         ;; The rest of these primitives will be implemented for the kernel.
         ;; Hash stuff
         hash-set hash-ref make-immutable-hasheq hash-map
         (rename-out (my-gensym gensym))
         path->string normalize-path path? resolve-module-path build-path
         )