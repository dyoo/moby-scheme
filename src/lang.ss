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


;; The following primitives will need support in the runtime,
;; or be handled specially by the preprocessor.
(provide (rename-out (base:provide provide)
                     (base:quote quote)
                     (base:define-struct define-struct))
         
         ;; Contract-related stuff
         provide/contract -> any/c listof or/c false/c natural-number/c

         ;; Hash stuff
         hash-set hash-ref make-immutable-hasheq make-immutable-hash hash-map hash?

         path->string normalize-path path? resolve-module-path build-path
         
         )