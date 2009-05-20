#lang scheme/base

;; The restricted language we're writing our compiler in.
;; Part of the self-hosting.  The Javascript compiler will be written
;; in a small superset of intermediate-level scheme, so we can then
;; bootstrap things.

(require (prefix-in base: scheme/base)
         scheme/path
         scheme/contract
         lang/htdp-intermediate-lambda
         scheme/match)


(provide (except-out (all-from-out lang/htdp-intermediate-lambda)
                     require define-struct quote))



;; The following primitives will need support in the runtime,
;; or be handled specially by the preprocessor.
(provide (rename-out (base:provide provide)
                     (base:require require)
                     (base:define-struct define-struct)
                     (base:quote quote))
         provide/contract -> any/c listof or/c false/c natural-number/c
         hash-set hash-ref make-immutable-hasheq hash-map
         path->string normalize-path path?
         match
         )