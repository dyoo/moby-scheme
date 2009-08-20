#lang scheme/base

;; The restricted language we're writing our compiler in.
;; Part of the self-hosting.  The Javascript compiler will be written
;; in a small superset of intermediate-level scheme, so we can then
;; bootstrap things.

(require (prefix-in base: scheme/base)
         scheme/contract
         lang/htdp-intermediate-lambda
         (for-syntax scheme/base))

                     
(provide (except-out (all-from-out lang/htdp-intermediate-lambda)
                     define-struct
                     quote
                     let
                     letrec
                     let*))


(define-syntax (my-define-struct stx)
  (syntax-case stx ()
    [(_ id (fields ...))
     (syntax/loc stx 
       (base:define-struct id (fields ...) #:transparent))]))
    



;; The following primitives will need support in the runtime,
;; or be handled specially by the preprocessor.
(provide (rename-out (base:provide provide)
                     (base:quote quote)
                     (my-define-struct define-struct))

         
         ;; Contract-related stuff: the following will be erased on 
         ;; javascript bootstrapping time.
         provide/contract -> any/c listof list/c or/c false/c natural-number/c hash?

         
         ;; The rest of these primitives will be implemented for the kernel.
         ;; Hash stuff
         ;; FIXME: the hash in javascript only accepts strings as keys.
         ;; We should use contracts here.
         hash-set hash-ref hash-remove make-immutable-hasheq hash-map
         

         ;path->string normalize-path path? resolve-module-path build-path
         )