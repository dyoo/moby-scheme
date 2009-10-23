#lang scheme/base

;; The restricted language we're writing our compiler in.
;; Part of the self-hosting.  The Javascript compiler will be written
;; in a small superset of intermediate-level scheme, so we can then
;; bootstrap things.

(require (prefix-in base: scheme/base)
         scheme/contract
         lang/htdp-advanced
         (for-syntax scheme/base)
         "error-struct.ss")

                     
(provide (except-out (all-from-out lang/htdp-advanced)
                     define-struct
                     define
                     quote
                     quasiquote
                     unquote
                     unquote-splicing
                     let
                     letrec
                     let*
                     image?
                     image=?
                     set!
                     not))


(define-syntax (my-define-struct stx)
  (syntax-case stx ()
    [(_ id (fields ...))
     (syntax/loc stx 
       (base:define-struct id (fields ...)
                           #:prefab 
                           #:mutable))]))
    

(base:define (syntax-error msg . stx)
  (raise (make-exn:fail:moby-syntax-error msg (current-continuation-marks) stx)))


;; The following primitives will need support in the runtime,
;; or be handled specially by the preprocessor.
(provide (rename-out (base:provide provide)
                     (base:quote quote)
                     (base:quasiquote quasiquote)
                     (base:unquote unquote)
                     (base:unquote-splicing unquote-splicing)
                     (my-define-struct define-struct)
                     (base:define define)
                     (base:set! set!)
                     (base:not not))

         
         ;; Contract-related stuff: the following will be erased on 
         ;; javascript bootstrapping time.
         provide/contract -> any/c listof list/c or/c false/c natural-number/c hash?

         begin
         void
         
         build-vector
         make-vector
         vector
         vector-length
         vector-ref
         vector-set!
         vector?
         
         case
         
         let let* letrec
         
         ;; The rest of these primitives will be implemented for the kernel.
         ;; Hash stuff
         ;; FIXME: the hash in javascript only accepts strings as keys.
         ;; We should use contracts here.
         hash-set hash-ref hash-remove make-immutable-hasheq hash-map
         

	 syntax-error

         ;path->string normalize-path path? resolve-module-path build-path
         )