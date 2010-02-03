#lang scheme/base

(require (prefix-in base: scheme/base)
         (for-syntax scheme/base)
         scheme/contract)

(provide (except-out (all-from-out scheme/base)
                     define-struct)
         (all-from-out scheme/contract))


(define-syntax (my-define-struct stx)
  (syntax-case stx ()
    [(_ id (fields ...))
     (syntax/loc stx 
       (base:define-struct id (fields ...)
         #:prefab 
         #:mutable))]))


(provide (rename-out (my-define-struct define-struct)))