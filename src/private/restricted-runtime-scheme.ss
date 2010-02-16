#lang scheme

(require (prefix-in base: scheme/base)
         (for-syntax scheme/base))

(provide (except-out (all-from-out scheme)
                     define-struct))


(define-syntax (my-define-struct stx)
  (syntax-case stx ()
    [(_ id (fields ...))
     (syntax/loc stx 
       (base:define-struct id (fields ...)
         #:prefab 
         #:mutable))]))


(define THE-UNDEFINED-VALUE
  (letrec ([x x])
    x))

(define (undefined? x)
  (eq? x THE-UNDEFINED-VALUE))


(provide (rename-out (my-define-struct define-struct))
         undefined?)