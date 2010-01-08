#lang scheme/base
(require "compiler/stx.ss")

(provide (all-defined-out))

;; syntax->stx: syntax -> stx
;; Go from Scheme's syntax objects to our own.
(define (syntax->stx a-syntax)
  (cond
    [(pair? (syntax-e a-syntax))
     (let ([elts
            (map syntax->stx (syntax->list a-syntax))])
       (make-stx:list elts
                      (make-Loc (syntax-position a-syntax)
                                (syntax-line a-syntax)
                                (syntax-column a-syntax)
                                (syntax-span a-syntax)
                                (format "~a" (syntax-source a-syntax)))))]
    [else
     (make-stx:atom (syntax-e a-syntax)
                    (make-Loc (syntax-position a-syntax)
                              (syntax-line a-syntax)
                              (syntax-column a-syntax)
                              (syntax-span a-syntax)
                              (format "~a" (syntax-source a-syntax))))]))