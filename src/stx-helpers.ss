#lang scheme/base
(require "collects/moby/runtime/stx.ss")

(provide (all-defined-out))

;; syntax->stx: syntax -> stx
;; Go from Scheme's syntax objects to our own.
(define (syntax->stx a-syntax)
  (cond
    [(pair? (syntax-e a-syntax))
     (let ([elts
            (map syntax->stx (syntax->list a-syntax))])
       (datum->stx #f
                   elts
                   (make-Loc (syntax-position a-syntax)
                             (syntax-line a-syntax)
                             (syntax-column a-syntax)
                             (syntax-span a-syntax)
                             (format "~a" (syntax-source a-syntax)))))]
    [else
     (datum->stx #f 
                 (syntax-e a-syntax)
                 (make-Loc (syntax-position a-syntax)
                           (syntax-line a-syntax)
                           (syntax-column a-syntax)
                           (syntax-span a-syntax)
                           (format "~a" (syntax-source a-syntax))))]))