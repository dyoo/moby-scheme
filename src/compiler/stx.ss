#lang scheme/base

(require "stx-struct.ss"
         scheme/list
         scheme/bool
         scheme/contract)

;; stx-begins-with?: stx symbol -> boolean
(define (stx-begins-with? a-stx a-sym)
  (cond
    [(stx:atom? a-stx)
     #f]
    [(stx:list? a-stx)
     (and (not (empty? (stx:list-elts a-stx)))
          (symbol? (stx-e (first (stx:list-elts a-stx))))
          (symbol=? (stx-e (first (stx:list-elts a-stx)))
                    a-sym))]))


;; datum->stx: any loc -> stx
;; Converts a datum into a syntax object deeply.  Every stx will
;; share the same loc value.  Pre-existing datum objects will
;; be left alone.
(define (datum->stx a-datum a-loc)
  (cond
    [(stx? a-datum)
     a-datum]
    [(or (pair? a-datum) (empty? a-datum))
     (make-stx:list (map (lambda (x) (datum->stx x a-loc)) a-datum)
                    a-loc)]
    [else
     (make-stx:atom a-datum a-loc)]))


;; stx->datum: stx -> any
;; Rip out the location information
(define (stx->datum a-stx)
  (cond
    [(stx:atom? a-stx)
     (stx:atom-datum a-stx)]
    [(stx:list? a-stx)
     (map stx->datum (stx:list-elts a-stx))]))



(provide (all-from-out "stx-struct.ss"))

(provide/contract                   
 [stx-begins-with? (stx? symbol? . -> . boolean?)]
 [datum->stx (any/c any/c . -> . stx?)]
 [stx->datum (stx? . -> . any)])
