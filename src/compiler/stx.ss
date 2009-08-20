#lang s-exp "lang.ss"

;; Syntax objects
(define-struct stx:atom (datum loc))
(define-struct stx:list (elts loc))


;; stx?: any -> boolean
(define (stx? x)
  (or (stx:atom? x)
      (stx:list? x)))


;; stx-e: stx -> any
;; Shallow unwrap of the element.
(define (stx-e a-stx)
  (cond
    [(stx:atom? a-stx)
     (stx:atom-datum a-stx)]
    [(stx:list? a-stx)
     (stx:list-elts a-stx)]))


;; stx-loc: stx -> loc
(define (stx-loc a-stx)
  (cond
    [(stx:atom? a-stx)
     (stx:atom-loc a-stx)]
    [(stx:list? a-stx)
     (stx:list-loc a-stx)]))


;; stx-begins-with?: stx symbol -> boolean
(define (stx-begins-with? a-stx a-sym)
  (cond
    [(stx:atom? a-stx)
     #f]
    [(stx:list? a-stx)
     (and (not (empty? (stx:list-elts a-stx)))
          (symbol? (first (stx:list-elts a-stx)))
          (symbol=? (first (stx:list-elts a-stx))
                    a-sym))]))


;; datum->stx: any loc -> stx
;; Converts a datum into a syntax object deeply.  Every stx will
;; share the same loc value.
(define (datum->stx a-datum a-loc)
  (cond
    [(or (pair? a-datum) (empty? a-datum))
     (make-stx:list (map (lambda (x) (datum->stx x a-loc)) a-datum)
                    a-loc)]
    [else
     (make-stx:atom a-datum a-loc)]))



(provide/contract [struct stx:atom ([datum any/c]
                                    [loc any/c])]
                  [struct stx:list ([elts (listof stx?)]
                                    [loc any/c])]
                  [stx? (any/c . -> . boolean?)]
                  [stx-e (stx? . -> . any)]
                  [stx-loc (stx? . -> . any)]
                  [stx-begins-with? (stx? symbol? . -> . boolean?)])