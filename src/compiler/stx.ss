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





(provide/contract [struct stx:atom ([datum any/c]
                                    [loc any/c])]
                  [struct stx:list ([elts (listof stx?)]
                                    [loc any/c])]
                  [stx? (any/c . -> . boolean?)]
                  [stx-e (stx? . -> . any)]
                  [stx-loc (stx? . -> . any)]
                  [stx-begins-with? (stx? . -> . boolean?)])