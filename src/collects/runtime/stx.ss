#lang s-exp "private/restricted-runtime-scheme.ss"


;; Syntax objects
(define-struct stx:atom (datum loc binding))
(define-struct stx:list (elts loc binding))

(define-struct Loc (offset line column span id))



;; stx?: any -> boolean
(define (stx? x)
  (or (stx:atom? x)
      (stx:list? x)))


;; stx-e: stx -> any
;; Shallow unwrap of the element out of the syntax.
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
          (symbol? (stx-e (first (stx:list-elts a-stx))))
          (symbol=? (stx-e (first (stx:list-elts a-stx)))
                    a-sym))]))


;; datum->stx: any loc -> stx
;; Converts a datum into a syntax object deeply.  Every stx will
;; share the same loc value.  Pre-existing datum objects will
;; be left alone.
(define (datum->stx context a-datum a-loc)
  (cond
    [(stx? a-datum)
     a-datum]
    [(or (pair? a-datum) (empty? a-datum))
     (make-stx:list (map (lambda (x) (datum->stx context x a-loc)) a-datum)
                    a-loc
                    context)]
    [else
     (make-stx:atom a-datum a-loc context)]))


;; stx->datum: stx -> any
;; Rip out the location information
(define (stx->datum a-stx)
  (cond
    [(stx:atom? a-stx)
     (stx:atom-datum a-stx)]
    [(stx:list? a-stx)
     (map stx->datum (stx:list-elts a-stx))]))




(provide/contract #;[struct stx:atom ([datum any/c]
                                      [loc any/c]
                                      [binding (or/c false? binding?)])]
                  #;[struct stx:list ([elts (listof stx?)]
                                      [loc any/c]
                                      [binding (or/c false? binding?)])]
                  
                  [stx:atom? (any/c . -> . boolean?)]
                  [stx:list? (any/c . -> . boolean?)]
                  
                  [struct Loc ([offset number?]
                               [line number?]
                               [column number?]
                               [span number?]
                               [id string?])]
                  
                  [stx? (any/c . -> . boolean?)]
                  [stx-e (stx? . -> . any)]
                  [stx-loc (stx? . -> . any)]
                  
                  [stx-begins-with? (stx? symbol? . -> . boolean?)]
                  [datum->stx ((or/c false? syntax?) any/c Loc? . -> . stx?)]
                  [stx->datum (stx? . -> . any)])