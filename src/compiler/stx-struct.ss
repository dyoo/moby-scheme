#lang scheme/base

(require scheme/list
         scheme/bool
         scheme/contract)

;; Syntax objects
(define-struct stx:atom (datum loc) #:prefab)
(define-struct stx:list (elts loc) #:prefab)

(define-struct Loc (offset line column span id) #:prefab)



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


(provide/contract [struct stx:atom ([datum any/c]
                                    [loc any/c])]
                  [struct stx:list ([elts (listof stx?)]
                                    [loc any/c])]
                  [struct Loc ([offset number?]
                               [line number?]
                               [column number?]
                               [span number?]
                               [id string?])]
                  
                  [stx? (any/c . -> . boolean?)]
                  [stx-e (stx? . -> . any)]
                  [stx-loc (stx? . -> . any)])