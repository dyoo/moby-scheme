#lang s-exp "../lang.ss"

(require "anormal-frag-helpers.ss")
(require "elim-anon.ss")
(require "../../collects/moby/runtime/stx.ss")

;; unbox-ids: stx (list-of symbol) -> stx
;; consumes a symbolic expression and a list of identifiers to unbox
;; returns the symbolic expression with each instance of the identifier wrapped in an unbox
(define (unbox-ids expr ids)
    (let ([contents (stx-e expr)])
      (cond
        [(symbol? contents) (if (member contents ids)
                            (datum->stx false
                                        (list 'unbox expr)
                                        (stx-loc expr))
                            expr)]
        [(cons? contents) (datum->stx false
                                      (map (lambda (an-expr) (unbox-ids an-expr ids))
                                           contents)
                                      (stx-loc expr))]
        [else expr])))
  
;; box-locals: stx -> stx
;; consumes a syntax object
;; returns a symantically equivalent expression with local value definitions
;;    created using set-box!
(define (box-locals expr)
  ;; atomic expressions cannot contain locals, so return expr
  (if (stx:atom? expr)
      expr
      ;; otherwise we have a list
      (let* ([expr-list (stx-e expr)]
             [first-elt (stx-e (first expr-list))])
        (cond
          ;; if we have a local statement
          ;; separate the definitions into explicit lambda expressions
          ;;    and everything else
          ;; change the "everything else" to be defined as (box 'undefined)
          ;;    and set the value using set-box! inside a begin immediately
          ;;    after the definition list
          ;; NOTE: the box of undefined and set-box! works for explicit
          ;;       procedures, but it is not necessary since nothing is
          ;;       evaluated during the definition, so the definitions
          ;;       can all be in the same fragment later
          [(equal? first-elt 'local)
           (let* ([sugared-defs (map ensugar (stx-e (second expr-list)))]
                  [old-val-defs (filter (lambda (a-def)
                                          (stx:atom? (second (stx-e a-def))))
                                        sugared-defs)]
                  [val-ids (map (lambda (an-expr) (stx-e (second (stx-e an-expr))))
                                old-val-defs)]
                  [boxed-val-defs (map box-locals old-val-defs)]
                  [old-fun-defs (filter (lambda (a-def)
                                          (stx:list? (second (stx-e a-def))))
                                        sugared-defs)]
                  [boxed-fun-defs
                   (unbox-ids (datum->stx false
                                          (map box-locals old-fun-defs)
                                          (stx-loc (second expr-list)))
                              val-ids)])
             (datum->stx
              false
              (list 'local
                    ;; definitions list is old procedure definitions
                    ;; followed by boxes of undefineds for the value definitions
                    (datum->stx false
                                (append (stx-e boxed-fun-defs)
                                        (map (lambda (symb)
                                               `(define ,symb (box 'undefined)))
                                             val-ids))
                                (stx-loc (second expr-list)))
                    ;; then use being and set-box! to set the values of the
                    ;; undefined definitions (not needed if none were there)
                    (if (empty? boxed-val-defs)
                        (box-locals (third expr-list))
                        (cons 'begin
                              (foldr (lambda (a-def rest-expr)
                                       (cons (list 'set-box!
                                                   (second (stx-e a-def))
                                                   (unbox-ids (third (stx-e a-def))
                                                              val-ids))
                                             rest-expr))
                                     (list (unbox-ids (box-locals (third expr-list))
                                                      val-ids))
                                     boxed-val-defs))))
              (stx-loc expr)))]
          ;; if we have a quote, define-struct, or require statement, leave it alone
          [(or (equal? first-elt 'quote)
               (equal? first-elt 'define-struct)
               (equal? first-elt 'require))
           expr]
          ;; otherwise map a recursive call across each sub-expression
          [else (datum->stx false
                            (map box-locals expr-list)
                            (stx-loc expr))]))))

;; ready-anormalize: stx -> stx
;; consumes a syntax object representing a toplevel program
;; produces a semantically equivalent program that, once a-normalized
;;    will be ready for fragmentation (identifiers munged, local struct
;;    definitions lifted to top level, no anonymous procedures, and
;;    local value definitions made using boxes and set-box!)
(define (ready-anormalize expr)
  (box-locals (datum->stx false
                          (map name-anon-procs (stx-e (lift-struct-defs expr)))
                          (stx-loc expr))))


(provide/contract
 [ready-anormalize (stx:list? . -> . stx:list?)])
