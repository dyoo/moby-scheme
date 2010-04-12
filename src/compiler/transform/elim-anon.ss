#lang s-exp "../lang.ss"

(require "anormal-frag-helpers.ss")
(require "munge-ids.ss")
(require "../../collects/moby/runtime/stx.ss")

;; format string to give names to formerly anonymous procedures
(define anon-prepend "anon~a")

;; fold-elim-anon-help: stx:list -> linfo
;; consumes a stx:list
;; returns the result of folding elim-anon-help over the elements of expr
(define (fold-elim-anon-help expr)
  (local [(define reversed-info
            (foldl (lambda (an-expr new-info)
                     (let ([rec-info (elim-anon-help an-expr)])
                       (make-linfo (cons (linfo-return rec-info)
                                         (linfo-return new-info))
                                   (append (linfo-raise new-info)
                                           (linfo-raise rec-info)))))
                   (make-linfo empty empty)
                   (stx-e expr)))]
    (make-linfo (datum->stx false
                            (reverse (linfo-return reversed-info))
                            (stx-loc expr))
                (linfo-raise reversed-info))))

;; elim-anon-help: stx -> linfo
;; consumes a syntax object with a valid expression inside
;; returns linfo where the return is the same statement
;;    but with all anonymous procedures named, and
;;    the raise is new local definitions to be placed inside the next binding form
(define (elim-anon-help expr)
  ;; if we have an atomic element, return it
  (if (stx:atom? expr)
      (make-linfo expr empty)
      ;; otherwise we have a list
      (let* ([expr-list (stx-e expr)]
             [first-elt (stx-e (first expr-list))])
        (cond
          
          ;; lambda expressions get lifted
          [(equal? first-elt 'lambda)
           (let ([new-proc-name (string->symbol (format anon-prepend (gensym)))]
                 [rec-info (elim-anon-help (third expr-list))])
             (make-linfo (datum->stx false new-proc-name (stx-loc expr))
                         (list
                          (datum->stx false
                                      (list 'define
                                            (cons new-proc-name
                                                  (stx-e (second expr-list)))
                                            (if (empty? (linfo-raise rec-info))
                                                (linfo-return rec-info)
                                                (list 'local
                                                      (linfo-raise rec-info)
                                                      (linfo-return rec-info))))
                                      (stx-loc expr)))))]
          
          ;; define is a binding form, so lift everything inside it
          ;; then put the new lifts in a local just inside (if they exist)
          ;; but make sure the definition uses syntactic sugar for procedure
          ;;    definitions so we don't unnecessarily name already-named procedures
          [(equal? first-elt 'define)
           (let* ([sugared-def (ensugar expr)]
                  [rec-info (elim-anon-help (third (stx-e sugared-def)))])
             (make-linfo (datum->stx false
                                     (list 'define
                                           (second (stx-e sugared-def))
                                           (if (empty? (linfo-raise rec-info))
                                               (linfo-return rec-info)
                                               (list 'local
                                                     (linfo-raise rec-info)
                                                     (linfo-return rec-info))))
                                     (stx-loc expr))
                         empty))]
          
          ;; local is another binding form
          ;; so recursively lift both the definitions and the body
          ;; then add the new bindings to the list of bindings on the local
          [(equal? first-elt 'local)
           (let ([new-defs (map elim-anon (stx-e (second expr-list)))]
                 [rec-info (elim-anon-help (third expr-list))])
             (make-linfo (datum->stx false
                                     (list 'local
                                           (append new-defs
                                                   (linfo-raise rec-info))
                                           (linfo-return rec-info))
                                     (stx-loc expr))
                         empty))]
          
          ;; if we have a quote or define-struct, we let it be
          [(or (equal? first-elt 'quote)
               (equal? first-elt 'define-struct)
               (equal? first-elt 'require))
           (make-linfo expr empty)]
          
          ;; each statement is its own self-contained thing in a begin
          ;; so just map elim-anon across them
          [(equal? first-elt 'begin)
           (make-linfo (datum->stx false
                                   (map elim-anon expr-list)
                                   (stx-loc expr))
                       empty)]
          
          ;; otherwise fold across each sub-expression
          [else (fold-elim-anon-help expr)]))))

;; elim-anon: stx -> stx
;; consumes an expression as a syntax object and names all anonymous procedures
(define (elim-anon expr)
  (let ([lifted (elim-anon-help expr)])
    (if (empty? (linfo-raise lifted))
        (linfo-return lifted)
        (datum->stx false
                    (list 'local
                          (linfo-raise lifted)
                          (linfo-return lifted))
                    (stx-loc expr)))))

;; name-anon-procs: stx -> stx
;; consumes a syntax object representing a program
;; produces a syntax object representing a semantically equivalent
;;    program, but with no anonymous procedures
;; NOTE: This procedure only exists to export a self-contained
;;       procedure that resets the state to insure it acts as a
;;       pure function. It is otherwise elim-anon
(define (name-anon-procs expr)
  (begin
    (reset-gensym)
    (elim-anon expr)))

;; lift-struct-defs: stx -> stx
;; consumes a syntax object representing a toplevel program
;; returns a symantically equivalent program with all local
;;    struct definitions raised to top leve
;; NOTE: This procedure munges identifiers to insure no collisions
;;       since it potentially increases the scope of different definitions
(define (lift-struct-defs expr)
  (let ([lifted (lift-struct-defs-help (munge-identifiers expr))])
    (datum->stx false
                (append (linfo-raise lifted)
                        (stx-e (linfo-return lifted)))
                (stx-loc expr))))

;; fold-lift-struct-defs-help: (list-of stx) -> linfo
;; consumes a list of syntax objects and folds lift-struct-defs-help over the list
;; producing linfo where the return is the lifted return and the raise
;;    is all the raises appended together
(define (fold-lift-struct-defs-help expr-list)
  (foldr (lambda (expr info)
           (let ([output (lift-struct-defs-help expr)])
             (make-linfo (cons (linfo-return output)
                               (linfo-return info))
                         (append (linfo-raise output)
                                 (linfo-raise info)))))
         (make-linfo empty empty)
         expr-list))

;; lift-struct-defs-help: stx -> linfo
;; consumes a syntax object representing an expression
;; produces an linfo where the return is a syntax object representing
;;    the same expression, but missing all local struct definitions
;;    and the raise is a list of removed struct definitions
(define (lift-struct-defs-help expr)
  ;; we have an atomic element, there cannot be any local struct definitions
  ;; so return the element
  (if (stx:atom? expr)
      (make-linfo expr empty)
      ;; otherwise we have a list
      (let* ([expr-list (stx-e expr)]
             [first-elt (stx-e (first expr-list))])
        (cond
          ;; if we have a local, we need to get the local struct definitions
          [(equal? first-elt 'local)
           (let ([struct-defs
                  (filter (lambda (a-def) (equal? (stx-e (first (stx-e a-def)))
                                                  'define-struct))
                          (stx-e (second expr-list)))]
                 [other-defs
                  (fold-lift-struct-defs-help
                   (filter (lambda (a-def) (equal? (stx-e (first (stx-e a-def)))
                                                   'define))
                           (stx-e (second expr-list))))])
             (make-linfo (datum->stx false
                                     (if (empty? (linfo-return other-defs))
                                         (third expr-list)
                                         (list 'local
                                               (linfo-return other-defs)
                                               (third expr-list)))
                                     (stx-loc expr))
                         (append struct-defs
                                 (linfo-raise other-defs))))]
          ;; if we see quote, define-struct (outside a local), or require
          ;; then don't touch anything
          [(or (equal? first-elt 'quote)
               (equal? first-elt 'define-struct)
               (equal? first-elt 'require))
           (make-linfo expr empty)]
          ;; otherwise recursively fold over each element
          [else (let ([folded-list (fold-lift-struct-defs-help expr-list)])
                  (make-linfo (datum->stx false
                                          (linfo-return folded-list)
                                          (stx-loc expr))
                              (linfo-raise folded-list)))]))))


(provide/contract
 [name-anon-procs (stx? . -> . stx?)]
 [lift-struct-defs (stx? . -> . stx?)])