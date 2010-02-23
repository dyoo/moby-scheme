#lang s-exp "../lang.ss"


(require "anormal-frag-helpers.ss")
(require "../../collects/moby/runtime/stx.ss")
(require "box-local-defs.ss")
(require "../toplevel.ss")
(require "../env.ss")

;; string with which to name temporary variables
(define temp-begin "temp~a")

;; procedures that we will not consider primitive because they can potentially
;; call arguments that might need a continuation
(define higher-order-prims '(andmap argmax argmin build-list build-string compose
                                    filter foldl foldr map memf ormap quicksort sort))
;; first-order "primitives" not included in toplevel.ss
(define other-prims '(quote set!))

;; stateful hash of primitives and reset procedure
(define prims (make-hash))
(define (reset-prims prim-hash) (set! prims prim-hash))

;; get-struct-defs: (listof s-expr) -> (listof s-expr)
;; takes a list of toplevel statements (a program)
;; returns all struct definitions appearing at toplevel
(define (get-struct-defs program)
  (filter (lambda (statement) (and (cons? statement)
                                   (equal? (first statement) 'define-struct)))
          program))

;; generate-prims: (listof s-expr) symbol -> (hash-of symbol . boolean)
;; consumes a list of toplevel statements (a program)
;; returns an environment containing all first-order primitives for that program
;;    these are the predefined first-order primitives and struct primitives
(define (generate-prims program language)
  (let* ([prim-hash (make-hash)]
         [add-key (lambda (key) (if (member key higher-order-prims)
                                    (void)
                                    (hash-set! prim-hash key #t)))])
    (begin
      (for-each add-key other-prims)
      (for-each add-key (env-keys (get-toplevel-env language)))
      (map (lambda (struct-def) (for-each add-key (get-struct-procs struct-def)))
           (get-struct-defs program))
      prim-hash)))

;; primitive-expr?: stx -> boolean
;; consumes a syntax object
;; returns true if the object represents a primitive expression, false otherwise
;;    a primitive expression is either any atomic expression
;;    or a procedure application where the procedure is a first-order primitive
(define (primitive-expr? expr)
  (or (stx:atom? expr)
      (hash-ref prims (stx-e (first (stx-e expr))) #f)))

;; gen-temp-symbol: number -> symbol
;; takes a gensym counter and returns a symbol for temporary binding using that gensym
(define (gen-temp-symbol num)
  (string->symbol (format temp-begin num)))

;; fold-anormal-help: (list-of stx) -> linfo
;; consumes a list of syntax objects
;; folds anormal-help across the list, returning linfo where the return
;;    is the list of returns from each call to anormal-help
;;    and the raise is the concatination of the raise lists
;; NOTE: there are some odd-looking calls to reverse that allow for a
;;       foldl so the gensym numbers appear in a sensable order for testing
(define (fold-anormal-help expr)
  (let ([reversed-result
         (foldl (lambda (an-expr rest-info)
                  (let ([rec-info (anormal-help an-expr)])
                    (make-linfo (cons (linfo-return rec-info)
                                      (linfo-return rest-info))
                                (append (reverse (linfo-raise rec-info))
                                        (linfo-raise rest-info)))))
                (make-linfo empty empty)
                expr)])
    (make-linfo (reverse (linfo-return reversed-result))
                (reverse (linfo-raise reversed-result)))))

;; anormal-help: stx -> linfo
;; consumes a syntax object representing an expression
;; produces a symantically equivalent expression as linfo
;;    where the return is the final return statement
;;    and the raise is the other local bindings created for a-normalization
(define (anormal-help expr)
  ;; if we have an atomic element there's nothing to a-normalize
  (if (stx:atom? expr)
      (make-linfo expr empty)
      ;; otherwise we have a list
      (let* ([expr-list (stx-e expr)]
             [first-elt (stx-e (first expr-list))])
        (cond
          
          ;; if we have a define statement, then a-normalize the body
          ;; and put any raised elements in a local inside the define
          [(equal? first-elt 'define)
           (let ([body-info (anormal-help (third expr-list))])
             (make-linfo (datum->stx false
                                     (list (first expr-list)
                                           (second expr-list)
                                           (if (empty? (linfo-raise body-info))
                                               (linfo-return body-info)
                                               (list 'local
                                                     (linfo-raise body-info)
                                                     (linfo-return body-info))))
                                     (stx-loc expr))
                         empty))]
          
          ;; if we have a local statement then first
          ;; do a self-contained a-normalize on each definition
          ;; then a-normalize the body and append any new definitions
          ;;    to the old list
          [(equal? first-elt 'local)
           (let ([defs (map make-anormal (stx-e (second expr-list)))]
                 [body-info (anormal-help (third expr-list))])
             (make-linfo (datum->stx false
                                     (list (first expr-list)
                                           (append defs
                                                   (linfo-raise body-info))
                                           (linfo-return body-info))
                                     (stx-loc expr))
                         empty))]
          
          ;; for if statements we a-normalize (and pass up the raise)
          ;;    on the condition and do a self-contained a-normalize
          ;;    on both the then and else clauses to make sure nothing
          ;;    is evaluated when it shouldn't be
          [(equal? first-elt 'if)
           (let ([condition (anormal-help (second expr-list))]
                 [then-clause (make-anormal (third expr-list))]
                 [else-clause (make-anormal (fourth expr-list))])
             (if (primitive-expr? (linfo-return condition))
                 (make-linfo (datum->stx false
                                         (list 'if
                                               (linfo-return condition)
                                               then-clause
                                               else-clause)
                                         (stx-loc expr))
                             (linfo-raise condition))
                 (let ([temp-symbol (gen-temp-symbol (gensym))])
                   (make-linfo (datum->stx false
                                           (list 'if
                                                 temp-symbol
                                                 then-clause
                                                 else-clause)
                                           (stx-loc expr))
                               (append (linfo-raise condition)
                                       (list (datum->stx
                                              false
                                              (list 'define
                                                    temp-symbol
                                                    (linfo-return condition))
                                              (stx-loc (second expr-list)))))))))]
          
          ;; with and, or, and begin it is easiest to just
          ;; do a self-contained a-normalize on each sub-expression
          [(or (equal? first-elt 'and)
               (equal? first-elt 'or)
               (equal? first-elt 'begin))
           (make-linfo (datum->stx false
                                   (map make-anormal expr-list)
                                   (stx-loc expr))
                       empty)]
          ;; for quote, define-struct, and require, we don't want to touch anything
          [(or (equal? first-elt 'quote)
               (equal? first-elt 'define-struct)
               (equal? first-elt 'require))
           (make-linfo expr empty)]
          
          ;; for any other expression, a-normalize each sub-expression
          ;; then fold across the result, checking if each expression is primitive
          ;; if it is, then leave it, otherwise create a new define binding it
          ;;    to a temporary variable and add that to the list of raises
          [else
           (let* ([arg-info (fold-anormal-help expr-list)]
                  [anormal-expr
                   (foldl (lambda (an-expr rest-args)
                            (if (primitive-expr? an-expr)
                                (make-linfo (cons an-expr (linfo-return rest-args))
                                            (linfo-raise rest-args))
                                (let ([temp-symbol (gen-temp-symbol (gensym))])
                                  (make-linfo (cons (datum->stx false
                                                                temp-symbol
                                                                (stx-loc an-expr))
                                                    (linfo-return rest-args))
                                              (cons (datum->stx false
                                                                (list 'define
                                                                      temp-symbol
                                                                      an-expr)
                                                                (stx-loc an-expr))
                                                    (linfo-raise rest-args))))))
                          (make-linfo empty empty)
                          (linfo-return arg-info))])
             (make-linfo (datum->stx false
                                     (reverse (linfo-return anormal-expr))
                                     (stx-loc expr))
                         (append (linfo-raise arg-info)
                                 (reverse (linfo-raise anormal-expr)))))]))))


;; make-anroaml: stx -> stx
;; consumes a syntax object representing a single stand-alone expression
;; produces a semantically equivalent expression in a-normal form
(define (make-anormal expr)
  (if (stx:atom? expr)
      expr
      (let ([linfo-out (anormal-help expr)])
        (if (empty? (linfo-raise linfo-out))
            (linfo-return linfo-out)
            (datum->stx false
                        (list 'local
                              (linfo-raise linfo-out)
                              (linfo-return linfo-out))
                        (stx-loc expr))))))

;; anormalize: stx:list -> stx:list
;; consumes a syntax object representing a program
;; produces a semantically equivalent program in a-normal form
;; NOTE: this is what is exported, so it resets all stateful fields
;;       and generates the list of primitives to insure it acts
;;       as a pure function
(define (anormalize program)
  (let ([readied (ready-anormalize program)])
    (begin
      (reset-gensym)
      (reset-prims (generate-prims (stx->datum readied) 'language-here))
      (datum->stx false
                  (map make-anormal (stx-e readied))
                  (stx-loc readied)))))
  
(provide/contract
 [anormalize (stx:list? . -> . stx:list?)])
