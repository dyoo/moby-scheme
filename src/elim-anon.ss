#lang s-exp "lang.ss"

(require "lift-locals.ss")
(require "toplevel.ss")
(require "env.ss")

;(define def-prepend "o_")
;(define arg-prepend "a_")
;(define struct-prepend "s_")
(define anon-prepend "anon")

#|
(define empty-hash (make-immutable-hasheq empty))

(define-struct wrapped (expr))

;; mod-symbol: string symbol string -> symbol
;; consumes a prepend string, an original symbol, and an append string
;; returns a new symbol with the prepend string prepended and the append string appended
(define (mod-symbol prepend symb append)
  (string->symbol (string-append prepend (symbol->string symb) append)))

;; unwrap: s-expr -> s-expr
;; consumes a symbolic expression
;; returns the same expression, unwrapping any wrapped statements
(define (unwrap expr)
  (cond
    [(wrapped? expr) (wrapped-expr expr)]
    [(cons? expr) (map unwrap expr)]
    [else expr]))

;; get-struct-names: (listof expr) -> (listof symbol)
;; takes a top-level list of expression
;; returns a list of the name of all structs defined at top level
(define (get-struct-ids expr)
  (foldl (lambda (an-expr symb-list)
           (if (and (cons? an-expr)
                    (equal? (first an-expr) 'define-struct))
               (cons (second an-expr) symb-list)
               symb-list))
         empty
         expr))

;; struct-replace: symbol (listof symbol) -> symbol/false
;; takes a symbol and a list of struct names
;; returns the original symbol with the struct name prepened
;;   or false if the struct name did not appear in the list
(define (struct-replace? symb prepend struct-names)
  (cond
    [(empty? struct-names) false]
    [(cons? struct-names)
     (cond
       [(equal? symb (first struct-names))
        (make-wrapped (mod-symbol prepend symb ""))]
       [(equal? symb (mod-symbol "make-" (first struct-names) ""))
        (make-wrapped (mod-symbol (string-append "make-" prepend)
                                (first struct-names)
                                ""))]
       [(equal? symb (mod-symbol "" (first struct-names) "?"))
        (make-wrapped (mod-symbol prepend (first struct-names) "?"))]
       [(and (> (string-length (symbol->string symb))
                (string-length (symbol->string (first struct-names))))
             (equal? (string->symbol
                 (substring (symbol->string symb)
                            0
                            (string-length (symbol->string (first struct-names)))))
                (first struct-names)))
        (make-wrapped (mod-symbol prepend symb ""))]
       [else (struct-replace? symb prepend (rest struct-names))])]))

;; replace-struct-ids: s-expr (listof symbol) -> s-expr
;; consumes a symbolic expression and a list of identifiers to replace
;; returns the same expression with the struct identifiers replaced
(define (replace-struct-ids expr struct-names)
  (cond
    [(cons? expr)
     (local [(define new-names (append struct-names
                                       (if (equal? (first expr) 'local)
                                           (get-struct-ids (second expr))
                                           empty)))]
       (map (lambda (elt) (replace-struct-ids elt new-names)) expr))]
    [(symbol? expr)
     (local [(define name-replace (struct-replace? expr struct-prepend struct-names))]
       (if (false? name-replace)
           expr
           name-replace))]
    [else expr]))

;; rename-toplevel-structs: (listof s-expr) -> s-expr
;; consumes a top-level list of expression
;; returns the same list by with all struct names munged
(define (rename-structs expr)
  (replace-struct-ids expr (get-struct-ids expr)))

;; add-id-pairs: string (listof symbol) hash -> hash
;; consumes a prepend string and a list of symbols
;; returns a hash table mapping each symbol in the list
;;    to the same symbol prepended with the string
(define (add-id-pairs prepend id-list base-hash)
  (foldl (lambda (id a-hash)
           (hash-set a-hash id (make-wrapped (mod-symbol prepend id ""))))
         base-hash
         id-list))

;; get-outter-ids: (listof s-expr) -> (listof symbol)
;; consumes a program represented as a list of symbolic expressions in abstract syntax
;; returns the list of all identifiers bound at the outter level of the expression
(define (get-outter-ids expr)
  (foldl (lambda (an-expr symb-list)
           (if (and (cons? an-expr)
                    (equal? (first an-expr) 'define))
               (cons (if (cons? (second an-expr))
                         (first (second an-expr))
                         (second an-expr))
                     symb-list)
               symb-list))
         empty
         expr))

;; replace-user-ids: s-expr (hashof symbol . wrapped) -> s-expr
;; consumes a symbolic expression and a hash of things to replace
;; returns the same expression with all user-defined identifiers munged
;;    by prepending orig-prepend
(define (replace-user-ids expr id-hash)
  (cond
    [(symbol? expr) (if (false? (hash-ref id-hash expr false))
                        expr
                        (hash-ref id-hash expr false))]
    [(cons? expr)
     (cond
       [(equal? (first expr) 'local)
        (local [(define new-id-hash
                  (add-id-pairs def-prepend
                                (get-outter-ids (second expr))
                                id-hash))]
          (map (lambda (elt) (replace-user-ids elt new-id-hash)) expr))]
       [(or (equal? (first expr) 'define)
            (equal? (first expr) 'lambda))
        (local [(define new-id-hash
                  (add-id-pairs arg-prepend
                                (if (equal? (first expr) 'lambda)
                                    (second expr)
                                    (if (cons? (second expr))
                                        (rest (second expr))
                                        empty))
                                id-hash))]
          (map (lambda (elt) (replace-user-ids elt new-id-hash)) expr))]
       [(equal? (first expr) 'quote) expr]
       [else (map (lambda (elt) (replace-user-ids elt id-hash)) expr)])]
    [else expr]))

;; replace-ids: s-expr -> s-expr
;; munges all indentifiers in the initial expression
(define (rename-all-ids expr)
  (unwrap (replace-user-ids (replace-struct-ids expr (get-struct-ids expr))
                            (add-id-pairs def-prepend (get-outter-ids expr)
                                          empty-hash))))
|#
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-struct linfo (return raise gensym))

;; fold-elim-anon: s-expr (listof symbol) (hashof symbol . wrapped) number -> linfo
;; consumes a symbolic expression, a list of formal arguments,
;;    a hash table of replacements, and a gensym counter
;; returns the result of folding elim-anon-help across the expression
(define (fold-elim-anon expr gensym)
  (foldr (lambda (an-expr new-info)
           (local [(define rec-info
                     (elim-anon-help an-expr (linfo-gensym new-info)))]
             (make-linfo (cons (linfo-return rec-info)
                               (linfo-return new-info))
                         (append (linfo-raise rec-info)
                                 (linfo-raise new-info))
                         (linfo-gensym rec-info))))
         (make-linfo empty empty gensym)
         expr))

;; elim-anon-help: s-expr number -> s-expr
(define (elim-anon-help expr gensym)
  (cond
    [(cons? expr)
     (cond
       [(equal? (first expr) 'lambda)
        (local [(define new-proc-name
                  (string->symbol (string-append anon-prepend
                                                 (number->string gensym))))
                (define rec-info (elim-anon-help (third expr) (add1 gensym)))]
        ;(begin
          #;(printf "lambda expression becoming ~a is\n ~a\n"
                  new-proc-name
                  expr)
          (make-linfo new-proc-name
                      (cons (list 'define
                                  (cons new-proc-name (second expr))
                                  (linfo-return rec-info))
                            (linfo-raise rec-info))
                      (linfo-gensym rec-info)))];)]
       [(equal? (first expr) 'define)
        (local [(define rec-info (if (cons? (third expr))
                                     (elim-anon-help (third expr) gensym)
                                     (make-linfo (third expr) empty gensym)))]
          (make-linfo (list 'define
                            (second expr)
                            (if (empty? (linfo-raise rec-info))
                                (linfo-return rec-info)
                                (list 'local
                                      (linfo-raise rec-info)
                                      (linfo-return rec-info))))
                      empty
                      (linfo-gensym rec-info)))]
       [(equal? (first expr) 'local)
        (local [(define new-defs (fold-elim-anon (second expr) gensym))
                (define rec-info (elim-anon-help (third expr) (linfo-gensym new-defs)))]
          ;(begin
            #;(when (not (empty? (linfo-raise new-defs)))
              (printf "local defines returned raise:\n ~a\n"
                      (linfo-raise new-defs)))
          (make-linfo (list 'local
                            (append (linfo-return new-defs)
                                    (linfo-raise rec-info))
                            (linfo-return rec-info))
                      empty
                      (linfo-gensym rec-info)))];)]
       [(equal? (first expr) 'quote) (make-linfo expr empty gensym)]
       [else (fold-elim-anon expr gensym)])]
    [else (make-linfo expr empty gensym)]))

;; elim-anon: s-expr -> s-expr
(define (elim-anon expr)
  (linfo-return (elim-anon-help expr 0)))

(define (ready-anormalize expr)
  (elim-anon (lift-program expr)))

(provide ready-anormalize)

