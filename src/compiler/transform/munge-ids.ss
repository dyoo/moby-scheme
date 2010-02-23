#lang s-exp "../lang.ss"


(require "anormal-frag-helpers.ss")
(require "../rbtree.ss")
(require "../../collects/moby/runtime/stx.ss")

;; format strings to modify symbols
;; to insure unique identifiers later
(define def-prepend "d~a_~a")
(define arg-prepend "a_~a")
(define struct-prepend "s~a_~a")

;; symbol<: symbol symbol -> boolean
(define (symbol< x y)
  (string<? (symbol->string x)
            (symbol->string y)))

;; get-id-tree: s-expr (tree-of symbol . symbol) -> (tree-of symbol . symbol)
;; consumes a symbolic expression and a tree of replacements
;; returns a tree containing all bindings of the old tree
;;    as well as new bindings representing replacements for anything
;;    defined at toplevel relative to the inputted s-expr
(define (get-id-tree expr base-tree)
  (if (not (cons? expr))
      base-tree
      (cond
        [(equal? (first expr) 'define-struct)
         (let* ([orig-procs (get-struct-procs expr)]
                [new-name (string->symbol (format struct-prepend
                                                  (gensym)
                                                  (second expr)))]
                [new-procs (get-struct-procs (list 'define-struct
                                                   new-name
                                                   (third expr)))])
           (foldl (lambda (old-proc new-proc a-tree)
                    (rbtree-insert symbol< a-tree old-proc new-proc))
                  (rbtree-insert symbol< base-tree (second expr) new-name)
                  orig-procs
                  new-procs))]
        [(equal? (first expr) 'define)
         (let ([name (if (cons? (second expr))
                         (first (second expr))
                         (second expr))])
           (rbtree-insert symbol< base-tree name
                          (string->symbol (format def-prepend (gensym) name))))]
        [else base-tree])))

;; replace-ids: stx (tree-of symbol . symbol) -> stx
;; consumes a syntax object and a tree of replacements
;; returns a new syntax object with all identifiers munged
;;    if they are in the replacement tree or if they are bound
;;    inside the current expression
(define (replace-ids expr replacements)
  (cond
    ;; if we have an identifier we might need to replace it
    [(symbol? (stx-e expr)) (if (false? (rbtree-lookup symbol<
                                                       replacements
                                                       (stx-e expr)))
                                expr
                                (datum->stx false
                                            (second (rbtree-lookup symbol<
                                                                   replacements
                                                                   (stx-e expr)))
                                            (stx-loc expr)))]
    ;; if we have a list, cheeck the first element of the list
    [(stx:list? expr)
     (let* ([expr-list (stx-e expr)]
            [first-elt (stx-e (first expr-list))])
       (cond
         ;; on a define or lambda, get the arguments
         ;; and recursively replace body elements
         [(or (equal? first-elt 'define)
              (equal? first-elt 'lambda))
          (let* ([new-args (if (equal? first-elt 'lambda)
                               (stx->datum (second expr-list))
                               (if (stx:list? (second expr-list))
                                   (rest (stx->datum (second expr-list)))
                                   empty))]
                 [new-replacements (foldl (lambda (symb a-tree)
                                            (rbtree-insert symbol<
                                                           a-tree
                                                           symb
                                                           (string->symbol
                                                            (format arg-prepend
                                                                    symb))))
                                          replacements
                                          new-args)])
            (datum->stx false
                        (list (first expr-list)
                              (replace-ids (second expr-list) new-replacements)
                              (replace-ids (third expr-list) new-replacements))
                        (stx-loc expr)))]
         ;; with local, get the bindings from the defines
         ;; recursively replace the defines and the body
         ;; using those new replacements
         [(equal? first-elt 'local)
          (let ([new-replacements (foldl get-id-tree
                                         replacements
                                         (stx->datum (second expr-list)))])
            (datum->stx false
                        (list (first expr-list)
                              (replace-ids (second expr-list) new-replacements)
                              (replace-ids (third expr-list) new-replacements))
                        (stx-loc expr)))]
         ;; if define-struct, we only want to munge the struct name
         [(equal? first-elt 'define-struct)
          (datum->stx false
                      (list (first expr-list)
                            (replace-ids (second expr-list) replacements)
                            (third expr-list))
                      (stx-loc expr))]
         ;; if quote or require, leave it alone
         [(or (equal? first-elt 'quote)
              (equal? first-elt 'require))
          expr]
         ;; otherwise map over the entire expression
         [else (datum->stx false
                           (map (lambda (an-expr)
                                  (replace-ids an-expr replacements))
                                expr-list)
                           (stx-loc expr))]))]
    ;; if neither an identifier nor a cons, do nothing
    [else expr]))

;; munge-identifiers: stx:list -> stx:list
;; consumes a program as a syntax object
;; returns the same program with all identifiers bound in that program munged
;;    using a gensym counter
;; NOTE: since the gensym counter is stateful, this resets it,
;;       insuring it acts as a pure function
(define (munge-identifiers expr)
  (begin
    (reset-gensym)
    (replace-ids expr (foldl get-id-tree empty-rbtree (stx->datum expr)))))


(provide/contract [munge-identifiers (stx:list? . -> . stx:list?)])
