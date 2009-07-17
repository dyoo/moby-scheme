#lang scheme

(define global-prepend "g_")
(define struct-prepend "s_")
(define arg-prepend "a_")
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

;; contains?: datum (listof datum) -> boolean
;; consumes a datum and a list
;; returns true if the list contains the datum and false otherwise
(define (contains? dat alod)
  (not (false? (member dat alod))))

;; make-id-pairs: string (listof symbol) -> hash
;; consumes a prepend string and a list of symbols
;; returns a hash table mapping each symbol in the list
;;    to the same symbol prepended with the string
(define (make-id-pairs prepend id-list)
  (foldl (lambda (id a-hash)
           (hash-set a-hash id (make-wrapped (mod-symbol prepend id ""))))
         empty-hash
         id-list))

;; get-struct-names: (listof expr) -> (listof symbol)
;; takes a top-level list of expression
;; returns a list of the name of all structs defined at top level
(define (get-struct-names expr)
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
(define (replace-struct-ids expr prepend struct-names)
  (cond
    [(cons? expr)
     (if (equal? (first expr) 'local)
         (local [(define new-names (get-struct-names (second expr)))
                 (define pruned-names (filter (lambda (elt)
                                                (not (contains? elt new-names)))
                                              struct-names))]
           (map (lambda (elt) (replace-struct-ids elt prepend pruned-names)) expr))
         (map (lambda (elt) (replace-struct-ids elt prepend struct-names)) expr))
     #;(local [(define filtered-names
               (if (equal? (first expr) 'define-struct)
                   (filter (lambda (elt) (not (equal? elt (second expr)))) struct-names)
                   struct-names))]
       (map (lambda (elt) (replace-struct-ids elt prepend filtered-names)) expr))]
    [(symbol? expr)
     (local [(define name-replace (struct-replace? expr prepend struct-names))]
       (if (false? name-replace)
           expr
           name-replace))]
    [else expr]))

;; rename-toplevel-structs: (listof s-expr) -> s-expr
;; consumes a top-level list of expression
;; returns the same list by with all struct names munged
(define (rename-toplevel-structs expr)
  (replace-struct-ids expr struct-prepend (get-struct-names expr)))

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
#;(define (get-outter-ids expr)
  (cond
    [(empty? expr) empty]
    [(cons? expr)
     (local [(define this-expr (first expr))]
       (if (or (not (cons? this-expr))
               (and (cons? this-expr)
                    (not (equal? (first this-expr) 'define))))
           (get-outter-ids (rest expr))
           (cons (if (cons? (second this-expr))
                     (first (second this-expr))
                     (second this-expr))
                 (get-outter-ids (rest expr)))))]))

;; replace-ids: s-expr (hashof symbol . wrapped) -> s-expr
;; consumes a program in abstract syntax and a hash table mapping identifiers to
;;     wrapped expression with which to replace the identifier
;; returns the same program with the specified identifiers replaced
;;     except where they are locally defined
(define (replace-ids expr id-hash)
  (cond
    [(cons? expr)
     (local [(define sub-expr (first expr))]
       (cond
         
         [(or (equal? sub-expr 'define)
              (equal? sub-expr 'lambda))
          (local [(define new-args (if (equal? sub-expr 'define)
                                       (if (cons? (second expr))
                                           (rest (second expr))
                                           empty)
                                       (second expr)))
                  (define new-hash
                    (foldl (lambda (id a-hash)
                             (if (wrapped? id)
                                 a-hash
                                 (hash-set a-hash
                                           id
                                           (make-wrapped
                                            (mod-symbol arg-prepend id "")))))
                           id-hash
                           new-args))]
          (map (lambda (an-expr) (replace-ids an-expr new-hash)) expr))]
         
         [(equal? sub-expr 'local)
          (local [(define pruned-hash
                    (foldl (lambda (id a-hash)
                             (hash-set a-hash id false))
                           id-hash
                           (get-outter-ids (second expr))))]
            (map (lambda (an-expr)
                   (replace-ids an-expr pruned-hash)) expr))]
         
         #;[(equal? sub-expr 'lambda)
          (local [(define new-hash
                    (foldl (lambda (id a-hash)
                             (hash-set a-hash
                                       id
                                       (make-wrapped (mod-symbol arg-prepend id ""))))
                           id-hash
                           (second expr)))]
            (map (lambda (an-expr) (replace-ids an-expr new-hash)) expr))]
         
         [(equal? sub-expr 'quote) expr]
         
         [else (map (lambda (an-expr) (replace-ids an-expr id-hash)) expr)]))]
    [(symbol? expr) (if (not (false? (hash-ref id-hash expr false)))
                        (hash-ref id-hash expr false)
                        expr)]
    [else expr]))

;; rename-top-level: s-expr -> s-expr
;; consumes a list of statements of scheme source
;; outputs the same list, but prepends all top-level identifiers with "g_"
(define (rename-top-level expr)
  (replace-ids expr (make-id-pairs global-prepend (get-outter-ids expr))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-struct linfo (return toplevel gensym))
(define-struct gensym-hold (gensym dat))
(define-struct temp-set (orig temp final))

;; get-junk-def: s-expr -> s-expr
;; consumes a define statement in abstract syntax
;; returns a statement defining the munged identifier to be 'undefined
(define (get-junk-def def-expr gensym)
  (list 'define
        (mod-symbol (string-append "l" (number->string gensym) "_")
                    (if (cons? (second def-expr))
                        (first (second def-expr))
                        (second def-expr))
                    "")
        ''undefined))


;; fold-expr: s-expr (listof symbol) (hashof symbol . wrapped) number -> linfo
;; consumes a symbolic expression, a list of formal arguments,
;;    a hash table of replacements, and a gensym counter
;; returns the result of folding lift-local across the expression
(define (fold-expr expr args replacements gensym)
  (foldr (lambda (an-expr new-info)
           (local [(define rec-info
;                     (begin
;                       (printf "calling lift-info on \n ~a \n ~a \n ~a \n ~a \n"
;                               (unwrap an-expr) (unwrap args) replacements gensym)
                     (lift-local an-expr args replacements (linfo-gensym new-info)))];)]
;             (begin
;               (printf "rec-info got back\n ~a\n" (unwrap (linfo-return rec-info)))
             (make-linfo (cons (linfo-return rec-info)
                               (linfo-return new-info))
                         (append (linfo-toplevel rec-info)
                                 (linfo-toplevel new-info))
                         (linfo-gensym rec-info))));)
         (make-linfo empty empty gensym)
         expr))

;; desugar: s-expr -> s-expr
;; takes a define statement in abstract syntax
;; returns a symantically equivalent statement without function definition sugar
(define (desugar def)
  (if (and (cons? def)
           (equal? (first def) 'define))
      (if (cons? (second def))
          (list 'define
                (first (second def))
                (list* 'lambda
                       (rest (second def))
                       (rest (rest def))))
          def)
      (error 'desugar "expected definition in abstract syntax, found something else.")))

;; lift-local: s-expr (listof symbol) (hashof symbol . wrapped) number -> linfo
;; consumes a symbolic expression, a list of higher-up arguments, a hashtable mapping
;;    symbols to wrapped expressions to replace them with, and a gensym counter
;; returns linfo where return is the expression with local defines lifted out,
;;    toplevel is the new top level definitions, and gensym is the new gensym counter
(define (lift-local expr args replacements gensym)
  (cond
    [(symbol? expr) (make-linfo (if (false? (hash-ref replacements expr false))
                                    expr
                                    (hash-ref replacements expr false))
                                empty
                                gensym)]
    [(cons? expr)
     (local [(define sub-expr (first expr))]
       (cond
         [(equal? sub-expr 'local)
          (local [(define local-struct-prepend
                    (string-append "s" (number->string gensym) "_"))
                  (define struct-defs (filter (lambda (elt) (equal? (first elt)
                                                                    'define-struct))
                                              (second expr)))
                  (define struct-names (get-struct-names (second expr)))
                  (define reg-defs (filter (lambda (elt) (not (equal? (first elt)
                                                                      'define-struct)))
                                           (second expr)))
                  (define old-local-ids (map (lambda (elt) (if (cons? (second elt))
                                                               (first (second elt))
                                                               (second elt)))
                                             reg-defs))
                  (define junk-defs
                    (foldl (lambda (an-expr def-list)
                             (make-gensym-hold (add1 (gensym-hold-gensym def-list))
                                               (cons (get-junk-def
                                                      an-expr
                                                      (gensym-hold-gensym def-list))
                                                     (gensym-hold-dat def-list))))
                           (make-gensym-hold gensym empty)
                           reg-defs))
                  (define gensym-defs
                    (foldl (lambda (def rest-info)
                             (local [(define rec-info
                                       (fold-expr def
                                                  args
                                                  replacements
                                                  (linfo-gensym rest-info)))]
                               (make-linfo (cons (linfo-return rec-info)
                                                 (linfo-return rest-info))
                                           (append (linfo-toplevel rec-info)
                                                   (linfo-toplevel rest-info))
                                           (linfo-gensym rec-info))))
                           (make-linfo empty empty (gensym-hold-gensym junk-defs))
                           reg-defs))
                    #;(foldl (lambda (an-expr other-defs)
                             (local [(define rec-info
                                       (lift-local an-expr
                                                   args
                                                   replacements
                                                   (gensym-hold-gensym other-defs)))]
                               (make-gensym-hold (add1 (linfo-gensym rec-info))
                                                 (cons (cons (get-junk-def
                                                              (linfo-return rec-info))
                                                             (linfo-toplevel rec-info))
                                                       (gensym-hold-dat other-defs)))))
                           (make-gensym-hold (gensym-hold-gensym junk-defs) empty)
                           reg-defs);)
                  (define new-replacements
                    (local
                      [(define-struct temp-pair (id val))
                       (define new-replace-pairs
                         (map (lambda (an-id a-def)
                                (make-temp-pair an-id (make-wrapped (second a-def))))
                              old-local-ids
                              (reverse (gensym-hold-dat junk-defs))))]
                      (foldl (lambda (a-pair a-hash)
                               (hash-set a-hash
                                         (temp-pair-id a-pair)
                                         (temp-pair-val a-pair)))
                             replacements
                             new-replace-pairs)))
                  (define new-toplevel-defs
                    (append (map (lambda (struct-def)
                                   (list 'define-struct
                                         (make-wrapped (mod-symbol local-struct-prepend
                                                                   (second struct-def)
                                                                   ""))
                                         (third struct-def)))
                                 struct-defs)
                            #;(replace-struct-ids struct-defs
                                                struct-names
                                                local-struct-prepend)
                            (gensym-hold-dat junk-defs)
                            (linfo-toplevel gensym-defs)))
                  (define new-gensym (if (empty? struct-names)
                                         (linfo-gensym gensym-defs)
                                         (add1 (linfo-gensym gensym-defs))))]
            (local [(define lifted-body
                      (lift-local (third expr) args new-replacements new-gensym))]
;              (begin
                #;(printf "unlifted local body:\n ~a\nlifted local body:\n ~a\n\n"
                        (unwrap (third expr))
                        (unwrap (linfo-return lifted-body)))
                #;(when (not (empty? struct-names))
                  (printf "struct-names is\n ~a\nstruct-defs is\n ~a\nbody is\n ~a\n"
                          struct-names
                          struct-defs
                          (unwrap (linfo-return lifted-body))))
              (if (empty? reg-defs)
                  lifted-body
                  (make-linfo
                   (replace-struct-ids
                    (append (cons 'begin
                                  (replace-ids (map (lambda (def)
                                                      (cons 'set! (rest (desugar def))))
                                                    (reverse (linfo-return gensym-defs)))
                                               new-replacements))
                            (list (linfo-return lifted-body)))
                    local-struct-prepend
                    struct-names)
                   (append (linfo-toplevel lifted-body)
                           new-toplevel-defs)
                   (linfo-gensym lifted-body)))))];)]
                  
         [(or (equal? sub-expr 'define)
              (equal? sub-expr 'lambda))
          (local [(define new-args (if (equal? sub-expr 'lambda)
                                       (second expr)
                                       (if (cons? (second expr))
                                           (rest (second expr))
                                           empty)))
                  (define total-args
                    (append new-args (filter (lambda (elt)
                                               (not (contains? elt new-args)))
                                             args)))]
            (fold-expr expr total-args replacements gensym))]
         [(equal? sub-expr 'quote) (make-linfo expr empty gensym)]
         [else (fold-expr expr args replacements gensym)]))]
    [else (make-linfo expr empty gensym)]))

;; collect-lift: s-expr number -> gensym-hold
;; consumes a top-level expression and a gensym counter
;; returns a gensym-hold where the gensym counter is the new value
;;    and the dat is a list of top-level expression with all locals from
;;    the original lifted to top level such that the new list of expression
;;    is symantically equivalent to the original expression
(define (collect-lift expr gensym)
  (local [(define lifted (lift-local expr empty empty-hash gensym))]
    (make-gensym-hold (linfo-gensym lifted)
                      (reverse (cons (linfo-return lifted)
                                     (linfo-toplevel lifted))))))

;; lift-program: (listof s-expr) -> (listof s-expr)
;; takes a list of top level statements
;; outputs a symantically equivalent list of top level statements
;;    with all local definitions in all statements lifted to top level
(define (lift-program expr)
  (unwrap
   (gensym-hold-dat
    (foldl (lambda (an-expr old-lifted)
             (local [(define new-lifted
                       (collect-lift an-expr (gensym-hold-gensym old-lifted)))]
               (make-gensym-hold (gensym-hold-gensym new-lifted)
                                 (append (gensym-hold-dat old-lifted)
                                         (gensym-hold-dat new-lifted)))))
           (make-gensym-hold 0 empty)
           (rename-top-level (rename-toplevel-structs expr))))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Storage
#;(
   
;; get-new-def: s-expr number (listof wrapped) -> s-expr
;; consumes a define statement in symbolic form with no local definitions,
;;    a gensym number, and a list of wrapped arguments
;; returns a new lifted function definition
(define (get-new-def expr gensym ext-args)
  (local [(define id-prepend (string-append "l" (number->string gensym) "_"))]
    (if (or (not (cons? expr))
            (not (equal? (first expr) 'define)))
        (error 'get-new-def "expected symbolic expression starting with 'define'.")
        (cond
          [(symbol? (second expr))
           (replace-ids (list* 'define (cons (second expr) ext-args) (rest (rest expr)))
                        (hash-set empty-hash
                                  (second expr)
                                  (mod-symbol id-prepend (second expr) "")))]
          [(cons? (second expr))
           (local [(define filtered-ext-args
                     (filter (lambda (elt)
                               (not (contains? elt (rest (second expr)))))
                             ext-args))]
             (replace-ids (list 'define
                                (cons (first (second expr)) filtered-ext-args)
                                (list* 'lambda
                                       (rest (second expr))
                                       (rest (rest expr))))
                          (hash-set empty-hash
                                    (first (second expr))
                                    (make-wrapped
                                     (mod-symbol id-prepend
                                                 (first (second expr))
                                                 "")))))]))))
   
;; gen-temps: number (listof wrapped) -> (listof wrapped)
;; takes a positive integer and a list of symbols
;;    and generates a list of that many distict symbols
;;    to be used as temporary variables and prepends them to the existing list
#;(define (gen-temps num acc)
  (cond
    [(<= num 0) acc]
    [(> num 0)
     (gen-temps (sub1 num) (cons (make-wrapped (string->symbol
                                                (string-append
                                                 "tmp" (number->string num))))
                                 acc))]))

;; get-temp-id: number -> symbol
;; takes a gensym number and returns that number appended to "tmp" as a symbol
(define (get-temp-id num)
  (make-wrapped (string->symbol (string-append "tmp" (number->string num)))))

;; fix-top-defs: ???
(define (fix-top-defs def-list temp-replaces replace-hash)
  (map (lambda (a-def)
         (local [(define filtered-temps
                   (filter (lambda (elt) (not (equal? (temp-set-final elt)
                                                      (second a-def))))
                           temp-replaces))
                 (define new-replaces
                   (foldl (lambda (a-tmp-set a-hash)
                            (hash-set a-hash
                                      (temp-set-orig a-tmp-set)
                                      (temp-set-temp a-tmp-set)))
                          replace-hash
                          filtered-temps))]
           (replace-ids
            (if (empty? filtered-temps)
                a-def
                (list 'define
                      (second a-def)
                      (cons (list 'lambda
                                  (map (lambda (elt) (temp-set-temp elt))
                                       filtered-temps)
                            (third a-def))
                      (map (lambda (elt) (temp-set-final elt)) filtered-temps))))
            new-replaces)))
       def-list))
                           

;; function-def?: s-expr -> boolean
;; takes a symbolic expression
;; returns true if the expression is guarenteed to be
;;    the lifted definition of a procedure
;;    false otherwise
(define (function-def? an-expr)
  (and (cons? an-expr)
       (equal? (first an-expr) 'define)
       (cons? (rest an-expr))
       (cons? (rest (rest an-expr)))
       (cons? (third an-expr))
       (equal? (first (third an-expr)) 'lambda)))

   
   #;[(equal? sub-expr 'local)
          (local [(define local-struct-prepend
                    (string-append "s" (number->string gensym) "_"))
                  (define struct-defs (filter (lambda (elt) (equal? (first elt)
                                                                    'define-struct))
                                              (second expr)))
                  (define struct-names (get-struct-names (second expr)))
                  (define reg-defs (filter (lambda (elt) (not (equal? (first elt)
                                                                      'define-struct)))
                                           (second expr)))
                  (define old-local-ids (map (lambda (elt) (if (cons? (second elt))
                                                               (first (second elt))
                                                               (second elt)))
                                             reg-defs))
                  (define gensym-defs
                    (foldl (lambda (an-expr other-defs)
                             (local [(define rec-info
                                       (lift-local an-expr
                                                   args
                                                   replacements
                                                   (gensym-hold-gensym other-defs)))]
                               (make-gensym-hold (add1 (linfo-gensym rec-info))
                                                 (cons (cons (get-new-def
                                                              (linfo-return rec-info)
                                                              (linfo-gensym rec-info)
                                                              args)
                                                             (linfo-toplevel rec-info))
                                                       (gensym-hold-dat other-defs)))))
                           (make-gensym-hold gensym empty)
                           reg-defs))
                  (define new-replace-defs (reverse
                                            (map first (gensym-hold-dat gensym-defs))))
                  (define (get-filter-sets gensym proc old-ids new-defs acc)
;                    (begin
;                      (printf "get-filter-sets new-defs is ~a\n" (unwrap new-defs))
                    (cond
                      [(empty? old-ids) acc]
                      [(cons? old-ids)
                       (get-filter-sets (if (proc (first new-defs))
                                            (add1 gensym)
                                            gensym)
                                        proc
                                        (rest old-ids)
                                        (rest new-defs)
                                        (if (proc (first new-defs))
                                            (cons (make-temp-set (first old-ids)
                                                                 (get-temp-id gensym)
                                                                 (second 
                                                                  (first new-defs)))
                                                  acc)
                                            acc))]));)
                  (define temp-replaces
                    (get-filter-sets 0
                                     (lambda (an-expr) (not (function-def? an-expr)))
                                     old-local-ids
                                     new-replace-defs
                                     empty))
                  (define lit-replaces
                    (get-filter-sets 0
                                     function-def?
                                     old-local-ids
                                     new-replace-defs
                                     empty))
                  (define lit-replace-hash
                    (foldl (lambda (a-tmp-set a-hash)
                             (hash-set a-hash
                                       (temp-set-orig a-tmp-set)
                                       (temp-set-final a-tmp-set)))
                           replacements
                           lit-replaces))
                  (define new-replacements
                    (foldl (lambda (a-tmp-set a-hash)
                             (hash-set a-hash
                                       (temp-set-orig a-tmp-set)
                                       (temp-set-temp a-tmp-set)))
                           lit-replace-hash
                           temp-replaces))
                  (define new-toplevel-defs
                    (fix-top-defs (foldr append empty (gensym-hold-dat gensym-defs))
                                  temp-replaces
                                  lit-replace-hash))
                    #;(append (replace-struct-ids struct-defs
                                                (string-append "s"
                                                               (number->string gensym)
                                                               "_")
                                                struct-names)
                            (replace-ids
                             (replace-struct-ids
                              (foldr append empty (gensym-hold-dat gensym-defs))
                              (string-append "s" (number->string gensym) "_")
                              struct-names)
                             new-replacements));)
                  (define new-gensym (gensym-hold-gensym gensym-defs))]
            
            (local [(define lifted-body
                      (fold-expr (third expr) args new-replacements new-gensym))]
;              (begin
;                (printf "new-replacements is\n ~a\n" new-replacements)
              (make-linfo (replace-struct-ids
                           (if (empty? temp-replaces)
                               (linfo-return lifted-body)
                               (cons (list 'lambda
                                           (map (lambda (elt) (temp-set-temp elt))
                                                temp-replaces)
                                           (linfo-return lifted-body))
                                     (map (lambda (elt) (temp-set-final elt))
                                          temp-replaces)))
                           local-struct-prepend
                           struct-names)
                          (append (linfo-toplevel lifted-body)
                                  (replace-struct-ids new-toplevel-defs
                                                      local-struct-prepend
                                                      struct-names))
                          (linfo-gensym lifted-body))))];)]
   
                         
                         #;(reverse
                          (gensym-hold-dat
                           (foldl (lambda (new-def def-list)
                                    (if (and (cons? (third (wrapped-expr new-def)))
                                             (equal? (first(third (wrapped-expr new-def)))
                                                'lambda))
                                        def-list
                                        (make-gensym-hold
                                         (add1 (gensym-hold-gensym def-list))
                                         (cons (make-wrapped
                                                (make-temp (gensym-hold-gensym def-list)))
                                               (gensym-hold-dat def-list)))))
                                  (make-gensym-hold 0 empty)
                                  (gensym-hold-dat gensym-defs))));)
                  #;(define new-replace-list
                    (reverse
                     (gensym-hold-dat
                      (foldl (lambda (new-def def-list)
                               (if (and (cons? (third (wrapped-expr new-def)))
                                        (equal? (first (third (wrapped-expr new-def)))
                                                'lambda))
                                   (make-gensym-hold (gensym-hold-gensym def-list)
                                                     (cons (second
                                                            (wrapped-expr new-def))
                                                           (gensym-hold-dat new-def)))
                                   (make-gensym-hold (add1
                                                      (gensym-hold-gensym def-list))
                                                     (make-temp
                                                      (gensym-hold-genysm def-list)))))
                             (make-gensym-hold 0 empty)
                             (gensym-hold-dat gensym-defs)))))
                                  
                    
;                    (map first (gensym-hold-dat gensym-defs)))
                  #;(define (get-replacements a-hash old-ids new-expr)
                    (cond
                      [(empty? old-ids) a-hash]
                      [(cons? old-ids)
                       (get-replacements (hash-set a-hash
                                                   (first old-ids)
                                                   (first new-expr))
                                         (rest old-ids)
                                         (rest new-expr))]))
                  #;(define new-replacements (get-replacements replacements
                                                             old-local-ids
                                                             new-replace-list))
)