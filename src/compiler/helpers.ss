#lang s-exp "lang.ss"

(require "rbtree.ss")
(require "../collects/moby/runtime/stx.ss")
(require "../collects/moby/runtime/error-struct.ss")

(define pair? cons?)

;; A program is a (listof (or/c defn? expr? library-require? provide-statement? require-permission?))

(define (list? datum)
  (or (empty? datum)
      (and
       (pair? datum)
       (list? (rest datum)))))


;; symbol<: symbol symbol -> boolean
(define (symbol< x y)
  (string<? (symbol->string x)
            (symbol->string y)))


;; expression<: expression expression -> boolean
;; Induces an ordering of expressions.
;; Returns true if one expression is less than another.
;; FIXME: this is a partial function at the moment: it doesn't know how to handle
;; +-inf.0, nan.0, or complex numbers yet.
(define (expression<? x y)
  (cond
    [(< (expression-type-number x)
        (expression-type-number y))
     true]
    [(= (expression-type-number x)
        (expression-type-number y))
     (cond
       [(number? (stx-e x))
        ;; FIXME: bug here if x is not a finite real number.
        (< (stx-e x) (stx-e y))]
       [(string? (stx-e x))
        (string<? (stx-e x) (stx-e y))]
       [(boolean? (stx-e x))
        (< (if (stx-e x) 1 0) (if (stx-e y) 1 0))]
       [(char? (stx-e x))
        (char<? (stx-e x) (stx-e y))]
       [(symbol? (stx-e x))
        (symbol< (stx-e x) (stx-e y))]
       [(pair? (stx-e x))
        (cond
          [(< (length (stx-e x))
              (length (stx-e y)))
           true]
          [(= (length (stx-e x))
              (length (stx-e y)))
           (ormap expression<? (stx-e x) (stx-e y))]
          [else
           false])]
       [(empty? (stx-e x))
        false])]
    [else
     false]))


;; expression-type-number: expression -> number
;; Produces an arbitrary but consistent numbering on an expression based on its type.
(define (expression-type-number x)
  (cond
    [(number? (stx-e x))
     0]
    [(string? (stx-e x))
     1]
    [(boolean? (stx-e x))
     2]
    [(char? (stx-e x))
     3]
    [(symbol? (stx-e x))
     4]
    [(empty? (stx-e x))
     5]
    [(pair? (stx-e x))
     6]))





;; program: any -> boolean
;; Returns true if the datum is a program.
(define (program? datum)
  (and (list? datum)
       (andmap (lambda (x) 
                 (or (defn? x)
                     (expression? x)
                     (library-require? x)
                     (provide-statement? x)
                     (require-permission? x)))
               datum)))


;; expression?: any -> boolean
;; Returns true if the datum is an expression.
(define (expression? an-expr)
  (and (not (defn? an-expr))
       (not (library-require? an-expr))
       (not (provide-statement? an-expr))
       (not (require-permission? an-expr))))


;; defn?: stx -> boolean
(define (defn? an-sexp)
  (cond
    [(stx-begins-with? an-sexp 'define)
     true]
    [(stx-begins-with? an-sexp 'define-struct)
     true]
    [(stx-begins-with? an-sexp 'define-values)
     true]
    [else
     false]))


;; provide-statement?: stx -> boolean
;; Produces true if the syntax looks like a provide.
(define (provide-statement? an-sexp)
  (stx-begins-with? an-sexp 'provide))


;; require-permission?: stx -> boolean
;; Produces true if the syntax looks like a permission-requirement.
(define (require-permission? an-sexp)
  (stx-begins-with? an-sexp 'require-permission))


;; string-join: (listof string) string -> string
(define (string-join strs delim)
  (cond
    [(empty? strs)
     ""]
    [(empty? (rest strs))
     (first strs)]
    [else
     (string-append
      (first strs)
      delim
      (string-join (rest strs) delim))]))


;; string-split: string char -> (listof string)
(define (string-split a-str delim)
  (local [(define (add-word acc)
            (list (cons (list->string (reverse (second acc)))
                        (first acc))
                  empty))
          (define (accumulate-character acc ch)
            (list (first acc)
                  (cons ch (second acc))))]
    (reverse (first
              (add-word
               (foldl (lambda (ch acc)
                        (cond [(char=? ch delim)
                               (add-word acc)]
                              [else
                               (accumulate-character acc ch)]))
                      (list empty empty)
                      (string->list a-str)))))))


;; test-case?: stx -> boolean
(define (test-case? an-sexp)
  (or (stx-begins-with? an-sexp 'check-expect)
      (stx-begins-with? an-sexp 'EXAMPLE)
      (stx-begins-with? an-sexp 'check-within)
      (stx-begins-with? an-sexp 'check-error)))



;; library-require?: stx -> boolean
(define (library-require? an-sexp)
  (stx-begins-with? an-sexp 'require))


;; java-identifiers: (rbtreeof symbol boolean)
(define java-identifiers
  (foldl (lambda (sym an-rbtree)
           (rbtree-insert symbol< an-rbtree sym true))
         empty-rbtree
         
         '(abstract  continue  	for  	new  	switch
                     assert 	default 	goto 	package 	synchronized
                     boolean 	do 	if 	private 	#; this
                     break 	double 	implements 	protected 	throw
                     byte 	delete  else 	import 	public 	throws
                     case 	enum 	instanceof instanceOf 	return 	transient
                     catch 	extends 	int 	short 	try
                     char 	final 	interface 	static 	void
                     class 	finally 	long 	strictfp 	volatile
                     const 	float 	native 	super 	while null
                     
                     comment export import in label typeof with false true
                     debugger)))


;; special-character-mappings: (rbtreeof char string)
(define special-character-mappings
  (foldl (lambda (ch+translation an-rbtree)
           (rbtree-insert char<? an-rbtree (first ch+translation) (second ch+translation)))
         empty-rbtree
         '((#\- "_dash_")
           (#\_ "_underline_")
           (#\? "_question_")
           (#\! "_bang_")
           (#\. "_dot_")
           (#\: "_colon_")
           (#\= "_equal_")
           (#\@ "_at_")
           (#\# "_pound_")
           (#\$ "_dollar_")
           (#\% "_percent_")
           (#\^ "_tilde_")
           (#\& "_and_")
           (#\* "_star_")
           (#\+ "_plus_")
           (#\/ "_slash_")
           (#\< "_lessthan_")
           (#\> "_greaterthan_")
           (#\~ "_tilde_"))))


;; translate-special-character: char -> string
;; Special character mappings for identifiers.
(define (translate-special-character ch)
  (cond
    [(cons? (rbtree-lookup char<? special-character-mappings ch))
     (second (rbtree-lookup char<? special-character-mappings ch))]
    [else
     (string ch)]))


;; identifier->munged-java-identifier: symbol -> symbol
(define (identifier->munged-java-identifier an-id)
  (cond
    [(cons? (rbtree-lookup symbol< java-identifiers an-id))
     (string->symbol (string-append "_" (symbol->string an-id) "_"))]
    [else
     (local [(define (maybe-prepend-hyphen chars)
               (cond
                 [(member (first chars) (string->list "0123456789"))
                  (cons #\- chars)]
                 [else
                  chars]))
             (define chars (maybe-prepend-hyphen (string->list (symbol->string an-id))))
             (define translated-chunks 
               (map translate-special-character chars))
             (define translated-id
               (string->symbol
                (string-join translated-chunks "")))]
       translated-id)]))




;; remove-leading-whitespace/list: (listof char) -> string
;; Removes leading whitespace from a list of characters.
(define (remove-leading-whitespace/list chars)
  (cond
    [(empty? chars)
     ""]
    [(char-whitespace? (first chars))
     (remove-leading-whitespace/list (rest chars))]
    [else
     (list->string chars)]))


;; remove-leading-whitespace: string -> string
;; Removes the whitespace from the front of a string.
(define (remove-leading-whitespace a-str)
  (remove-leading-whitespace/list (string->list a-str)))


;; take: (listof X) number -> (listof X)
;; Produces a list of the first n elmeents of a-list.
(define (take a-list n)
  (cond
    [(= n 0)
     empty]
    [else
     (cons (first a-list)
           (take (rest a-list) (sub1 n)))]))


;; list-tail: (listof X) number -> (listof X)
;; Produces a list of the last n elmeents in a-list.
(define (list-tail a-list n)
  (cond
    [(= n 0)
     a-list]
    [else
     (list-tail (rest a-list)
                (sub1 n))]))


;; range: number -> number
;; Produces a list of the numbers [0, ..., n).
(define (range n)
  (cond
    [(= n 0)
     empty]
    [else
     (append (range (sub1 n))
             (list (sub1 n)))]))


;; stx-list-of-symbols?: stx -> boolean
;; Produces true if an-stx is a syntax containing a list of symbols.
(define (stx-list-of-symbols? an-stx)
  (and (stx:list? an-stx)
       (andmap (lambda (elt) (symbol? (stx-e elt)))
               (stx-e an-stx))))


;; Helper to help with the destructuring and case analysis of functions.
(define (case-analyze-definition a-definition 
                                 f-function             ;; (stx (listof stx) expr-stx) -> ...
                                 f-regular-definition   ;; (stx expr-stx) -> ...
                                 f-define-struct        ;; (stx (listof id-stx)) -> ...
                                 f-define-values        ;; ((listof id-stx) stx) -> ... 
                                 )
  (cond
    ;; (define (id args ...) body)
    [(and (stx-begins-with? a-definition 'define)
          (= (length (stx-e a-definition)) 3)
          (stx-list-of-symbols? (second (stx-e a-definition))))
     (local [(define id (first (stx-e (second (stx-e a-definition)))))
             (define args (rest (stx-e (second (stx-e a-definition)))))
             (define body (third (stx-e a-definition)))]
       (begin
         (check-single-body-stx! (rest (rest (stx-e a-definition))) a-definition)
         (f-function id args body)))]
    
    
    ;; (define id (lambda (args ...) body))
    [(and (stx-begins-with? a-definition 'define)
          (= (length (stx-e a-definition)) 3)
          (symbol? (stx-e (second (stx-e a-definition))))
          (stx-begins-with? (third (stx-e a-definition)) 'lambda))
     (local [(define id (second (stx-e a-definition)))
             (define args (stx-e (second (stx-e (third (stx-e a-definition))))))
             (define body (third (stx-e (third (stx-e a-definition)))))]
       (begin
         (check-single-body-stx! (rest (rest (stx-e (third (stx-e a-definition))))) a-definition)
         (f-function id args body)))]
    
    ;; (define id body)
    [(and (stx-begins-with? a-definition 'define)
          (= (length (stx-e a-definition)) 3)
          (symbol? (stx-e (second (stx-e a-definition))))
          (not (stx-begins-with? (third (stx-e a-definition)) 'lambda)))
     (local [(define id (second (stx-e a-definition)))
             (define body (third (stx-e a-definition)))]
       (f-regular-definition id body))]
    
    ;(define-struct id (fields ...))    
    [(and (stx-begins-with? a-definition 'define-struct)
          (= (length (stx-e a-definition)) 3)
          (symbol? (stx-e (second (stx-e a-definition))))
          (or (empty? (stx-e (third (stx-e a-definition))))
              (pair? (stx-e (third (stx-e a-definition))))))
     (local [(define id (second (stx-e a-definition)))
             (define fields (stx-e (third (stx-e a-definition))))]
       (f-define-struct id fields))]
    
    ;; (define-values (id ...) body)
    [(and (stx-begins-with? a-definition 'define-values)
          (= (length (stx-e a-definition)) 3)
          (stx-list-of-symbols? (second (stx-e a-definition))))
     (local [(define ids (stx-e (second (stx-e a-definition))))
             (define body (third (stx-e a-definition)))]
       (f-define-values ids body))]
    
    
    
    ;; FIXME: add more error productions as necessary to get
    ;; reasonable error messages.
    [(stx-begins-with? a-definition 'define)
     (raise (make-moby-error 
             (stx-loc a-definition)
             (make-moby-error-type:generic-syntactic-error
              "define expects either an identifier and a body: (define answer 42), or a function header and body: (define (double x ) (* x 2))"
              (list))))]
    
    [(stx-begins-with? a-definition 'define-struct)
     (raise (make-moby-error
             (stx-loc a-definition)
             (make-moby-error-type:generic-syntactic-error
              "define-struct expects an identifier and a list of fields.  i.e. (define-struct pizza (dough sauce toppings))"
              (list))))]
    
    [(stx-begins-with? a-definition 'define-values)
     (raise (make-moby-error
             (stx-loc a-definition)
             (make-moby-error-type:generic-syntactic-error
              "define-values expects a list of identifiers and a body.  i.e. (define-values (x) 42)"
              (list))))]
    
    ))




;; symbol-stx?: any -> boolean
;; Produces true when x is a symbol syntax object.
(define (symbol-stx? x)
  (and (stx? x)
       (symbol? (stx-e x))))




;; check-duplicate-identifiers!: (listof stx) -> void
;; Return a list of the identifiers that are duplicated.
;; Also check to see that each of the ids is really a symbolic identifier.
(define (check-duplicate-identifiers! ids)
  (local [(define seen-ids (make-hash))
          
          (define (loop ids)
            (cond
              [(empty? ids)
               (void)]
              [else
               (cond [(stx? (hash-ref seen-ids (stx-e (first ids)) #f))
                      (raise (make-moby-error (stx-loc (first ids))
                                              (make-moby-error-type:duplicate-identifier
                                               (stx-e (first ids))
                                               (stx-loc (hash-ref seen-ids (stx-e (first ids)) #f)))))]
                     [(not (symbol? (stx-e (first ids))))
                      (raise (make-moby-error (stx-loc (first ids))
                                              (make-moby-error-type:expected-identifier (first ids))))]
                     [else
                      (begin
                        (hash-set! seen-ids (stx-e (first ids)) (first ids))
                        (loop (rest ids)))])]))]
    (loop ids)))



;; check-single-body-stx!: (listof stx) stx -> void
(define (check-single-body-stx! stxs original-stx)
  (cond
    [(empty? stxs)
     (raise
      (make-moby-error (stx-loc original-stx)
                       (make-moby-error-type:generic-syntactic-error
                        "I expected a single body in this expression, but I didn't find any."
                        (list))))]
    [(not (empty? (rest stxs)))
     (raise
      (make-moby-error (stx-loc original-stx)
                       (make-moby-error-type:generic-syntactic-error
                        "I expected a single body in this expression, but I found more than one."
                        (map stx-loc (rest stxs)))))]
    [else
     (void)]))


;; mapi: (X number -> Y) (listof X) -> (listof Y)
(define (mapi f lst)
  (local ([define (loop lst i)
            
            (cond
              [(empty? lst)
               empty]
              [else
               (cons (f (first lst) i)
                     (loop (rest lst) (add1 i)))])])
    (loop lst 0)))


(provide/contract [symbol< (symbol? symbol? . -> . boolean?)]
                  [mapi ((any/c number? . -> . any/c) (listof any/c) . -> . (listof any/c))]
                  [program? (any/c . -> . boolean?)]
                  [expression? (any/c . -> . boolean?)]
                  [defn? (any/c . -> . boolean?)]
                  [test-case? (any/c . -> . boolean?)]
                  [require-permission? (any/c . -> . boolean?)]
                  [library-require? (any/c . -> . boolean?)]
                  [provide-statement? (any/c . -> . boolean?)]
                  [take ((listof any/c) number? . -> . (listof any/c))]
                  [list-tail ((listof any/c) number? . -> . (listof any/c))]
                  
                  [expression<? (expression? expression? . -> . boolean?)]
                  
                  [remove-leading-whitespace (string? . -> . string?)]
                  [identifier->munged-java-identifier (symbol? . -> . symbol?)]
                  [range (number? . -> . (listof number?))]
                  
                  
                  [check-duplicate-identifiers! ((listof stx?)  . -> . any)]
                  
                  [check-single-body-stx! ((listof stx?) stx? . -> . any)]
                  [case-analyze-definition (stx? 
                                            (symbol-stx? (listof symbol-stx?) stx? . -> . any)
                                            (symbol-stx? any/c . -> . any)
                                            (symbol-stx? (listof symbol-stx?) . -> . any)
                                            ((listof symbol-stx?) stx? . -> . any)
                                            . -> . any)]
                  [string-join ((listof string?) string? . -> . string?)]
                  [string-split (string? char? . -> . (listof string?))])