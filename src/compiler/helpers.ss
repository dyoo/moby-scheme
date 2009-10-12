#lang s-exp "lang.ss"

(require "stx.ss")


;; A program is a (listof (or/c defn? expr? test-case? library-require?))

(define (list? datum)
  (or (empty? datum)
      (and
       (pair? datum)
       (list? (rest datum)))))


;; program: any -> boolean
;; Returns true if the datum is a program.
(define (program? datum)
  (and (list? datum)
       (andmap (lambda (x) 
                 (or (defn? x)
                     (expression? x)
                     (test-case? x)
                     (library-require? x)))
               datum)))


;; expression?: any -> boolean
;; Returns true if the datum is an expression.
(define (expression? an-expr)
  (and (not (defn? an-expr))
       (not (test-case? an-expr))
       (not (library-require? an-expr))))


;; defn?: stx -> boolean
(define (defn? an-sexp)
  (cond
    [(stx-begins-with? an-sexp 'define)
     true]
    [(stx-begins-with? an-sexp 'define-struct)
     true]
    [else
     false]))


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



;; test-case?: stx -> boolean
(define (test-case? an-sexp)
  (or (stx-begins-with? an-sexp 'check-expect)
      (stx-begins-with? an-sexp 'check-within)
      (stx-begins-with? an-sexp 'check-error)))



;; library-require?: stx -> boolean
(define (library-require? an-sexp)
  (stx-begins-with? an-sexp 'require))


(define java-identifiers
  '(abstract  continue  	for  	new  	switch
              assert 	default 	goto 	package 	synchronized
              boolean 	do 	if 	private 	#; this
              break 	double 	implements 	protected 	throw
              byte 	else 	import 	public 	throws
              case 	enum 	instanceof 	return 	transient
              catch 	extends 	int 	short 	try
              char 	final 	interface 	static 	void
              class 	finally 	long 	strictfp 	volatile
              const 	float 	native 	super 	while null))
          

;; translate-special-character: char -> string
;; Special character mappings for identifiers.
(define (translate-special-character ch)
  (cond
    [(char=? ch #\-)
     "_dash_"]
    [(char=? ch #\_)
     "_underline_"]
    [(char=? ch #\?)
     "_question_"]
    [(char=? ch #\!)
     "_bang_"]
    [(char=? ch #\.)
     "_dot_"]
    [(char=? ch #\:)
     "_colon_"]
    [(char=? ch #\=)
     "_equal_"]
    [(char=? ch #\#)
     "_pound_"]
    [(char=? ch #\$)
     "_dollar_"]
    [(char=? ch #\%)
     "_percent_"]
    [(char=? ch #\^)
     "_tilde_"]
    [(char=? ch #\&)
     "_and_"]
    [(char=? ch #\*)
     "_star_"]
    [(char=? ch #\+)
     "_plus_"]
    [(char=? ch #\*)
     "_star_"]
    [(char=? ch #\/)
     "_slash_"]
    [(char=? ch #\<)
     "_lessthan_"]
    [(char=? ch #\>)
     "_greaterthan_"]
    [(char=? ch #\~)
     "_tilde_"]
    [else
     (string ch)]))


;; identifier->munged-java-identifier: symbol -> symbol
(define (identifier->munged-java-identifier an-id)
  (cond
    [(member an-id java-identifiers)
     (string->symbol (string-append "_" (symbol->string an-id) "_"))]
    [else
     (local [(define chars (string->list (symbol->string an-id)))
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



;; Helper to help with the destructuring and case analysis of functions.
(define (case-analyze-definition a-definition 
                                 f-function            ;; (stx (listof stx) expr-stx) -> ...
                                 f-regular-definition  ;; (stx expr-stx) -> ...
                                 f-define-struct)      ;; (stx (listof id-stx)) -> ...
  (cond
    ;; (define (id args ...) body)
    [(and (stx-begins-with? a-definition 'define)
          (= (length (stx-e a-definition)) 3)
          (stx:list? (second (stx-e a-definition))))
     (local [(define id (first (stx-e (second (stx-e a-definition)))))
             (define args (rest (stx-e (second (stx-e a-definition)))))
             (define body (third (stx-e a-definition)))]
       (f-function id args body))]


    ;; (define id (lambda (args ...) body))
    [(and (stx-begins-with? a-definition 'define)
          (= (length (stx-e a-definition)) 3)
          (symbol? (stx-e (second (stx-e a-definition))))
          (stx-begins-with? (third (stx-e a-definition)) 'lambda))
     (local [(define id (second (stx-e a-definition)))
             (define args (stx-e (second (stx-e (third (stx-e a-definition))))))
             (define body (third (stx-e (third (stx-e a-definition)))))]
       (f-function id args body))]
    
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
    
    
    ;; FIXME: add more error productions as necessary to get
    ;; reasonable error messages.
    [(stx-begins-with? a-definition 'define)
     (error 
      'define 
      "define expects an identifier and a body.  i.e. (define answer 42)")]
    [(stx-begins-with? a-definition 'define-struct)
     (error 
      'define-struct 
      "define-struct expects an identifier and a list of fields.  i.e. (define-struct pizza (dough sauce toppings))")]))



;; symbol-stx?: any -> boolean
;; Produces true when x is a symbol syntax object.
(define (symbol-stx? x)
  (and (stx? x)
       (symbol? (stx-e x))))




;; check-duplicate-identifiers!: (listof stx) -> void
;; Return a list of the identifiers that are duplicated.
(define (check-duplicate-identifiers! ids)
  (local [(define (loop ids known-ids)
            (cond
              [(empty? ids)
               (void)]
              [else
               (cond [(member (stx-e (first ids)) known-ids)
                      (syntax-error "found a name that's used more than once" (first ids))]
                     [else
                      (loop (rest ids) 
                            (cons (stx-e (first ids)) 
                                  known-ids))])]))]
    (loop ids empty)))




(provide/contract [program? (any/c . -> . boolean?)]
                  [expression? (any/c . -> . boolean?)]
                  [defn? (any/c . -> . boolean?)]
                  [test-case? (any/c . -> . boolean?)]
                  [library-require? (any/c . -> . boolean?)]
                  [take ((listof any/c) number? . -> . (listof any/c))]
                  [list-tail ((listof any/c) number? . -> . (listof any/c))]
                  [remove-leading-whitespace (string? . -> . string?)]
                  [identifier->munged-java-identifier (symbol? . -> . symbol?)]
                  [range (number? . -> . (listof number?))]
                  
                  
                  [check-duplicate-identifiers! ((listof stx?)  . -> . any)]
                  
                  [case-analyze-definition (stx? 
                                            (symbol-stx? (listof symbol-stx?) stx? . -> . any)
                                            (symbol-stx? any/c . -> . any)
                                            (symbol-stx? (listof symbol-stx?) . -> . any)
                                            . -> . any)]
                  [string-join ((listof string?) string? . -> . string?)])