#lang scheme/base

(require scheme/local
         scheme/list
         scheme/bool
         scheme/string
         scheme/contract
         scheme/path)


;; A program is a (listof (or/c defn? expr? test-case? library-require?))


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


;; defn?: s-expression -> boolean
(define (defn? an-sexp)
  (cond
    [(list-begins-with? an-sexp 'define)
     #t]
    [(list-begins-with? an-sexp 'define-struct)
     #t]
    [else
     #f]))


;; list-begins-with?: sexp symbol -> boolean
;; Returns true if the input is a list with the label as the first element.
(define (list-begins-with? an-sexp a-label)
  (and (list? an-sexp)
       (not (empty? an-sexp))
       (symbol=? (first an-sexp))
       (symbol=? (first an-sexp) a-label)))


;; test-case?: s-expression -> boolean
(define (test-case? an-sexp)
  (or (list-begins-with? an-sexp 'check-expect)
      (list-begins-with? an-sexp 'check-within)
      (list-begins-with? an-sexp 'check-error)))



;; library-require?: s-expression -> boolean
(define (library-require? an-sexp)
  (list-begins-with? an-sexp 'require))





;; identifier->munged-java-identifier: symbol -> symbol
(define (identifier->munged-java-identifier an-id)
  (local [(define java-identifiers
            '(abstract  continue  	for  	new  	switch
                        assert 	default 	goto 	package 	synchronized
                        boolean 	do 	if 	private 	#; this
                        break 	double 	implements 	protected 	throw
                        byte 	else 	import 	public 	throws
                        case 	enum 	instanceof 	return 	transient
                        catch 	extends 	int 	short 	try
                        char 	final 	interface 	static 	void
                        class 	finally 	long 	strictfp 	volatile
                        const 	float 	native 	super 	while))
          ;; Special character mappings for identifiers
          (define char-mappings 
            #hash((#\- . "_dash_")
                  (#\_ . "_underline_")
                  (#\? . "_question_")
                  (#\! . "_bang_")
                  (#\. . "_dot_")
                  (#\: . "_colon_")
                  (#\= . "_equal_")
                  (#\# . "_pound_")
                  (#\$ . "_dollar_")
                  (#\% . "_percent_")
                  (#\^ . "_tilde_")
                  (#\& . "_and_")
                  (#\* . "_star_")
                  (#\+ . "_plus_")
                  (#\* . "_star_")
                  (#\/ . "_slash_")
                  (#\< . "_lessthan_")
                  (#\> . "_greaterthan_")
                  (#\~ . "_tilde_")))]
    (cond
      [(member an-id java-identifiers)
       (string->symbol (format "_nonclashing_~a" an-id))]
      [else
       (let* ([chars (string->list (symbol->string an-id))]
              [translated-chunks 
               (map (lambda (ch) (hash-ref char-mappings ch (string ch))) chars)]
              [translated-id
               (string->symbol
                (string-join translated-chunks ""))])
         translated-id)])))


;; path=?: path path -> boolean
;; Returns true if the paths refer to the same file.
(define (path=? path-1 path-2)
  (string=? (path->string (normalize-path path-1))
            (path->string (normalize-path path-2))))


(provide/contract [program? (any/c . -> . boolean?)]
                  [expression? (any/c . -> . boolean?)]
                  [defn? (any/c . -> . boolean?)]
                  [test-case? (any/c . -> . boolean?)]
                  [library-require? (any/c . -> . boolean?)]
                  [identifier->munged-java-identifier (symbol? . -> . symbol?)]
                  [path=? (path? path? . -> . boolean?)])