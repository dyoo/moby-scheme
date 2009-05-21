#lang s-exp "lang.ss"


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


;; defn?: s-expression -> boolean
(define (defn? an-sexp)
  (cond
    [(list-begins-with? an-sexp 'define)
     true]
    [(list-begins-with? an-sexp 'define-struct)
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


;; list-begins-with?: sexp symbol -> boolean
;; Returns true if the input is a list with the label as the first element.
(define (list-begins-with? an-sexp a-label)
  (and (list? an-sexp)
       (not (empty? an-sexp))
       (symbol? (first an-sexp))
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
          (define (trans ch)
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
               (string ch)]))]
    (cond
      [(member an-id java-identifiers)
       (string->symbol (format "_nonclashing_~a" an-id))]
      [else
       (local [(define chars (string->list (symbol->string an-id)))
               (define translated-chunks 
                 (map trans chars))
               (define translated-id
                 (string->symbol
                  (string-join translated-chunks "")))]
         translated-id)])))




;; desugar-cond: expr -> expr
;; Translates conds to ifs.
(define (desugar-cond an-expr)
  (local
    [(define (loop questions answers question-last answer-last)
       (cond
         [(empty? questions)
          `(if ,question-last ,answer-last (error 'cond "Fell out of cond"))]
         [else
          `(if ,(first questions) 
               ,(first answers)
               ,(loop (rest questions)
                      (rest answers)
                      question-last
                      answer-last))]))]
    
    (match an-expr
      [(list 'cond [list questions answers] ... [list 'else answer-last])
       (loop questions answers 'true answer-last)]
      
      [(list 'cond [list questions answers] ... [list question-last answer-last])
       (loop questions answers question-last answer-last)])))



(define (remove-leading-whitespace a-str)
  (local [(define (remove-leading-whitespace/list chars)
            (cond
              [(empty? chars)
               empty]
              [(char-whitespace? (first chars))
               (remove-leading-whitespace/list (rest chars))]
              [else
               (list->string chars)]))]
    (remove-leading-whitespace/list (string->list a-str))))


(define (take a-list n)
  (cond
    [(= n 0)
     empty]
    [else
     (cons (first a-list)
           (take (rest a-list) (sub1 n)))]))

(define (list-tail a-list n)
  (reverse (take (reverse a-list) n)))

(define (range n)
  (cond
    [(= n 0)
     empty]
    [else
     (append (range (sub1 n))
             (list (sub1 n)))]))


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
                  [list-begins-with? (any/c symbol? . -> . boolean?)]
                  [take ((listof any/c) number? . -> . (listof any/c))]
                  [list-tail ((listof any/c) number? . -> . (listof any/c))]
                  [remove-leading-whitespace (string? . -> . string?)]
                  [identifier->munged-java-identifier (symbol? . -> . symbol?)]
                  [desugar-cond (any/c . -> . any/c)]
                  [range (number? . -> . (listof number?))]
                  
                  [path=? (path? path? . -> . boolean?)]
                  [string-join ((listof string?) string? . -> . string?)])