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


;; identifier->munged-java-identifier: symbol -> symbol
(define (identifier->munged-java-identifier an-id)
  (local [(define (member? an-id elts)
            (cond
              [(empty? elts)
               false]
              [(equal? (first elts) an-id)
               true]
              [else
               (member? an-id (rest elts))]))
          
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
      [(member? an-id java-identifiers)
       (string->symbol (string-append "_" (symbol->string an-id) "_"))]
      [else
       (local [(define chars (string->list (symbol->string an-id)))
               (define translated-chunks 
                 (map trans chars))
               (define translated-id
                 (string->symbol
                  (string-join translated-chunks "")))]
         translated-id)])))

;; desugar-cond: stx:list -> stx:list
;; Translates conds to ifs.
(define (desugar-cond an-expr)
  (local
    [;; loop: (listof stx) (listof stx) stx stx -> stx
     (define (loop questions answers question-last answer-last)
       (cond
         [(empty? questions)
          (make-stx:list (list (datum->stx 'if (stx-loc an-expr))
                               question-last
                               answer-last 
                               (datum->stx '(error 'cond "Fell out of cond")
                                           (stx-loc an-expr)))
                         (stx-loc an-expr))]

         [else
          (make-stx:list (list (datum->stx 'if (stx-loc an-expr))
                               (first questions)
                               (first answers)
                               (loop (rest questions)
                                     (rest answers)
                                     question-last
                                     answer-last))
                         (stx-loc an-expr))]))
     ;; process-clauses: (listof stx) (listof stx) (listof stx) -> stx
     (define (process-clauses clauses questions/rev answers/rev)
       (cond
         [(stx-begins-with? (first clauses) 'else)
          (if (not (empty? (rest clauses)))
              (syntax-error (format "cond: else clause should be the last one: ~s" (stx-e an-expr)) an-expr)
              (loop (reverse questions/rev) 
                    (reverse answers/rev) 
                    (datum->stx 'true (stx-loc (first clauses)))
                    (second (stx-e (first clauses)))))]
         [(empty? (rest clauses))
          (loop (reverse questions/rev) 
                (reverse answers/rev) 
                (first (stx-e (first clauses)))
                (second (stx-e (first clauses))))]
         [else
          (process-clauses (rest clauses)
                           (cons (first (stx-e (first clauses))) questions/rev) 
                           (cons (second (stx-e (first clauses))) answers/rev))]))]
    (cond
      [(stx-begins-with? an-expr 'cond)
       (process-clauses (rest (stx-e an-expr)) empty empty)]
      [else
       (syntax-error (format "Not a cond clause: ~s" (stx-e an-expr))
                     an-expr)])))

;; desugar-case: stx:list -> stx:list
;; translates case to if.
(define (desugar-case an-expr)
  (local
    [
     ;; predicate: stx -> stx
     (define predicate
       (make-stx:list (list (datum->stx 'lambda (stx-loc an-expr))
                            (make-stx:list (list (datum->stx 'check (stx-loc an-expr))) (stx-loc an-expr))
                            (make-stx:list (list (datum->stx 'equal? (stx-loc an-expr))
                                                 (datum->stx 'check (stx-loc an-expr))
                                                 (second (stx-e an-expr))) (stx-loc an-expr)))
                      (stx-loc an-expr)))
     ;; loop: (listof stx) (listof stx) stx stx -> stx
     (define (loop list-of-datum answers datum-last answer-last)
       (cond
         [(empty? list-of-datum)
          (make-stx:list (list (datum->stx 'if (stx-loc an-expr))
                               (make-stx:list (list (datum->stx 'ormap (stx-loc an-expr))
                                                    (make-stx:list (list (datum->stx 'lambda (stx-loc an-expr))
                                                                         (make-stx:list (list (datum->stx 'val (stx-loc an-expr))) (stx-loc an-expr))
                                                                         (make-stx:list (list predicate
                                                                                              (datum->stx 'val (stx-loc an-expr))) (stx-loc an-expr)))
                                                                   (stx-loc an-expr))
                                                    datum-last)
                                              (stx-loc an-expr))
                               answer-last
                               ;; FIXME: we should find a way to return nothing, (void)?
                               ;; current solution : (local [(define dummy 1)] (set! dummy 2)))
                               (make-stx:list (list (datum->stx 'local (stx-loc an-expr))
                                                    (make-stx:list 
                                                     (list
                                                      (make-stx:list (list (datum->stx 'define (stx-loc an-expr))
                                                                           (datum->stx 'dummy (stx-loc an-expr))
                                                                           (datum->stx 1 (stx-loc an-expr)))
                                                                     (stx-loc an-expr)))
                                                     (stx-loc an-expr))
                                                    (make-stx:list (list (datum->stx 'set! (stx-loc an-expr))
                                                                         (datum->stx 'dummy (stx-loc an-expr))
                                                                         (datum->stx '2 (stx-loc an-expr)))
                                                                   (stx-loc an-expr)))
                                              (stx-loc an-expr)))
                         (stx-loc an-expr))]
         [else
          (make-stx:list (list (datum->stx 'if (stx-loc an-expr))
                               (make-stx:list (list (datum->stx 'ormap (stx-loc an-expr))
                                                    (make-stx:list (list (datum->stx 'lambda (stx-loc an-expr))
                                                                         (make-stx:list (list (datum->stx 'val (stx-loc an-expr))) (stx-loc an-expr))
                                                                         (make-stx:list (list predicate
                                                                                              (datum->stx 'val (stx-loc an-expr))) (stx-loc an-expr)))
                                                                   (stx-loc an-expr))
                                                    (first list-of-datum))
                                              (stx-loc an-expr))
                               (first answers)
                               (loop (rest list-of-datum)
                                     (rest answers)
                                     datum-last
                                     answer-last))
                         (stx-loc an-expr))]))
     ;; process-clauses: (listof stx) (listof stx) (listof stx) -> stx
     (define (process-clauses clauses questions/rev answers/rev)
       (cond
         [(stx-begins-with? (first clauses) 'else)
          (if (not (empty? (rest clauses)))
              (syntax-error (format "case: else clause should be the last one: ~s" (stx-e an-expr)) an-expr)
              (loop (reverse questions/rev) 
                    (reverse answers/rev)
                    (make-stx:list (list (datum->stx 'list (stx-loc an-expr)) (second (stx-e an-expr))) (stx-loc an-expr))
                    ;;(datum->stx 'true (stx-loc (first clauses)))
                    (second (stx-e (first clauses)))))]
         [(empty? (rest clauses))
          (loop (reverse questions/rev) 
                (reverse answers/rev) 
                (first (stx-e (first clauses)))
                (second (stx-e (first clauses))))]
         [else
          (process-clauses (rest clauses)
                           (cons (first (stx-e (first clauses))) questions/rev) 
                           (cons (second (stx-e (first clauses))) answers/rev))]))]
    (cond
      [(stx-begins-with? an-expr 'case)
       (process-clauses (rest (rest (stx-e an-expr))) empty empty)]
      [else
       (syntax-error (format "Not a case clause: ~s" (stx-e an-expr))
                     an-expr)])))



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
  (cond
    [(= n 0)
     a-list]
    [else
     (list-tail (rest a-list)
                (sub1 n))]))

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



(define (symbol-stx? x)
  (and (stx? x)
       (symbol? (stx-e x))))



(provide/contract [program? (any/c . -> . boolean?)]
                  [expression? (any/c . -> . boolean?)]
                  [defn? (any/c . -> . boolean?)]
                  [test-case? (any/c . -> . boolean?)]
                  [library-require? (any/c . -> . boolean?)]
                  [take ((listof any/c) number? . -> . (listof any/c))]
                  [list-tail ((listof any/c) number? . -> . (listof any/c))]
                  [remove-leading-whitespace (string? . -> . string?)]
                  [identifier->munged-java-identifier (symbol? . -> . symbol?)]
                  [desugar-cond (stx? . -> . stx?)]
                  [desugar-case (stx? . -> . stx?)]
                  [range (number? . -> . (listof number?))]
                  
                  [case-analyze-definition (any/c 
                                            (stx? (listof symbol-stx?) stx? . -> . any)
                                            (stx? any/c . -> . any)
                                            (stx? (listof symbol-stx?) . -> . any)
                                            . -> . any)]
                  [string-join ((listof string?) string? . -> . string?)])