#lang s-exp "lang.ss"

(require "helpers.ss")
(require "pinfo.ss")
(require "env.ss")
(require "modules.ss")
(require "../collects/runtime/stx.ss")
(require "../collects/runtime/error-struct.ss")





;; desugar-program: program pinfo -> (list program pinfo)
;;
;; FIX BUG: user must not be allowed to rebind any of the primitive keyword names
(define (desugar-program a-program a-pinfo)
  (local [
          
          ;; reorder-tests-to-end: program (listof program-element) (listof program-element) -> program
          ;; Helper to reorder the test case special forms to the end of the program.
          (define (reorder-tests-to-end a-program program/rev tests/rev)
            (cond
              [(empty? a-program)
               (append (reverse program/rev) (reverse tests/rev))]
              [(test-case? (first a-program))
               (reorder-tests-to-end (rest a-program) 
                                     program/rev
                                     (cons (first a-program)
                                           tests/rev))]
              [else
               (reorder-tests-to-end (rest a-program)
                                     (cons (first a-program) program/rev)
                                     tests/rev)]))
          
          ;; desugar-program-element: program-element pinfo -> (list (listof program-element) pinfo)
          (define (desugar-program-element an-element a-pinfo)
            (cond
              [(defn? an-element)
               (desugar-defn an-element a-pinfo)]
              [(library-require? an-element)
               (list (list an-element) a-pinfo)]
              [(provide-statement? an-element)
               (list (list an-element) a-pinfo)]
              [(test-case? an-element)
               (desugar-test-case an-element a-pinfo)]
              [(provide/contract-statement? an-element)
               (desugar-provide/contract an-element a-pinfo)]
              [(expression? an-element)
               (local [(define expr+pinfo (desugar-expression an-element a-pinfo))]
                 (list (list (first expr+pinfo))
                       (second expr+pinfo)))]))
          
          
          
          ;; desugar-defn: defn pinfo -> (list (listof defn) pinfo)
          (define (desugar-defn a-defn a-pinfo)
            (local [(define define-stx (first (stx-e a-defn)))]
              (case-analyze-definition a-defn
                                       (lambda (id args body) 
                                         (begin
                                           (check-duplicate-identifiers! (cons id args))   
                                           (local [(define subexpr+pinfo (desugar-expression body a-pinfo))]
                                             (list (list (datum->stx #f (list define-stx
                                                                              (datum->stx #f (cons id args)
                                                                                          (stx-loc a-defn))
                                                                              (first subexpr+pinfo))
                                                                     (stx-loc a-defn)))
                                                   (second subexpr+pinfo)))))
                                       (lambda (id body) 
                                         (local [(define subexpr+pinfo (desugar-expression body a-pinfo))]
                                           (list (list (datum->stx #f (list define-stx
                                                                            id
                                                                            (first subexpr+pinfo))
                                                                   (stx-loc a-defn)))
                                                 (second subexpr+pinfo))))
                                       (lambda (id fields) 
                                         ;; FIXME: extend the environment with the
                                         ;; structure identifiers here!
                                         (list (list a-defn) a-pinfo)))))
          
          
          ;; desugar-expressions: (listof expr) pinfo -> (list (listof expr) pinfo)
          (define (desugar-expressions exprs pinfo)
            (cond
              [(empty? exprs)
               (list empty pinfo)]
              [else
               (local [(define first-desugared+pinfo 
                         (desugar-expression (first exprs) pinfo))
                       (define rest-desugared+pinfo 
                         (desugar-expressions (rest exprs) 
                                              (second first-desugared+pinfo)))]
                 (list (cons (first first-desugared+pinfo)
                             (first rest-desugared+pinfo))
                       (second rest-desugared+pinfo)))]))
          
          
          ;; thunkify-stx: stx -> stx
          ;; Wraps a thunk around a syntax.
          (define (thunkify-stx an-stx)
            (datum->stx #f 
                        (list 'lambda (list)
                              an-stx)
                        (stx-loc an-stx)))
          
          
          ;; check-length!: stx -> void
          (define (check-test-case-length! stx n error-msg)
            (cond [(not (= n (length (stx-e stx))))
                   (raise (make-moby-error 
                           (stx-loc stx)
                           (make-moby-error-type:generic-syntactic-error
                            error-msg
                            (list))))]
                  [else
                   (void)]))
          
          
          ;; desugar-test-case: test-case-stx pinfo -> (list (listof test-case-stx) pinfo)
          ;; Translates use of a test case form to use of the test case toplevel functions.
          ;; We transform each expression to a thunk, and provide the test case function
          ;; the locations of all expressions as another argument.
          (define (desugar-test-case a-test-case a-pinfo)
            (local [(define test-symbol-stx (first (stx-e a-test-case)))
                    (define test-exprs (map thunkify-stx (rest (stx-e a-test-case))))
                    
                    (define desugared-exprs+pinfo (desugar-expressions test-exprs a-pinfo))]
              (begin
                (cond [(stx-begins-with? a-test-case 'check-expect)
                       (check-test-case-length! a-test-case 3
                                                "check-expect requires two expressions.  Try (check-expect test expected).")]
                      [(stx-begins-with? a-test-case 'EXAMPLE)
                       (check-test-case-length! a-test-case 3
                                                "EXAMPLE requires two expressions.  Try (EXAMPLE test expected).")]
                      
                      [(stx-begins-with? a-test-case 'check-within)
                       (check-test-case-length! a-test-case 4
                                                "check-within requires three expressions.  Try (check-within test expected range).")]
                      [(stx-begins-with? a-test-case 'check-error)
                       (check-test-case-length! a-test-case 3
                                                "check-error requires two expressions.  Try (check-error test message).")]
                      [else
                       (void)])
                
                (list (list (datum->stx #f (cons test-symbol-stx
                                                 (first desugared-exprs+pinfo))
                                        (stx-loc a-test-case)))
                      (second desugared-exprs+pinfo)))))
          
          
          ;; desugar-expression/expr+pinfo: (list expr pinfo) -> (list expr pinfo)
          (define (desugar-expression/expr+pinfo expr+pinfo)
            (desugar-expression (first expr+pinfo)
                                (second expr+pinfo)))
          
          
          ;; desugar-expression: expr pinfo -> (list expr pinfo)
          (define (desugar-expression expr pinfo)
            (cond
              
              ;; (cond ...)
              [(stx-begins-with? expr 'cond)
               (desugar-expression/expr+pinfo (desugar-cond expr pinfo))]
              
              ;; (case val-expr [quoted-expr expr] ...)
              [(stx-begins-with? expr 'case)
               (desugar-expression/expr+pinfo (desugar-case expr pinfo))]
              
              ;; (let ([id val] ...) ...)
              [(stx-begins-with? expr 'let)
               (desugar-expression/expr+pinfo (desugar-let expr pinfo))]
              
              ;; (let* ([id val] ...) ...)
              [(stx-begins-with? expr 'let*)
               (desugar-expression/expr+pinfo (desugar-let* expr pinfo))]
              
              ;; (let* ([id val] ...) ...)
              [(stx-begins-with? expr 'letrec)
               (desugar-expression/expr+pinfo (desugar-letrec expr pinfo))]
              
              ;; (quasiquote x)
              [(or (stx-begins-with? expr 'quasiquote)
                   (stx-begins-with? expr 'unquote)
                   (stx-begins-with? expr 'quasiquote))
               (desugar-expression/expr+pinfo (desugar-quasiquote expr pinfo))]
              
              ;; (local ([define ...] ...) body)
              [(stx-begins-with? expr 'local)
               (begin
                 (check-single-body-stx! (rest (rest (stx-e expr))) expr)
                 (local [(define local-symbol-stx (first (stx-e expr)))
                         (define defns (stx-e (second (stx-e expr))))
                         (define body (third (stx-e expr)))
                         
                         (define desugared-defns+pinfo (desugar-program defns pinfo))
                         (define desugared-body+pinfo (desugar-expression body (second desugared-defns+pinfo)))]
                   (list (datum->stx #f (list local-symbol-stx
                                              (datum->stx #f (first desugared-defns+pinfo)
                                                          (stx-loc (second (stx-e expr))))
                                              (first desugared-body+pinfo))
                                     (stx-loc expr))
                         (pinfo-update-env (second desugared-body+pinfo)
                                           (pinfo-env pinfo)))))]
              
              ;; (begin ...)
              [(stx-begins-with? expr 'begin)
               (local [(define begin-symbol-stx (first (stx-e expr)))
                       (define exprs (rest (stx-e expr)))
                       (define desugared-exprs+pinfo (desugar-expressions exprs pinfo))]
                 (list (datum->stx #f (cons begin-symbol-stx
                                            (first desugared-exprs+pinfo))
                                   (stx-loc expr))
                       (second desugared-exprs+pinfo)))]
              
              ;; (set! identifier value)
              [(stx-begins-with? expr 'set!)
               (local [(define set-symbol-stx (first (stx-e expr)))
                       (define id (second (stx-e expr)))
                       (define value (third (stx-e expr)))
                       (define desugared-value+pinfo (desugar-expression value pinfo))]
                 (list (datum->stx #f (list set-symbol-stx
                                            id
                                            (first desugared-value+pinfo))
                                   (stx-loc expr))
                       (second desugared-value+pinfo)))]
              
              
              ;; (if test consequent alternative)
              [(stx-begins-with? expr 'if)
               (local [(define if-symbol-stx (first (stx-e expr)))
                       (define exprs (rest (stx-e expr)))
                       (define desugared-exprs+pinfo (desugar-expressions exprs pinfo))]
                 (list (datum->stx #f (cons if-symbol-stx
                                            (first desugared-exprs+pinfo))
                                   (stx-loc expr))
                       (second desugared-exprs+pinfo)))]
              
              
              ;; (and exprs ...)
              [(stx-begins-with? expr 'and)
               (local [(define and-symbol-stx (first (stx-e expr)))
                       (define exprs (rest (stx-e expr)))
                       (define desugared-exprs+pinfo (desugar-expressions exprs pinfo))]
                 (list (datum->stx #f (cons and-symbol-stx
                                            (first desugared-exprs+pinfo))
                                   (stx-loc expr))
                       (second desugared-exprs+pinfo)))]
              
              ;; (or exprs ...)
              [(stx-begins-with? expr 'or)
               (local [(define or-symbol-stx (first (stx-e expr)))
                       (define exprs (rest (stx-e expr)))
                       (define desugared-exprs+pinfo (desugar-expressions exprs pinfo))]
                 (list (datum->stx #f (cons or-symbol-stx
                                            (first desugared-exprs+pinfo))
                                   (stx-loc expr))
                       (second desugared-exprs+pinfo)))]
              
              ;; (lambda (args ...) body)
              [(stx-begins-with? expr 'lambda)
               (begin
                 (check-single-body-stx! (rest (rest (stx-e expr))) expr)
                 (local [(define lambda-symbol-stx (first (stx-e expr)))
                         (define args (second (stx-e expr)))
                         (define body (third (stx-e expr)))
                         (define desugared-body+pinfo (desugar-expression body pinfo))]
                   (list (datum->stx #f (list lambda-symbol-stx
                                              args
                                              (first desugared-body+pinfo))
                                     (stx-loc expr))
                         ;; FIXME: I should extend the pinfo with the identifiers in the arguments.
                         (second desugared-body+pinfo))))]
              
              ;; Numbers
              [(number? (stx-e expr))
               (list expr pinfo)]
              
              ;; Strings
              [(string? (stx-e expr))
               (list expr pinfo)]
              
              ;; Literal booleans
              [(boolean? (stx-e expr))
               (list expr pinfo)]
              
              ;; Characters
              [(char? (stx-e expr))
               (list expr pinfo)]
              
              ;; Identifiers
              [(symbol? (stx-e expr))
               (list expr pinfo)]
              
              ;; Quoted datums
              [(stx-begins-with? expr 'quote)
               (list expr pinfo)]
              
              ;; Function call/primitive operation call
              [(pair? (stx-e expr))
               (local [(define exprs (stx-e expr))
                       (define desugared-exprs+pinfo (desugar-expressions exprs pinfo))]
                 (list (datum->stx #f (first desugared-exprs+pinfo)
                                   (stx-loc expr))
                       (second desugared-exprs+pinfo)))]
              [else
               (raise 
                (make-moby-error (stx-loc expr)
                                 (make-moby-error-type:generic-syntactic-error
                                  (format "Unable to desugar ~s" (stx->datum expr))
                                  (list))))]))
          
          ;; processing-loop: program pinfo -> (list program pinfo)
          (define (processing-loop a-program a-pinfo)
            (cond 
              [(empty? a-program)
               (list empty a-pinfo)]
              [else
               (local [(define desugared-elts+pinfo
                         (desugar-program-element (first a-program) a-pinfo))
                       (define desugared-rest+pinfo
                         (processing-loop (rest a-program) 
                                          (second desugared-elts+pinfo)))]
                 (list (append (first desugared-elts+pinfo)
                               (first desugared-rest+pinfo))
                       (second desugared-rest+pinfo)))]))]
    
    (processing-loop (reorder-tests-to-end a-program empty empty)
                     a-pinfo)))






;; desugar-case: stx:list -> (list stx:list pinfo)
;; translates case to if.
;;
;; KNOWN BUG: this doesn't do a let binding of the value that's being
;; analyzed, so the value is going to be evaluated again and again.
;; Before we fix this bug, I'd like us to have something like syntax-case and
;; helpers for building syntax objects, because it's really painful
;; to build syntax expanders without linguistic support.
(define (desugar-case an-expr pinfo)
  (local
    [(define pinfo+val-sym (pinfo-gensym pinfo 'val))
     (define updated-pinfo-1 (first pinfo+val-sym))
     (define val-stx (datum->stx #f (second pinfo+val-sym) (stx-loc an-expr)))
     
     (define pinfo+x-sym (pinfo-gensym updated-pinfo-1 'x))
     (define updated-pinfo-2 (first pinfo+x-sym))
     (define x-stx (datum->stx #f (second pinfo+x-sym) (stx-loc an-expr)))     
     
     ;; predicate: stx
     (define predicate
       (datum->stx #f 
                   (list 'lambda (list x-stx)
                         (list 'equal? x-stx val-stx))
                   (stx-loc an-expr)))
     
     
     ;; loop: (listof stx) (listof stx) stx stx -> stx
     (define (loop list-of-datum answers datum-last answer-last)
       (cond
         [(empty? list-of-datum)
          (if (and (symbol? (stx-e datum-last)) (symbol=? 'else (stx-e datum-last)))
              answer-last
              (datum->stx #f (list (datum->stx #f 'if (stx-loc an-expr))
                                   (datum->stx #f (list (datum->stx #f 'ormap (stx-loc an-expr))
                                                        predicate
                                                        (datum->stx #f (list (datum->stx #f 'quote (stx-loc an-expr))
                                                                             datum-last)
                                                                    (stx-loc an-expr)))
                                               (stx-loc an-expr))
                                   answer-last
                                   (datum->stx #f (list (datum->stx #f 'void (stx-loc an-expr)))
                                               (stx-loc an-expr)))
                          (stx-loc an-expr)))]
         [else
          (cond
            [(not (list? (stx-e (first list-of-datum))))
             (raise (make-moby-error (stx-loc (first list-of-datum))
                                     (make-moby-error-type:generic-syntactic-error
                                      (format "case needs a list of values for each clause, but sees ~s instead"
                                              (stx->datum (first list-of-datum)))
                                      (list))))]
            [else
             (datum->stx #f (list (datum->stx #f 'if (stx-loc an-expr))
                                  (datum->stx #f (list (datum->stx #f 'ormap (stx-loc an-expr))
                                                       predicate
                                                       (datum->stx #f (list (datum->stx #f 'quote (stx-loc an-expr))
                                                                            (first list-of-datum))
                                                                   (stx-loc an-expr)))
                                              (stx-loc an-expr))
                                  (first answers)
                                  (loop (rest list-of-datum)
                                        (rest answers)
                                        datum-last
                                        answer-last))
                         (stx-loc an-expr))])]))]
    (cond
      [(stx-begins-with? an-expr 'case)
       (deconstruct-clauses-with-else (rest (rest (stx-e an-expr)))
                                      (lambda (else-stx)
                                        else-stx)
                                      (lambda (questions answers question-last answer-last)
                                        (list (datum->stx #f 
                                                          (list 'let (list (list val-stx (second (stx-e an-expr))))
                                                                (loop questions answers question-last answer-last))
                                                          (stx-loc an-expr))
                                              updated-pinfo-2)))]
      [else
       (raise (make-moby-error (stx-loc an-expr)
                               (make-moby-error-type:generic-syntactic-error
                                (format "Not a case clause: ~s" (stx->datum an-expr))
                                (list))))])))




;; make-cond-exhausted-expression: loc -> stx
(define (make-cond-exhausted-expression a-loc)
  (tag-application-operator/module
   (datum->stx #f `(throw-cond-exhausted-error (quote ,(Loc->sexp a-loc))) a-loc)
   'moby/runtime/kernel/misc))


;; tag-application-operator/module: stx module-name -> stx
;; Adjust the lexical context of the operator so it refers to the environment of a particular module.
(define (tag-application-operator/module an-application-stx a-module-name)
  (local [(define an-id-stx (first (stx-e an-application-stx)))
          (define operands (rest (stx-e an-application-stx)))]
    (datum->stx an-application-stx
                `(,(stx-update-context 
                    an-id-stx
                    (extend-env/module-binding empty-env
                                               (default-module-resolver a-module-name)))
                  ,@operands)
                (stx-loc an-application-stx))))


;; desugar-cond: stx:list -> (list stx:list pinfo)
;; Translates conds to ifs.
(define (desugar-cond an-expr pinfo)
  (local
    [
     (define cond-clauses (rest (stx-e an-expr)))
     
     (define (check-clause-structures!)
       (for-each (lambda (a-clause)
                   (cond [(not (pair? (stx-e a-clause)))
                          (raise (make-moby-error (stx-loc a-clause)
                                                  (make-moby-error-type:conditional-malformed-clause)))]
                         [(< (length (stx-e a-clause)) 2)
                          (raise (make-moby-error (stx-loc a-clause)
                                                  (make-moby-error-type:conditional-clause-too-few-elements)))]
                         [(> (length (stx-e a-clause)) 2)
                          (raise (make-moby-error (stx-loc a-clause)
                                                  (make-moby-error-type:conditional-clause-too-many-elements)))]
                         [else
                          (void)]))
                 cond-clauses))
     
     
     ;; loop: (listof stx) (listof stx) stx stx -> stx
     (define (loop questions answers question-last answer-last)
       (cond
         [(empty? questions)
          (datum->stx #f `(if ,question-last 
                              ,answer-last
                              ,(make-cond-exhausted-expression (stx-loc an-expr)))
                      (stx-loc an-expr))]
         
         [else
          (datum->stx #f `(if ,(first questions)
                              ,(first answers)
                              ,(loop (rest questions)
                                     (rest answers)
                                     question-last
                                     answer-last))
                      (stx-loc an-expr))]))]
    (cond
      [(empty? cond-clauses)
       (raise (make-moby-error (stx-loc an-expr)
                               (make-moby-error-type:conditional-missing-question-answer)))]
      [else
       (begin
         (check-clause-structures!)
         (deconstruct-clauses-with-else cond-clauses
                                        (lambda (else-stx)
                                          (datum->stx #f 'true (stx-loc else-stx)))
                                        (lambda (questions answers question-last answer-last)
                                          (list (loop questions answers question-last answer-last)
                                                pinfo))))])))



;; deconstruct-clauses-with-else: (listof stx) (stx -> stx) ((listof stx) (listof stx) stx stx -> X) -> X
;; Helper for functions that need to destruct a list of 
;; clauses of the form ([question answer] ... [else answer-last]).
(define (deconstruct-clauses-with-else clauses else-replacement-f f)
  (local 
    [;; process-clauses: (listof stx) (listof stx) (listof stx) -> X
     (define (process-clauses clauses questions/rev answers/rev)
       (cond
         [(stx-begins-with? (first clauses) 'else)
          (if (not (empty? (rest clauses)))
              (raise (make-moby-error (stx-loc (first clauses))
                                      (make-moby-error-type:generic-syntactic-error
                                       "else clause should be the last, but there's another clause after it" 
                                       (list)
                                       )))
              (f (reverse questions/rev) 
                 (reverse answers/rev) 
                 (else-replacement-f (first (stx-e (first clauses))))
                 (second (stx-e (first clauses)))))]
         
         [(empty? (rest clauses))
          (f (reverse questions/rev)
             (reverse answers/rev) 
             (first (stx-e (first clauses)))
             (second (stx-e (first clauses))))]
         [else
          (process-clauses (rest clauses)
                           (cons (first (stx-e (first clauses))) questions/rev) 
                           (cons (second (stx-e (first clauses))) answers/rev))]))]
    (process-clauses clauses empty empty)))




;; desugar-let: expr-stx -> (list expr-stx pinfo)
;; Given a let expression, translates it to the equivalent use of
;; a lambda application.
(define (desugar-let a-stx pinfo)
  (local [(define clauses-stx (second (stx-e a-stx)))
          (define body-stx (third (stx-e a-stx)))
          (define ids (map (lambda (clause)
                             (first (stx-e clause)))
                           (stx-e clauses-stx)))
          (define vals (map (lambda (clause)
                              (second (stx-e clause)))
                            (stx-e clauses-stx)))
          
          (define new-lambda-stx
            (datum->stx #f (list (datum->stx #f 'lambda (stx-loc a-stx))
                                 (datum->stx #f ids (stx-loc a-stx))
                                 body-stx)
                        (stx-loc a-stx)))]    
    (begin
      (check-single-body-stx! (rest (rest (stx-e a-stx))) a-stx)
      (check-duplicate-identifiers! (map (lambda (a-clause)
                                           (first (stx-e a-clause)))
                                         (stx-e clauses-stx)))      
      (list (datum->stx #f (cons new-lambda-stx vals)
                        (stx-loc a-stx))
            pinfo))))


;; desugar-let*: expr-stx -> expr-stx
;; Desugars let* into a nested bunch of let expressions.
(define (desugar-let* a-stx pinfo)
  (local [(define clauses-stx (second (stx-e a-stx)))
          (define body-stx (third (stx-e a-stx)))
          
          ;; loop: (listof stx) -> stx
          (define (loop clauses)
            (cond
              [(empty? clauses)
               body-stx]
              [else
               (datum->stx #f (list (datum->stx #f 'let (stx-loc (first clauses)))
                                    (datum->stx #f (list (first clauses))
                                                (stx-loc (first clauses)))
                                    (loop (rest clauses)))
                           (stx-loc (first clauses)))]))]    
    (begin
      (check-single-body-stx! (rest (rest (stx-e a-stx))) a-stx)
      (list (loop (stx-e clauses-stx))
            pinfo))))


;; desugar-letrec: stx pinfo -> (list stx pinfo)
;; Letrec will be desugared into local.
(define (desugar-letrec a-stx pinfo)
  (local [(define clauses-stx (second (stx-e a-stx)))
          (define body-stx (third (stx-e a-stx)))
          (define define-clauses
            (map (lambda (a-clause)
                   (local [(define name (first (stx-e a-clause)))
                           (define val (second (stx-e a-clause)))]
                     (datum->stx #f (list 'define name val)
                                 (stx-loc a-clause))))
                 (stx-e clauses-stx)))]
    (begin
      (check-single-body-stx! (rest (rest (stx-e a-stx))) a-stx)
      (check-duplicate-identifiers! (map (lambda (a-clause) (first (stx-e a-clause)))
                                         (stx-e clauses-stx)))
      (list (datum->stx #f 
                        (list 'local define-clauses body-stx)
                        (stx-loc a-stx))
            pinfo))))


;; desugar-quasiquote: stx pinfo -> (list stx pinfo)
(define (desugar-quasiquote a-stx pinfo)
  (local [;; handle-quoted: stx depth -> stx
          (define (handle-quoted a-stx depth)
            (cond
              [(stx:list? a-stx)
               (cond [(stx-begins-with? a-stx 'quasiquote)
                      (begin 
                        (check-single-body-stx! (rest (stx-e a-stx)) a-stx)
                        (cond
                          [(> depth 0)
                           (datum->stx #f (list 'list (list 'quote (first (stx-e a-stx)))
                                                (handle-quoted (second (stx-e a-stx))
                                                               (add1 depth)))
                                       (stx-loc a-stx))]
                          [else
                           (datum->stx #f (handle-quoted (second (stx-e a-stx))
                                                         (add1 depth))
                                       (stx-loc a-stx))]))]
                     
                     [(stx-begins-with? a-stx 'unquote)
                      (begin
                        (check-single-body-stx! (rest (stx-e a-stx)) a-stx)
                        (cond
                          [(> depth 1)
                           (datum->stx #f (list 'list (list 'quote (first (stx-e a-stx)))
                                                (handle-quoted (second (stx-e a-stx)) 
                                                               (sub1 depth)))
                                       (stx-loc a-stx))]
                          [(= depth 1)
                           (second (stx-e a-stx))]
                          [else
                           (raise (make-moby-error (stx-loc a-stx)
                                                   (make-moby-error-type:generic-syntactic-error
                                                    "misuse of a comma or 'unquote, not under a quasiquoting backquote" 
                                                    (list)
                                                    )))]))]
                     
                     [(stx-begins-with? a-stx 'unquote-splicing)
                      (cond
                        [(> depth 1)
                         (datum->stx #f (list 'list (list 'quote (first (stx-e a-stx)))
                                              (handle-quoted (second (stx-e a-stx)) 
                                                             (sub1 depth)))
                                     (stx-loc a-stx))]
                        [(= depth 1)
                         (raise (make-moby-error (stx-loc a-stx)
                                                 (make-moby-error-type:generic-syntactic-error
                                                  "misuse of ,@ or unquote-splicing within a quasiquoting backquote" 
                                                  (list))))]
                        
                        [else
                         (raise (make-moby-error (stx-loc a-stx)
                                                 (make-moby-error-type:generic-syntactic-error
                                                  "misuse of a ,@ or unquote-splicing, not under a quasiquoting backquote"
                                                  (list))))])]
                     
                     [else
                      (datum->stx #f (cons 'append 
                                           (map 
                                            ;; (stx -> (listof stx))
                                            (lambda (s) 
                                              (cond
                                                [(stx-begins-with? s 'quasiquote)
                                                 (list 'list (handle-quoted s depth))]
                                                
                                                [(stx-begins-with? s 'unquote)
                                                 (list 'list (handle-quoted s depth))]
                                                
                                                [(stx-begins-with? s 'unquote-splicing)
                                                 (cond
                                                   [(> depth 1)
                                                    (list 'list (handle-quoted s depth))]
                                                   [(= depth 1)
                                                    (begin
                                                      (check-single-body-stx! (rest (stx-e s)) s)
                                                      (second (stx-e s)))]
                                                   [else
                                                    (make-moby-error 
                                                     (stx-loc a-stx)
                                                     (make-moby-error-type:generic-syntactic-error
                                                      "misuse of ,@ or unquote-splicing within a quasiquoting backquote" 
                                                      (list)))])]
                                                
                                                [else
                                                 (list 'list (handle-quoted s depth))]))
                                            (stx-e a-stx)))
                                  (stx-loc a-stx))])]
              [else
               (cond
                 [(> depth 0)
                  (datum->stx #f (list 'quote a-stx) (stx-loc a-stx))]
                 [else
                  a-stx])]))]
    
    (list (handle-quoted a-stx 0) 
          pinfo)))



;; provide/contract-statement: stx -> boolean
(define (provide/contract-statement? a-stx)
  (stx-begins-with? a-stx 'provide/contract))


;; replace-provide/contracts: stx pinfo -> (list (listof stx) pinfo)
;; Rewrites all the provide/contracts to regular provides, since we don't
;; yet have a contract system in place.
(define (desugar-provide/contract a-provide-contract a-pinfo)
  (cond [(stx-begins-with? a-provide-contract 'provide/contract)
         (list (list (datum->stx #f 
                                 `(provide ,@(map convert-provide/contract-clause 
                                                  (rest (stx-e a-provide-contract)))) 
                                 (stx-loc a-provide-contract)))
               a-pinfo)]
        [else
         (list (list a-provide-contract) 
               a-pinfo)]))


;; convert-provide/contract-clause: stx -> stx
(define (convert-provide/contract-clause a-clause)
  (cond
    [(stx-begins-with? a-clause 'struct)
     ;; FIXME: Check all syntactic conditions for well-formedness!
     (datum->stx #f 
                 `(struct-out ,(first (rest (stx-e a-clause))))
                 (stx-loc a-clause))]
    [(list? (stx-e a-clause))
     ;; FIXME: we're ignoring the contract.
     (first (stx-e a-clause))]
    [(symbol? (stx-e a-clause))
     a-clause]
    [else
     (raise (make-moby-error (stx-loc a-clause)
                             (make-moby-error-type:generic-syntactic-error 
                              (format "provide/contract: ~s" a-clause)
                              (list))))]))




(provide/contract
 [desugar-program (program? pinfo? . -> . (list/c program? pinfo?))]) 