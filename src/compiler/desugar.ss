#lang s-exp "lang.ss"

(require "helpers.ss")
(require "stx.ss")
(require "pinfo.ss")



;; desugar-program: program pinfo -> (list program pinfo)
(define (desugar-program a-program a-pinfo)
  (local [
          ;; desugar-program-element: program-element pinfo -> (list program-element pinfo)
          (define (desugar-program-element an-element a-pinfo)
            (cond
              [(defn? an-element)
               (desugar-defn an-element a-pinfo)]
              [(test-case? an-element)
               (desugar-test-case an-element a-pinfo)]
              [(library-require? an-element)
               (list an-element a-pinfo)]
              [(expression? an-element)
               (desugar-expression an-element a-pinfo)]))
          
          ;; desugar-defn: defn pinfo -> (list defn pinfo)
          (define (desugar-defn a-defn a-pinfo)
            (local [(define define-stx (first (stx-e a-defn)))]
              (case-analyze-definition a-defn
                                       (lambda (id args body) 
                                         (local [(define subexpr+pinfo (desugar-expression body a-pinfo))]
                                           (list (make-stx:list (list define-stx
                                                                      (make-stx:list (cons id args)
                                                                                     (stx-loc a-defn))
                                                                      (first subexpr+pinfo))
                                                                (stx-loc a-defn))
                                                 (second subexpr+pinfo))))
                                       (lambda (id body) 
                                         (local [(define subexpr+pinfo (desugar-expression body a-pinfo))]
                                           (list (make-stx:list (list define-stx
                                                                      id
                                                                      (first subexpr+pinfo))
                                                                (stx-loc a-defn))
                                                 (second subexpr+pinfo))))
                                       (lambda (id fields) 
                                         ;; FIXME: extend the environment with the
                                         ;; structure identifiers here!
                                         (list a-defn a-pinfo)))))
          
          
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
                 
          
          ;; desugar-test-case: test-case-stx pinfo -> (list test-case-stx pinfo)
          (define (desugar-test-case a-test-case a-pinfo)
            (local [(define test-symbol-stx (first (stx-e a-test-case)))
                    (define test-exprs (rest (stx-e a-test-case)))
                    (define desugared-exprs+pinfo (desugar-expressions test-exprs a-pinfo))]
              (list (make-stx:list (cons test-symbol-stx
                                         (first desugared-exprs+pinfo))
                                   (stx-loc a-test-case))
                    (second desugared-exprs+pinfo))))
          
          
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
              
              
              ;; (local ([define ...] ...) body)
              [(stx-begins-with? expr 'local)
               (local [(define local-symbol-stx (first (stx-e expr)))
                       (define defns (stx-e (second (stx-e expr))))
                       (define body (third (stx-e expr)))
                       
                       (define desugared-defns+pinfo (desugar-program defns pinfo))
                       (define desugared-body+pinfo (desugar-expression body (second desugared-defns+pinfo)))]
                 (list (make-stx:list (list local-symbol-stx
                                            (make-stx:list (first desugared-defns+pinfo)
                                                           (stx-loc (second (stx-e expr))))
                                            (first desugared-body+pinfo))
                                      (stx-loc expr))
                       (pinfo-update-env (second desugared-body+pinfo)
                                         (pinfo-env pinfo))))]
              
              ;; (begin ...)
              [(stx-begins-with? expr 'begin)
               (local [(define begin-symbol-stx (first (stx-e expr)))
                       (define exprs (rest (stx-e expr)))
                       (define desugared-exprs+pinfo (desugar-expressions exprs pinfo))]
                 (list (make-stx:list (cons begin-symbol-stx
                                            (first desugared-exprs+pinfo))
                                      (stx-loc expr))
                       (second desugared-exprs+pinfo)))]

              ;; (set! identifier value)
              [(stx-begins-with? expr 'set!)
               (local [(define set-symbol-stx (first (stx-e expr)))
                       (define id (second (stx-e expr)))
                       (define value (third (stx-e expr)))
                       (define desugared-value+pinfo (desugar-expression value pinfo))]
                 (list (make-stx:list (list set-symbol-stx
                                            id
                                            (first desugared-value+pinfo))
                                      (stx-loc expr))
                       (second desugared-value+pinfo)))]
              
              
              ;; (if test consequent alternative)
              [(stx-begins-with? expr 'if)
               (local [(define if-symbol-stx (first (stx-e expr)))
                       (define exprs (rest (stx-e expr)))
                       (define desugared-exprs+pinfo (desugar-expressions exprs pinfo))]
                 (list (make-stx:list (cons if-symbol-stx
                                            (first desugared-exprs+pinfo))
                                      (stx-loc expr))
                       (second desugared-exprs+pinfo)))]
              
              
              ;; (and exprs ...)
              [(stx-begins-with? expr 'and)
               (local [(define and-symbol-stx (first (stx-e expr)))
                       (define exprs (rest (stx-e expr)))
                       (define desugared-exprs+pinfo (desugar-expressions exprs pinfo))]
                 (list (make-stx:list (cons and-symbol-stx
                                            (first desugared-exprs+pinfo))
                                      (stx-loc expr))
                       (second desugared-exprs+pinfo)))]

              ;; (or exprs ...)
              [(stx-begins-with? expr 'or)
               (local [(define or-symbol-stx (first (stx-e expr)))
                       (define exprs (rest (stx-e expr)))
                       (define desugared-exprs+pinfo (desugar-expressions exprs pinfo))]
                 (list (make-stx:list (cons or-symbol-stx
                                            (first desugared-exprs+pinfo))
                                      (stx-loc expr))
                       (second desugared-exprs+pinfo)))]
              
              ;; (lambda (args ...) body)
              [(stx-begins-with? expr 'lambda)
               (local [(define lambda-symbol-stx (first (stx-e expr)))
                       (define args (second (stx-e expr)))
                       (define body (third (stx-e expr)))
                       (define desugared-body+pinfo (desugar-expression body pinfo))]
                 (list (make-stx:list (list lambda-symbol-stx
                                            args
                                            (first desugared-body+pinfo))
                                      
                                      (stx-loc expr))
                       ;; FIXME: I should extend the pinfo with the identifiers in the arguments.
                       (second desugared-body+pinfo)))]
              
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
                 (list (make-stx:list (first desugared-exprs+pinfo)
                                      (stx-loc expr))
                       (second desugared-exprs+pinfo)))]))]
    (cond 
      [(empty? a-program)
       (list empty a-pinfo)]
      [else
       (local [(define desugared-elt+pinfo (desugar-program-element (first a-program) a-pinfo))
               (define desugared-rest+pinfo (desugar-program (rest a-program) (second desugared-elt+pinfo)))]
         (list (cons (first desugared-elt+pinfo)
                     (first desugared-rest+pinfo))
               (second desugared-rest+pinfo)))])))





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
     (define val-stx (make-stx:atom (second pinfo+val-sym) (stx-loc an-expr)))
     
     (define pinfo+x-sym (pinfo-gensym updated-pinfo-1 'x))
     (define updated-pinfo-2 (first pinfo+x-sym))
     (define x-stx (make-stx:atom (second pinfo+x-sym) (stx-loc an-expr)))     
     
     ;; predicate: stx
     (define predicate
       (datum->stx (list 'lambda (list x-stx)
                         (list 'equal? x-stx val-stx))
                   (stx-loc an-expr)))

     
     ;; loop: (listof stx) (listof stx) stx stx -> stx
     (define (loop list-of-datum answers datum-last answer-last)
       (cond
         [(empty? list-of-datum)
          (if (and (symbol? (stx-e datum-last)) (symbol=? 'else (stx-e datum-last)))
              answer-last
              (make-stx:list (list (make-stx:atom 'if (stx-loc an-expr))
                                   (make-stx:list (list (make-stx:atom 'ormap (stx-loc an-expr))
                                                        predicate
                                                        (make-stx:list (list (make-stx:atom 'quote (stx-loc an-expr))
                                                                             datum-last)
                                                                       (stx-loc an-expr)))
                                                  (stx-loc an-expr))
                                   answer-last
                                   (make-stx:list (list (make-stx:atom 'void (stx-loc an-expr)))
                                                  (stx-loc an-expr)))
                             (stx-loc an-expr)))]
         [else
          (make-stx:list (list (make-stx:atom 'if (stx-loc an-expr))
                               (make-stx:list (list (make-stx:atom 'ormap (stx-loc an-expr))
                                                    predicate
                                                    (make-stx:list (list (make-stx:atom 'quote (stx-loc an-expr))
                                                                         (first list-of-datum))
                                                                   (stx-loc an-expr)))
                                              (stx-loc an-expr))
                               (first answers)
                               (loop (rest list-of-datum)
                                     (rest answers)
                                     datum-last
                                     answer-last))
                         (stx-loc an-expr))]))]
    (cond
      [(stx-begins-with? an-expr 'case)
       (deconstruct-clauses-with-else (rest (rest (stx-e an-expr)))
                                      (lambda (else-stx)
                                        else-stx)
                                      (lambda (questions answers question-last answer-last)
                                        (list (datum->stx (list 'let (list (list val-stx (second (stx-e an-expr))))
                                                                (loop questions answers question-last answer-last))
                                                          (stx-loc an-expr))
                                              updated-pinfo-2)))]
      [else
       (syntax-error (format "Not a case clause: ~s" (stx-e an-expr))
                     an-expr)])))



;; desugar-cond: stx:list -> (list stx:list pinfo)
;; Translates conds to ifs.
(define (desugar-cond an-expr pinfo)
  (local
    [;; loop: (listof stx) (listof stx) stx stx -> stx
     (define (loop questions answers question-last answer-last)
       (cond
         [(empty? questions)
          (make-stx:list (list (make-stx:atom 'if (stx-loc an-expr))
                               question-last
                               answer-last 
                               (datum->stx '(error 'cond "Fell out of cond")
                                           (stx-loc an-expr)))
                         (stx-loc an-expr))]

         [else
          (make-stx:list (list (make-stx:atom 'if (stx-loc an-expr))
                               (first questions)
                               (first answers)
                               (loop (rest questions)
                                     (rest answers)
                                     question-last
                                     answer-last))
                         (stx-loc an-expr))]))]
    (cond
      [(stx-begins-with? an-expr 'cond)
       (deconstruct-clauses-with-else (rest (stx-e an-expr))
                                      (lambda (else-stx)
                                        (make-stx:atom 'true (stx-loc else-stx)))
                                      (lambda (questions answers question-last answer-last)
                                        (list (loop questions answers question-last answer-last)
                                              pinfo)))]
      [else
       (syntax-error (format "Not a cond clause: ~s" (stx-e an-expr))
                     an-expr)])))


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
              (syntax-error "else clause should be the last, but there's another clause after it" (first clauses))
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
            (make-stx:list (list (make-stx:atom 'lambda (stx-loc a-stx))
                                 (make-stx:list ids (stx-loc a-stx))
                                 body-stx)
                           (stx-loc a-stx)))]    
    (list (make-stx:list (cons new-lambda-stx vals)
                         (stx-loc a-stx))
          pinfo)))


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
               (make-stx:list (list (make-stx:atom 'let (stx-loc (first clauses)))
                                    (make-stx:list (list (first clauses))
                                                   (stx-loc (first clauses)))
                                    (loop (rest clauses)))
                              (stx-loc (first clauses)))]))]    
    (list (loop (stx-e clauses-stx))
          pinfo)))




(provide/contract
 [desugar-program (program? pinfo? . -> . (list/c program? pinfo?))])