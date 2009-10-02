#lang s-exp "lang.ss"

(require "helpers.ss")
(require "stx.ss")



;; desugar-program: program pinfo -> program
(define (desugar-program a-program a-pinfo)
  (local [(define (desugar-program-element an-element)
            (cond
              [(defn? an-element)
               (desugar-defn an-element)]
              [(test-case? an-element)
               (desugar-test-case an-element)]
              [(library-require? an-element)
               an-element]
              [(expression? an-element)
               (desugar-expression an-element)]))
          
          (define (desugar-defn a-defn)
            (local [(define define-stx (first (stx-e a-defn)))]
              (case-analyze-definition a-defn
                                       (lambda (id args body) 
                                         (make-stx:list (list define-stx
                                                              (make-stx:list (cons id args)
                                                                             (stx-loc a-defn))
                                                              (desugar-expression body))
                                                        (stx-loc a-defn)))
                                       (lambda (id body) 
                                         (make-stx:list (list define-stx
                                                              id
                                                              (desugar-expression body))
                                                        (stx-loc a-defn)))
                                       (lambda (id fields) 
                                         a-defn))))

          
          (define (desugar-test-case a-test-case)
            (local [(define test-symbol-stx (first (stx-e a-test-case)))]
              (cond [(stx-begins-with? a-test-case 'check-expect)
                     (make-stx:list (list test-symbol-stx
                                          (desugar-expression (second (stx-e a-test-case)))
                                          (desugar-expression (third (stx-e a-test-case))))
                                    (stx-loc a-test-case))]
                    [(stx-begins-with? a-test-case 'check-within)
                     (make-stx:list (list test-symbol-stx
                                          (desugar-expression (second (stx-e a-test-case)))
                                          (desugar-expression (third (stx-e a-test-case)))
                                          (desugar-expression (fourth (stx-e a-test-case))))
                                    (stx-loc a-test-case))]
                    [(stx-begins-with? a-test-case 'check-error)
                     (make-stx:list (list test-symbol-stx
                                          (desugar-expression (second (stx-e a-test-case)))
                                          (desugar-expression (third (stx-e a-test-case))))
                                    (stx-loc a-test-case))])))
          
          
          (define (desugar-expression expr)
            (cond
              ;; (local ([define ...] ...) body)
              [(stx-begins-with? expr 'local)
               (local [(define defns (stx-e (second (stx-e expr))))
                       (define body (third (stx-e expr)))]
                 (make-stx:list (list (first (stx-e expr))
                                      (make-stx:list (desugar-program defns)
                                                     (stx-loc (second (stx-e expr))))
                                      (desugar-expression body))
                                (stx-loc expr)))]
              
              ;; (begin ...)
              [(stx-begins-with? expr 'begin)
               (local [(define exprs (rest (stx-e expr)))]
                 (make-stx:list (cons (first (stx-e expr))
                                      (map desugar-expression (rest (stx-e expr))))
                                (stx-loc expr)))]
              
              ;; (set! identifier value)
              ;; Attention: it's evaluation doesn't produce an Object
              [(stx-begins-with? expr 'set!)
               (local [(define id (second (stx-e expr)))
                       (define value (third (stx-e expr)))]
                 ...)]
              
              ;; (cond ...)
              [(stx-begins-with? expr 'cond)
               ...]
              
              ;; (if test consequent alternative)
              [(stx-begins-with? expr 'if)
               (local [(define test (second (stx-e expr)))
                       (define consequent (third (stx-e expr)))
                       (define alternative (fourth (stx-e expr)))]
                 ...)]
              
              ;; (case val-expr [quoted-expr expr] ...)
              [(stx-begins-with? expr 'case)
               ...]
              
              ;; (and exprs ...)
              [(stx-begins-with? expr 'and)
               (local [(define exprs (rest (stx-e expr)))]
                 ...)]
              
              ;; (or exprs ...)
              [(stx-begins-with? expr 'or)
               (local [(define exprs (rest (stx-e expr)))]
                 ...)]
              
              ;; (lambda (args ...) body)
              [(stx-begins-with? expr 'lambda)
               (local [(define args (stx-e (second (stx-e expr))))
                       (define body (third (stx-e expr)))]
                 ...)]
              
              ;; Numbers
              [(number? (stx-e expr))
               ...]
              
              ;; Strings
              [(string? (stx-e expr))
               ...]
              
              ;; Literal booleans
              [(boolean? (stx-e expr))
               ...]
              
              ;; Characters
              [(char? (stx-e expr))
               ...]
              
              ;; Identifiers
              [(symbol? (stx-e expr))
               ...]
              
              ;; Quoted datums
              [(stx-begins-with? expr 'quote)
               ...]
              
              ;; Function call/primitive operation call
              [(pair? (stx-e expr))
               (local [(define operator (first (stx-e expr)))
                       (define operands (rest (stx-e expr)))]
                 ...)]))]

    (cond 
      [(empty? a-program)
       empty]
      [else
       (cons (desugar-program-element (first a-program))
             (desugar-program (rest a-program)))])))





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
                                                    (make-stx:list (cons (datum->stx 'list (stx-loc an-expr)) 
                                                                         (stx-e datum-last))
                                                                   (stx-loc an-expr)))
                                              (stx-loc an-expr))
                               answer-last
                               (make-stx:list (list (datum->stx 'void (stx-loc an-expr)))
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
                                                    (make-stx:list (cons (datum->stx 'list (stx-loc an-expr)) 
                                                                         (stx-e (first list-of-datum)))
                                                                   (stx-loc an-expr)))
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
                    (make-stx:list (list (second (stx-e an-expr))) (stx-loc an-expr))
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





;; desugar-let: expr-stx -> expr-stx
;; Given a let expression, translates it to the equivalent use of
;; a lambda application.
(define (desugar-let a-stx)
  (local [(define clauses-stx (second (stx-e a-stx)))
          (define body-stx (third (stx-e a-stx)))
          (define ids (map (lambda (clause)
                             (first (stx-e clause)))
                           (stx-e clauses-stx)))
          (define vals (map (lambda (clause)
                              (second (stx-e clause)))
                            (stx-e clauses-stx)))
          
          (define new-lambda-stx
            (make-stx:list (list 
                            (datum->stx 'lambda (stx-loc a-stx))
                            (make-stx:list ids (stx-loc a-stx))
                            body-stx)
                           (stx-loc a-stx)))]    
    (make-stx:list (cons new-lambda-stx vals)
                   (stx-loc a-stx))))





(provide/contract
 [desugar-program (program? . -> . program?)]
 [desugar-cond (stx? . -> . stx?)]
 [desugar-case (stx? . -> . stx?)]
 [desugar-let (stx? . -> . stx?)])