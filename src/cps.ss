#lang scheme/base
(require scheme/match
         scheme/list
         "pinfo.ss"
         "env.ss")

;; CPSing intermediate-level programs

#;(define (cps-program a-program an-env)
  ...)

#;(define (cps-definition a-defn an-env a-pinfo)
  ...)

;; cps-expression: expr env -> expr
;; Translates an expression into CPS form.
(define (cps-expression an-expr an-env a-pinfo)
  (match an-expr

    [(list 'local [list defns ...] body)
     'fixme]
    
    [(list 'cond [list questions answers] ... [list 'else answer-last])
     (cps-expression (desugar-cond an-expr) an-env a-pinfo)]
    
    [(list 'cond [list questions answers] ... [list question-last answer-last])
     (cps-expression (desugar-cond an-expr) an-env a-pinfo)]
    
    [(list 'if test consequent alternative)
     (cps-if-expression test consequent alternative an-env a-pinfo)]

    
    [(list 'and expr ...)
     (cps-and-expression expr an-env a-pinfo)]

    
    [(list 'or expr ...)
     (cps-or-expression expr an-env a-pinfo)]
        
    ;; Numbers
    [(? number?)
     `(lambda (k)
        (k ,an-expr))]
    
    ;; Strings
    [(? string?)
     `(lambda (k)
        (k ,an-expr))]
    
    ;; Characters
    [(? char?)
     `(lambda (k)
        (k ,an-expr))]
    
    ;; Identifiers
    [(? symbol?)
     `(lambda (k)
        (k ,an-expr))]

    
    ;; Quoted datum.
    [(list 'quote datum)
     `(lambda (k)
        (k ',an-expr))]

    
    ;; Function call/primitive operation call
    [(list (? symbol? id) exprs ...)
     'fixme]))



(define (cps-if-expression test consequent alternative env a-pinfo)
  (let ([cps-test (cps-expression test env a-pinfo)]
        [cps-consequent (cps-expression consequent env a-pinfo)]
        [cps-alternative (cps-expression alternative env a-pinfo)])
    `(lambda (k)
       (,cps-test (lambda (test-val)
                    (if test-val
                        (,cps-consequent k)
                        (,cps-alternative k)))))))


(define (cps-and-expression conjuncts env a-pinfo)
  (let ([cps-conjuncts (map (lambda (e) (cps-expression e env a-pinfo))
                            conjuncts)])
    (cond [(empty? cps-conjuncts)
           `(lambda (k)
              (k #t))]
          [else
           (let loop ([cps-conjuncts cps-conjuncts])
             (cond
               [(empty? (rest cps-conjuncts))
                `(lambda (k)
                   (,(first cps-conjuncts) k))]
               [else
                `(lambda (k)
                   (,(first cps-conjuncts) 
                    (lambda (v)
                      (if v (,(loop (rest cps-conjuncts)) k)
                          v))))]))])))


(define (cps-or-expression disjuncts env a-pinfo)
  (let ([cps-disjuncts (map (lambda (e) (cps-expression e env a-pinfo))
                            disjuncts)])
    (let loop ([cps-disjuncts cps-disjuncts])
      (cond
        [(empty? cps-disjuncts)
         `(lambda (k)
            (k #f))]
        [else
         `(lambda (k)
            (,(first cps-disjuncts) 
             (lambda (v)
               (if v v (,(loop (rest cps-disjuncts)) k)))))]))))



;; desugar-cond: expr -> expr
;; Translates conds to ifs.
(define (desugar-cond an-expr)
  (match an-expr
    [(list 'cond [list questions answers] ... [list 'else answer-last])
     (let loop ([questions questions]
                [answers answers])
       (cond
         [(empty? questions)
          `,answer-last]
         [else
          `(if ,(first questions) 
               ,(first answers)
               ,(loop (rest questions)
                      (rest answers)))]))]
    
    [(list 'cond [list questions answers] ... [list question-last answer-last])
     (let loop ([questions questions]
                [answers answers])
       (cond
         [(empty? questions)
          `(if ,question-last ,answer-last (error "Fell out of cond"))]
         [else
          `(if ,(first questions) 
               ,(first answers)
               ,(loop (rest questions)
                      (rest answers)))]))]))