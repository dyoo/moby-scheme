#lang scheme/base
(require scheme/match
         scheme/list
         "helpers.ss"
         (prefix-in primitive-pinfo: "pinfo.ss")
         (prefix-in primitive-env: "env.ss"))


;; CPSing intermediate-level programs


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; An env collects a set of bindings.
(define-struct env (bindings) #:transparent)
(define empty-env (make-env (make-immutable-hasheq '())))

;; A binding associates a symbol with some value.
(define-struct binding (id) #:transparent)
(define-struct (binding:defined binding) () #:transparent)
(define-struct (binding:primitive binding) () #:transparent)

;; env-extend: env binding -> env
(define (env-extend an-env new-binding)
  (make-env (hash-set (env-bindings an-env) 
                      (binding-id new-binding)
                      new-binding)))

;; env-lookup: env symbol -> (or/c binding #f)
(define (env-lookup an-env name)
  (match an-env
    [(struct env (bindings))
     (hash-ref (env-bindings an-env) name #f)]))

;; Translates the env structure in env.ss to a simpler one here.
(define (translate-primitive-env a-primitive-env)
  (define (translate-primitive-binding a-binding)
    (match a-binding
      [(struct primitive-env:binding:constant (_ _ _))
       (make-binding:defined (primitive-env:binding-id a-binding))]
      [(struct primitive-env:binding:function (_ _ _ _ _ _ cps?))
       (cond [cps?
              (make-binding:defined (primitive-env:binding-id a-binding))]
             [else
              (make-binding:primitive (primitive-env:binding-id a-binding))])]))
  
  (foldl (lambda (k env)
           (env-extend env (translate-primitive-binding
                            (primitive-env:env-lookup a-primitive-env k))))
         empty-env
         (primitive-env:env-keys a-primitive-env)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define default-env
  (translate-primitive-env 
   (primitive-pinfo:pinfo-env
    (primitive-pinfo:get-base-pinfo))))


(define (cps-program a-program (env default-env))
  (let loop ([a-program a-program]
             [env env])
    (cond
      [(empty? a-program)
       empty]
      [else
       (cond
         [(defn? (first a-program))
          (let-values ([(transformed-defn new-env)
                        (cps-definition (first a-program) env)])
            (cons transformed-defn 
                  (loop (rest a-program)
                        new-env)))]

         [(test-case? (first a-program))
          ;; FIXME: this is wrong.  We need to apply CPS on the use of the test.
          (cons (first a-program)
                (loop (rest a-program)
                      env))]

         [(library-require? (first a-program))
          (cons (first a-program)
                (loop (rest a-program)
                      env))]
         
         [(expression? (first a-program))
          (cons (cps-expression (first a-program) env)
                (loop (rest a-program)
                      env))])])))


;; cps-definition: defn env -> (values defn env)
;; Transforms a definition
(define (cps-definition a-defn an-env)
  (match a-defn
    [(list 'define (list fun args ...) body)
     (cps-function-definition fun args body an-env)]

    [(list 'define (? symbol? fun) (list 'lambda (list args ...) body))
     (cps-function-definition fun args body an-env)]

    [(list 'define (? symbol? id) body)
     (values `(define ,id ,(cps-expression body an-env))
             (env-extend an-env (make-binding:defined id)))]

    [(list 'define-struct id (list fields ...))
     (cps-structure-definition a-defn an-env)]))


;; cps-function-definition: symbol (listof symbol) expr env -> (values defn env)
;; Given a function definition, produces a CPSed version of that definition
(define (cps-function-definition id args body env)
  (let* ([cps-arg (generate-unique-arg args)]
         [new-env (env-extend env (make-binding:defined id))]
         [new-body (cps-expression body (foldl (lambda (arg env) 
                                                 (env-extend env (make-binding:defined arg)))
                                               new-env
                                               args))])
    (values `(define (,id ,@args ,cps-arg)
               (,new-body ,cps-arg))
            new-env)))


;; cps-structure-definition: defn env -> (values defn env)
(define (cps-structure-definition a-defn an-env)
  (match a-defn 
    [(list 'define-struct id (list fields ...))

     (define (field-accessor a-field)
       (string->symbol (format "~a-~a" id a-field)))
     
     (let* (;; Constructor
            [new-env (env-extend an-env (make-binding:primitive (string->symbol (format "make-~a" id))))]
            ;; Predicate
            [new-env (env-extend new-env (make-binding:primitive (string->symbol (format "~a?" id))))]
            ;; Accessors
            [new-env (foldl (lambda (field env)
                              (env-extend env (make-binding:primitive (field-accessor field))))
                            new-env
                            fields)])
       (values a-defn new-env))]))

    
    

;; generate-unique-arg: (listof symbol) -> symbol
;; Produces a unique symbol distinct from the given ones.
(define (generate-unique-arg args)
  (let ([arg (gensym 'k)])
      (cond
        [(member arg args)
         (generate-unique-arg)]
        [else
         arg])))






;; cps-expression: expr env -> expr
;; Translates an expression into CPS form.
(define (cps-expression an-expr an-env)
  (match an-expr

    [(list 'local [list defns ...] body)
     (cps-local-expression defns body an-env)]
    
    [(list 'cond [list questions answers] ... [list 'else answer-last])
     (cps-expression (desugar-cond an-expr) an-env)]
    
    [(list 'cond [list questions answers] ... [list question-last answer-last])
     (cps-expression (desugar-cond an-expr) an-env)]
    
    [(list 'if test consequent alternative)
     (cps-if-expression test consequent alternative an-env)]

    [(list 'and expr ...)
     (cps-and-expression expr an-env)]

    
    [(list 'or expr ...)
     (cps-or-expression expr an-env)]
        
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
    [(list operator-expr operand-exprs ...)
     (cps-application-expression operator-expr operand-exprs an-env)]))


    
(define (cps-local-expression defns body env)
  'fixme)


(define (cps-if-expression test consequent alternative env)
  (let ([cps-test (cps-expression test env)]
        [cps-consequent (cps-expression consequent env)]
        [cps-alternative (cps-expression alternative env)])
    `(lambda (k)
       (,cps-test (lambda (test-val)
                    (if test-val
                        (,cps-consequent k)
                        (,cps-alternative k)))))))


(define (cps-and-expression conjuncts env)
  (let ([cps-conjuncts (map (lambda (e) (cps-expression e env))
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


(define (cps-or-expression disjuncts env)
  (let ([cps-disjuncts (map (lambda (e) (cps-expression e env))
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



  

(define (cps-application-expression operator-expr operand-exprs env)
  (let ([cps-operator (cps-expression operator-expr env)]
        [cps-operands (map (lambda (e) (cps-expression e env))
                           operand-exprs)])
    (cond
      [(symbol? operator-expr)
       (let ([operator-binding (env-lookup env operator-expr)])
         (match operator-binding
           ['#f
            (error 'cps-application "Unknown operator: ~s" operator-expr)]
           
           [(struct binding:primitive (id))
            `(lambda (k)
               ,(let loop ([i 0]
                           [cps-operands cps-operands]
                           [args/rev empty])
                  (cond
                    [(empty? cps-operands)
                     `(k (,operator-expr ,@(reverse args/rev)))]
                    [else
                     (let ([arg (string->symbol
                                 (string-append "_" (number->string i)))])
                       `(,(first cps-operands)
                         (lambda (,arg)
                           ,(loop (add1 i) 
                                  (rest cps-operands)
                                  (cons arg args/rev)))))])))]
           
           [(struct binding:defined (id))            
            `(lambda (k)
               ,(let loop ([i 0]
                           [cps-operands cps-operands]
                           [args/rev empty])
                  (cond
                    [(empty? cps-operands)
                     `(,operator-expr ,@(reverse args/rev) k)]
                    [else
                     (let ([arg (string->symbol
                                 (string-append "_" (number->string i)))])
                       `(,(first cps-operands)
                         (lambda (,arg)
                           ,(loop (add1 i) 
                                  (rest cps-operands)
                                  (cons arg args/rev)))))])))]))]         
      [else
       `(lambda (k)
          ,(let loop ([i 0]
                      [cps-operands cps-operands]
                      [args/rev empty])
             (cond
               [(empty? cps-operands)
                `(,cps-operator ,@(reverse args/rev) k)]
               [else
                (let ([arg (string->symbol
                            (string-append "_" (number->string i)))])
                  `(,(first cps-operands)
                    (lambda (,arg)
                      ,(loop (add1 i) 
                             (rest cps-operands)
                             (cons arg args/rev)))))])))])))
       






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



