#lang scheme/base

;; This program translates advanced-level Scheme into mzscheme-vm's bytecode.

(require scheme/local
         scheme/bool
         scheme/contract
         scheme/match
         (only-in scheme/list empty first second third fourth empty? rest))


(require "../desugar.ss")
(require "../analyzer.ss")
(require "../rbtree.ss")
(require (prefix-in binding: "../../collects/moby/runtime/binding.ss"))



(require "../helpers.ss"
         
         "../pinfo.ss"
         "../../collects/moby/runtime/stx.ss"
         
         "env.ss")

(require 
 (prefix-in bcode: 
            "../../../support/externals/mzscheme-vm/src/bytecode-structs.ss"))




(provide/contract [compile-compilation-top
                   (program? pinfo? #:name symbol? . -> . 
                             (values (or/c bcode:form? bcode:indirect? any/c)
                                     pinfo?))]
                  
                  [compile-expression 
                   (expression? env? pinfo? . -> . 
                                (values 
                                 (or/c bcode:form? bcode:indirect? any/c)
                                 pinfo?))]
                  
                  [free-variables 
                   (expression? env? . -> . (listof symbol?))])




;; compile-compilation-top-module: program pinfo -> 
(define (compile-compilation-top a-program base-pinfo
                                 #:name name) 
  (let* ([a-program+pinfo (desugar-program a-program base-pinfo)]
         [a-program (first a-program+pinfo)]
         [pinfo (second a-program+pinfo)]
         [pinfo (program-analyze/pinfo a-program pinfo)])
    
    ;; The toplevel is going to include all of the defined identifiers in the pinfo
    ;; The environment will refer to elements in the toplevel.
    
    (let-values ([(toplevel-prefix env) 
                  (make-module-prefix-and-env pinfo)])
      
      (let ([defns (filter defn? a-program)]
            [requires (filter library-require? a-program)]
            [provides (filter provide-statement? a-program)]
            [expressions (filter (lambda (x) (or (test-case? x)
                                                 (expression? x))) 
                                 a-program)])
        (let*-values ([(compiled-requires pinfo)
                       (compile-requires requires pinfo)]
                      [(compiled-definitions pinfo)
                       (compile-definitions defns env pinfo)]
                      [(compiled-exprs pinfo)
                       (compile-expressions expressions env pinfo)])
          (values (bcode:make-compilation-top 0 
                                              toplevel-prefix
                                              (bcode:make-seq 
                                               (append compiled-requires
                                                       compiled-definitions 
                                                       compiled-exprs)))
                  pinfo))))))



;; make-module-prefix-and-env: pinfo -> (values prefix env)
(define (make-module-prefix-and-env pinfo)
  ;;
  ;;
  ;; FIXME: local-defined-names and module-defined-bindings may not be
  ;; disjoint, in which case, references to toplevel-bound identifiers
  ;; may actually be intended to reference a global that should be
  ;; shadowing.
  ;;
  ;; We may need to do a solution such as namespace-require/copy
  ;; (http://list.cs.brown.edu/pipermail/plt-scheme/2007-February/016390.html)
  ;; to copy all toplevel references off to globals.  That means every variable
  ;; reference (save lexically-scoped ones) become references to the global array.
  ;; This doesn't sound so good either...
  
  ;;
  ;; collect all the free names being defined and used at toplevel
  ;;
  ;; Create a prefix that refers to those values
  ;; Create an environment that maps to the prefix
  (let* ([required-modules 
          (pinfo-modules pinfo)]
         
         [required-module-bindings
          (foldl (lambda (a-module acc)
                   (append (binding:module-binding-bindings a-module)
                           acc)) 
                 empty
                 required-modules)]
         
         [free-variables (pinfo-free-variables pinfo)]
         
         [local-defined-names (rbtree-keys (pinfo-defined-names pinfo))]
         
         [module-or-toplevel-defined-bindings 
          (rbtree-fold (pinfo-used-bindings-hash pinfo)
                       (lambda (name a-binding acc)
                         #;(printf "~s ~s ~s\n"  
                                   a-binding
                                   (binding:binding-id a-binding)
                                   (binding:binding-module-source a-binding))
                         (cond
                           [(and (binding:binding-module-source a-binding)
                                 (not (member a-binding required-module-bindings)))
                            (cons a-binding acc)]
                           [else acc]))
                       '())])

    (values (bcode:make-prefix 0 (append (list #f)
                                         (map bcode:make-global-bucket free-variables)
                                         (map bcode:make-global-bucket local-defined-names)
                                         (map (lambda (binding) 
                                                (bcode:make-module-variable (module-path-index-join 
                                                                             (binding:binding-module-source binding) 
                                                                             (module-path-index-join #f #f))
                                                                            (binding:binding-id binding)
                                                                            -1
                                                                            0))
                                              (append required-module-bindings
                                                      module-or-toplevel-defined-bindings)))
                               '())
            (env-push-globals 
             empty-env 
             (append (list #f)
                     free-variables
                     local-defined-names 
                     (map binding:binding-id (append required-module-bindings module-or-toplevel-defined-bindings)))))))





(define (compile-definitions defns env a-pinfo)
  (let loop ([defns defns]
             [a-pinfo a-pinfo])
    (cond [(empty? defns)
           (values empty a-pinfo)]
          [else
           (let*-values ([(compiled-defn a-pinfo)
                          (compile-definition (first defns) env a-pinfo)]
                         [(compiled-rest-defns a-pinfo)
                          (loop (rest defns) a-pinfo)])
             (values (cons compiled-defn compiled-rest-defns)
                     a-pinfo))])))



(define (compile-definition a-defn env a-pinfo)
  (case-analyze-definition 
   a-defn
   (lambda (fun args body)
     (compile-function-definition fun args body env a-pinfo))
   (lambda (id body)
     (compile-variable-definition id body env a-pinfo))
   (lambda (id fields)
     (error 'compile-definition "structure definitions not implemented yet"))
   (lambda (ids body)
     (compile-variables-definition ids body env a-pinfo))))


;; compile-requires: (listof require) pinfo -> (values (listof bcode) pinfo)
(define (compile-requires requires a-pinfo)
  (cond [(empty? requires)
         (values empty a-pinfo)]
        [else
         (let*-values ([(first-compiled-require a-pinfo)
                        (compile-require (first requires) a-pinfo)]
                       [(rest-compiled-requires a-pinfo)
                        (compile-requires (rest requires) a-pinfo)])
           (values (cons first-compiled-require rest-compiled-requires)
                   a-pinfo))]))


(define (compile-require a-require a-pinfo)
  ;; FIXME: I should be doing some kind of module resolution here.
  (values (bcode:make-req
           (datum->syntax #f (stx->datum (second (stx-e a-require))))
           (bcode:make-toplevel 0 0 #f #f))
          a-pinfo))


(define (compile-function-definition fun-name args body env a-pinfo)
  (let*-values ([(compiled-fun-name a-pinfo)
                 (compile-expression fun-name env a-pinfo)]
                [(compiled-lambda a-pinfo)
                 (compile-lambda-expression (stx-e fun-name)
                                            args
                                            body
                                            env 
                                            a-pinfo)])
    (values (bcode:make-def-values (list compiled-fun-name)
                                   compiled-lambda)
            a-pinfo)))


(define (compile-variable-definition id body env a-pinfo)
  (let*-values ([(compiled-id a-pinfo)
                 (compile-expression id env a-pinfo)]
                [(compiled-body a-pinfo)
                 (compile-expression body env a-pinfo)])
    (values (bcode:make-def-values (list compiled-id)
                                   compiled-body)
            a-pinfo)))


;; compile-variables-definition: (listof id) body env pinfo -> (values expression pinfo)
(define (compile-variables-definition ids body env a-pinfo)
  (let*-values ([(compiled-ids a-pinfo)
                 (compile-expressions ids env a-pinfo)]
                [(compiled-body a-pinfo)
                 (compile-expression body env a-pinfo)])
    (values (bcode:make-def-values compiled-ids
                                   compiled-body)
            a-pinfo)))



;; compile-expression: expression env pinfo -> (values expr pinfo)
(define (compile-expression expr env a-pinfo)
  (cond
    
    ;; (if test consequent alternative)
    [(stx-begins-with? expr 'if)
     (local [(define test (second (stx-e expr)))
             (define consequent (third (stx-e expr)))
             (define alternative (fourth (stx-e expr)))]
       (compile-if-expression test consequent alternative env a-pinfo))]
    
    
    ;; (begin ...)
    [(stx-begins-with? expr 'begin)
     (local [(define exprs (rest (stx-e expr)))]
       (compile-begin exprs env a-pinfo))]
    
    
    ;; Identifiers
    [(symbol? (stx-e expr))
     (compile-identifier-expression expr env a-pinfo)]
    
    
    
    ;; (lambda (args ...) body)
    [(stx-begins-with? expr 'lambda)
     (local [(define args (stx-e (second (stx-e expr))))
             (define body (third (stx-e expr)))]
       (compile-lambda-expression empty args body env a-pinfo))]
    
    
    ;; (local ([define ...] ...) body)
    [(stx-begins-with? expr 'local)
     (local [(define defns (stx-e (second (stx-e expr))))
             (define body (third (stx-e expr)))]
       (compile-local-expression defns body env a-pinfo))]
    
    
    ;; (set! identifier value)
    ;; Attention: it's evaluation doesn't produce an Object
    #;[(stx-begins-with? expr 'set!)
       (local [(define id (second (stx-e expr)))
               (define value (third (stx-e expr)))]
         (set!-expression->javascript-string id value env a-pinfo))]
    
    
    ;; Quoted datums
    [(stx-begins-with? expr 'quote)
     (compile-quote-expression (second (stx-e expr)) env a-pinfo)]
    
    
    ;; Function call/primitive operation call
    [(pair? (stx-e expr))
     (local [(define operator (first (stx-e expr)))
             (define operands (rest (stx-e expr)))]
       (compile-application-expression/stack-record (stx-loc expr)
                                                    operator
                                                    operands env a-pinfo))]
    
    
    ;; Regular data are just themselves in the emitted bytecode.
    
    ;; Numbers
    [(number? (stx-e expr))
     (values (stx-e expr) a-pinfo)]
    
    ;; Strings
    [(string? (stx-e expr))
     (values (stx-e expr) a-pinfo)]
    
    ;; Bytes
    [(bytes? (stx-e expr))
     (values (stx-e expr) a-pinfo)]

    ;; Literal booleans
    [(boolean? (stx-e expr))
     (values (stx-e expr) a-pinfo)]
    
    ;; Characters
    [(char? (stx-e expr))
     (values (stx-e expr) a-pinfo)]


    ;; Paths
    [(path? (stx-e expr))
     (values (stx-e expr) a-pinfo)]

    ;; Boxes
    [(box? (stx-e expr))
     (values (stx-e expr) a-pinfo)]
    
    ;; Regexps
    [(regexp? (stx-e expr))
     (values (stx-e expr) a-pinfo)]


    ;; Byte regexps
    [(byte-regexp? (stx-e expr))
     (values (stx-e expr) a-pinfo)]))




;; compile-expressions: (listof expression) env pinfo -> (values (listof expr) pinfo)
(define (compile-expressions exprs env pinfo)
  (let loop ([exprs exprs]
             [pinfo pinfo])
    (cond
      [(empty? exprs)
       (values empty pinfo)]
      [else
       (let*-values ([(compiled-expr pinfo-1)
                      (compile-expression (first exprs) env pinfo)]
                     [(compiled-rest-exprs pinfo-2)
                      (loop (rest exprs) pinfo-1)])
         (values (cons compiled-expr compiled-rest-exprs)
                 pinfo-2))])))





;; compile-if-expression: expression expression expression env pinfo -> (values expr pinfo)
(define (compile-if-expression test then else env pinfo)
  (let*-values ([(c-test pinfo-1) (compile-expression test env pinfo)]
                [(c-then pinfo-2) (compile-expression then env pinfo-1)]
                [(c-else pinfo-3) (compile-expression else env pinfo-2)])
    (values (bcode:make-branch c-test c-then c-else)
            pinfo-3)))


(define (compile-begin exprs env pinfo)
  (let-values ([(compiled-exprs pinfo-1)
                (compile-expressions exprs env pinfo)])
    (values (bcode:make-seq compiled-exprs) pinfo-1)))


(define (compile-identifier-expression expr env pinfo)
  (let ([a-stack-reference (env-lookup env (stx-e expr))])
    (values (match a-stack-reference
              [(struct local-stack-reference (name boxed? depth))
               (bcode:make-localref boxed? depth #f #f #f)]
              
              [(struct global-stack-reference (name depth pos))
               (bcode:make-toplevel depth pos #f #f)]
              
              [(struct unbound-stack-reference (name))
               (error 'compile-identifier-expression 
                      (format "Couldn't find ~a in the environment" 
                              name))])
    pinfo)))




;; quote-expression->javascript-string: expr env pinfo -> (values expr pinfo)
(define (compile-quote-expression expr env pinfo)
  (values (stx->datum expr) pinfo))





;; compile-lambda-expression: (or symbol empty) (listof symbol-stx) expr env pinfo -> (values lam pinfo)
;; Compile a lambda expression.  The lambda must close its free variables over the
;; environment.
(define (compile-lambda-expression name args body env pinfo)
  (let*-values ([(free-vars) 
                 (free-variables body 
                                 (foldl (lambda (var env) (env-push-local env (stx-e var)))
                                        empty-env
                                        args))]
                [(closure-vector extended-env)
                 (get-closure-vector-and-env (map stx-e args) free-vars env)]
                
                [(compiled-body pinfo-1) 
                 (compile-expression body extended-env pinfo)])
    
    (values (bcode:make-lam name 
                            '()
                            (length args)
                            (build-list (length args) (lambda (i)
                                                        'val))
                            #f
                            closure-vector
                            (build-list (vector-length closure-vector) 
                                        (lambda (i) 'val/ref))
                            0
                            compiled-body)
            pinfo-1)))


;; get-closure-vector-and-env: (listof symbol) (listof symbol) env -> (values (vectorof number) env) 
;; Produce the closure map, given the set of free variables.
(define (get-closure-vector-and-env args free-variables original-env)
  (let* ([free-variable-references 
          (map (lambda (var) (env-lookup original-env var))
               free-variables)])
    (cond 
      ;; If anything's unbound, we're in trouble and need to signal an error.
      [(ormap unbound-stack-reference? free-variable-references)
       (error 'get-closure-vector-and-env
              (format "Can't produce closure; I don't know where ~s is bound."
                      (unbound-stack-reference-name
                       (findf unbound-stack-reference? free-variable-references))))]
      
      [else
       (let* ([lexical-free-references 
               (sort-and-unique (filter local-stack-reference? free-variable-references)
                                (lambda (x y) (< (local-stack-reference-depth x)
                                                 (local-stack-reference-depth y)))
                                (lambda (x y) (= (local-stack-reference-depth x)
                                                 (local-stack-reference-depth y))))]
              [lexical-free-depths (map local-stack-reference-depth lexical-free-references)]
              
              [global-references (filter global-stack-reference? free-variable-references)]
              [global-depths (sort-and-unique (map global-stack-reference-depth global-references)
                                              < =)]
              
              ;; Function arguments
              [env-1 (foldl (lambda (name env)
                              (env-push-local env name))
                            original-env
                            (reverse args))]         
              
              ;; The lexical free variables
              [env-2 (foldl (lambda (ref env)
                              (cond 
                                [(local-stack-reference-boxed? ref)
                                 (env-push-local/boxed env 
                                                 (local-stack-reference-name ref))]
                                [else
                                 (env-push-local env 
                                                 (local-stack-reference-name ref))]))
                            env-1
                            (reverse lexical-free-references))]
              ;; The global free variables
              [env-3 (foldl (lambda (a-depth env)
                              (let* ([references-at-depth
                                      (filter (lambda (a-ref)
                                                (= (global-stack-reference-depth a-ref) a-depth))
                                              global-references)]
                                     [used-global-names 
                                      (map global-stack-reference-name
                                           references-at-depth)])
                                (env-push-globals env 
                                                  (mask-unused-globals 
                                                   (global-env-names
                                                    (env-peek original-env a-depth))
                                                   used-global-names))))
                            env-2
                            (reverse global-depths))])
         #|
           When the function is called, the rest-argument list (if any) is pushed onto the stack,
           then the normal arguments in reverse order, then the closure-captured values in 
           reverse order. Thus, when body is run, the first value on the stack is the first value
           captured by the closure-map array, and so on.
         |#
         
         (values (list->vector (append global-depths lexical-free-depths))
                 env-3))])))


;; mask-unused-globals: (listof symbol?) (listof symbol?) -> (listof (or/c symbol? false/c))
(define (mask-unused-globals list-of-names names-to-keep)
  (map (lambda (n)
         (cond
           [(member n names-to-keep)
            n]
           [else
            #f]))
       list-of-names))


(define MOBY-STACK-RECORD-CONTINUATION-MARK-KEY
  'moby-stack-record-continuation-mark-key)


(define (compile-application-expression/stack-record a-loc operator operands env pinfo)
  (let-values ([(an-app pinfo)
                (compile-application-expression operator operands env pinfo)])
    (values (bcode:make-with-cont-mark
             MOBY-STACK-RECORD-CONTINUATION-MARK-KEY
             (vector (Loc-id a-loc)
                     (Loc-offset a-loc)
                     (Loc-line a-loc)
                     (Loc-column a-loc)
                     (Loc-span a-loc))
             an-app)
            pinfo)))

;; compile-application-expression: expr (listof expr) env pinfo -> (values expression-form pinfo)
(define (compile-application-expression operator operands env pinfo)
  (let*-values ([(extended-env) (foldl (lambda (operand env)
                                         (env-push-unnamed env))
                                       env
                                       operands)]
                ;; extended-env includes the intermediate scratch space used for operand/operator
                ;; evaluation.
                [(compiled-operator pinfo-1)
                 (compile-expression operator extended-env pinfo)]
                [(compiled-operands pinfo-2)
                 (compile-expressions operands extended-env pinfo-1)])
    (values (bcode:make-application compiled-operator compiled-operands) pinfo-2)))



;; compile-local-expression: (listof defn) body env pinfo -> (values bcode pinfo)
(define (compile-local-expression defns body env pinfo)
  (cond
    [(empty? defns)
     (compile-expression body env pinfo)]
    [else
     (let* ([defined-names (collect-defined-names defns)]
            [env-with-boxed-names (foldl (lambda (id env)
                                           (env-push-local/boxed env (stx-e id)))
                                         env 
                                         (reverse defined-names))])

       (let-values
           ([(let-void-body pinfo)
             (let loop ([defns defns]
                        [pinfo pinfo]
                        [i 0])
               (cond [(empty? defns)
                      (compile-expression body 
                                          env-with-boxed-names
                                          pinfo)]
                     [else
                      (case-analyze-definition 
                       (first defns)
                       
                       (lambda (id args body)
                         (let*-values ([(lambda-rhs pinfo)
                                        (compile-lambda-expression 
                                         (stx-e id)
                                         args body 
                                         env-with-boxed-names pinfo)]
                                       [(new-body pinfo)
                                        (loop (rest defns) pinfo (add1 i))])
                           (values (bcode:make-install-value 1
                                                             i
                                                             #t
                                                             lambda-rhs
                                                             new-body)
                                   pinfo)))
                       
                       (lambda (id val)
                         (let*-values ([(rhs pinfo)
                                        (compile-expression 
                                         val env-with-boxed-names pinfo)]
                                       [(new-body pinfo)
                                        (loop (rest defns) pinfo (add1 i))])
                           (values (bcode:make-install-value 1
                                                             i
                                                             #t
                                                             rhs
                                                             new-body)
                                   pinfo)))
                       
                       
                       (lambda (id fields)
                         (error 'compile-local-expression 
                                "IMPOSSIBLE: structures should have been desugared"))
                       
                       (lambda (ids body)
                         (let*-values ([(rhs pinfo)
                                        (compile-expression 
                                         body env-with-boxed-names pinfo)]
                                       [(new-body pinfo)
                                        (loop (rest defns)
                                              pinfo
                                              (+ i (length ids)))])
                           (values (bcode:make-install-value (length ids)
                                                             i
                                                             #t
                                                             rhs
                                                             new-body)
                                   pinfo))))]))])
       
         (values (bcode:make-let-void 
                  (length defined-names)
                  #t
                  let-void-body)
                 pinfo)))]))





;; collect-defined-names: (listof defn) -> (listof id-stx)
(define (collect-defined-names defns)
  (reverse 
   (foldl (lambda (a-defn collected-names)
            (case-analyze-definition a-defn
                                     (lambda (id args body)
                                       (cons id collected-names))
                                     (lambda (id val)
                                       (cons id collected-names))
                                     (lambda (id fields)
                                       (error 'collect-defined-names
                                              "IMPOSSIBLE"))
                                     (lambda (ids body)
                                       (append (reverse ids) collected-names))))
          empty
          defns)))






;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; free-variables: expr env pinfo -> (listof symbol)
;; Given an expression, compute the set of free variable occcurances
(define (free-variables expr env)
  (sort-and-unique (let loop ([expr expr]
                              [env env])
                     (cond
                       ;; (if test consequent alternative)
                       [(stx-begins-with? expr 'if)
                        (local [(define test (second (stx-e expr)))
                                (define consequent (third (stx-e expr)))
                                (define alternative (fourth (stx-e expr)))]
                          (append (loop test env)
                                  (loop consequent env)
                                  (loop alternative env)))]
                       
                       
                       ;; (begin ...)
                       [(stx-begins-with? expr 'begin)
                        (local [(define exprs (rest (stx-e expr)))]
                          (apply append
                                 (map (lambda (e) (loop e env)) exprs)))]
                       
                       
                       ;; Identifiers
                       [(symbol? (stx-e expr))
                        (match (env-lookup env (stx-e expr))
                          [(struct local-stack-reference (name boxed? depth))
                           empty]
                          [(struct global-stack-reference (name depth pos))
                           empty]
                          [(struct unbound-stack-reference (name))
                           (list (stx-e expr))])]
                       
                       
                       ;; (local ([define ...] ...) body)
                       [(stx-begins-with? expr 'local)
                          (local [(define defns (stx-e (second (stx-e expr))))
                                  (define body (third (stx-e expr)))] 
                            ;; construct an updated environment, adding all the definitions
                            ;; introduced by defns.
                            ;; Also walk though each of the definitions and collect its free variables.
                            (let* ([defined-names (collect-defined-names defns)]
                                   [updated-env (foldl (lambda (id env)
                                                         (env-push-local/boxed env (stx-e id)))
                                                       env 
                                                       (reverse defined-names))])
                              (append 
                               (loop body updated-env)
                               (apply append (map (lambda (a-defn) 
                                                    (case-analyze-definition a-defn
                                                                             (lambda (id args body)
                                                                               (loop body (foldl (lambda (id env)
                                                                                                   (env-push-local env (stx-e id)))
                                                                                                 updated-env
                                                                                                 args)))
                                                                             (lambda (id body)
                                                                               (loop body updated-env))
                                                                             (lambda (id fields)
                                                                               empty)
                                                                             (lambda (ids body)
                                                                               (loop body updated-env))))
                                                  defns)))))]
                              
                              

                       
                       
                       ;; (set! identifier value)
                       ;; Attention: it's evaluation doesn't produce an Object
                       #;[(stx-begins-with? expr 'set!)
                          (local [(define id (second (stx-e expr)))
                                  (define value (third (stx-e expr)))]
                            ...)]
                       
                       ;; (and exprs ...)
                       [(stx-begins-with? expr 'and)
                        (apply append (map (lambda (x)
                                             (loop x env)))
                               (rest (stx-e expr)))]
                       
                       ;; (or exprs ...)
                       [(stx-begins-with? expr 'or)
                        (apply append (map (lambda (x)
                                             (loop x env)))
                               (rest (stx-e expr)))]
                       
                       
                       ;; (lambda (args ...) body)
                       [(stx-begins-with? expr 'lambda)
                        (let ([args (map stx-e (stx-e (second (stx-e expr))))]
                              [body (third (stx-e expr))])
                          (loop body (foldl (lambda (id env)
                                              (env-push-local env id))
                                            env
                                            (reverse args))))]
                       
                       
                       ;; Quoted datums
                       [(stx-begins-with? expr 'quote)
                        empty]
                       
                       ;; Function call/primitive operation call
                       [(pair? (stx-e expr))
                        (apply append (map (lambda (x)
                                             (loop x env))
                                           (stx-e expr)))]
                       
                       ;; Numbers
                       [(number? (stx-e expr))
                        empty]
                       
                       ;; Strings
                       [(string? (stx-e expr))
                        empty]
                       
                       ;; Literal booleans
                       [(boolean? (stx-e expr))
                        empty]
                       
                       ;; Characters
                       [(char? (stx-e expr))
                        empty]
                       [else
                        (error 'free-variables (format "~s" (stx-e expr)))]))
                   
                   (lambda (x y)
                     (string<? (symbol->string x) (symbol->string y)))
                   
                   symbol=?))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



;; sort-and-unique: (listof X) (X X -> boolean) (X X -> boolean) -> (listof symbol)
(define (sort-and-unique elts < =)
  (let loop ([elts (sort elts <)])
    (cond
      [(empty? elts)
       empty]
      [(empty? (rest elts))
       elts]
      [(= (first elts) (second elts))
       (loop (rest elts))]
      [else
       (cons (first elts) (loop (rest elts)))])))








;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



