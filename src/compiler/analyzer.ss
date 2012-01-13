#lang s-exp "lang.ss"

(define pair? cons?)

(require "env.ss")
(require "pinfo.ss")
(require "helpers.ss")
(require "rbtree.ss")
(require "../collects/moby/runtime/permission-struct.ss")
(require "../collects/moby/runtime/binding.ss")
(require "../collects/moby/runtime/stx.ss")
(require "../collects/moby/runtime/error-struct.ss")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Assumption: the functions here assume that the input program has already been
;; desugared.



;; program-analyze: program [program-info] -> program-info
;; Collects which identifiers are defined by the program, and which identifiers
;; are actively used.

(define (program-analyze a-program)
  (program-analyze/pinfo a-program (get-base-pinfo 'base)))


(define (program-analyze/pinfo a-program pinfo)
  (local [(define pinfo-1
            (program-analyze-collect-definitions a-program pinfo))
          (define pinfo-2
            (program-analyze-collect-provides a-program pinfo-1))]
    ;; FIXME: we need to walk all the require-permissions and mark
    ;; bindings with the appropriate ones.
    (program-analyze-uses a-program pinfo-2)))




;; program-analyze-collect-definitions: program pinfo -> pinfo
;; Collects the definitions either imported or defined by this program.
(define (program-analyze-collect-definitions a-program pinfo)
  ;; FIXME: this does not yet say anything if a definition is introduced twice
  ;; in the same lexical scope.  We must do this error check!
  (foldl (lambda (an-element pinfo)
           (cond [(defn? an-element)
                  (definition-analyze-collect-definitions an-element pinfo)]
                 [(test-case? an-element)
                  pinfo]
                 [(library-require? an-element)
                  (require-analyze-collect-definitions (second (stx-e an-element)) pinfo)]
                 [(provide-statement? an-element)
                  pinfo]
                 [(expression? an-element)
                  pinfo]
                 
                 ;; HACK!  Shriram says this is wrong!
                 [(require-permission? an-element)
                  (pinfo-accumulate-declared-permission
                   (stx-e (second (stx-e an-element)))
                   (string->permission (stx->datum (third (stx-e an-element))))
                   pinfo)]))
         pinfo
         a-program))



;; program-analyze-collect-provides: program pinfo -> pinfo
;; Walk through the program and collect all the provide statements.
(define (program-analyze-collect-provides a-program pinfo)
  (foldl (lambda (an-element pinfo)
           (cond [(defn? an-element)
                  pinfo]
                 [(test-case? an-element)
                  pinfo]
                 [(library-require? an-element)
                  pinfo]
                 [(provide-statement? an-element)
                  (collect-provided-names (rest (stx-e an-element)) pinfo)]
                 [(expression? an-element)
                  pinfo]
                 [(require-permission? an-element)
                  pinfo]))
         pinfo
         a-program))




;; program-analyze-uses: program pinfo -> pinfo
;; Collects the uses of bindings that this program uses.
(define (program-analyze-uses a-program pinfo)
  (foldl (lambda (an-element pinfo)
           (cond [(defn? an-element)
                  (definition-analyze-uses an-element pinfo)]
                 [(test-case? an-element)
                  pinfo]
                 [(library-require? an-element)
                  pinfo]
                 [(provide-statement? an-element)
                  pinfo]
                 [(expression? an-element)
                  (expression-analyze-uses an-element
                                           pinfo 
                                           (pinfo-env pinfo))]
                 [(require-permission? an-element)
                  pinfo]))
         pinfo
         a-program))



;; collect-provided-names: (listof stx) pinfo -> pinfo
;; Collect the provide statements into the pinfo-provided-names.
(define (collect-provided-names clauses a-pinfo)
  (foldl (lambda (a-clause a-pinfo)
           (cond
             [(symbol? (stx-e a-clause))
              (begin 
                (unless (rbtree-member? symbol< (pinfo-defined-names a-pinfo) (stx-e a-clause))
                  (raise (make-moby-error (stx-loc a-clause)
                                          (make-moby-error-type:provided-name-not-defined
                                           (stx-e a-clause)))))
                (pinfo-update-provided-names a-pinfo
                                             (rbtree-insert symbol<
                                                            (pinfo-provided-names a-pinfo)
                                                            (stx-e a-clause)
                                                            (make-provide-binding:id a-clause))))]
             [(stx-begins-with? a-clause 'struct-out)
              (cond
                [(and (= (length (stx-e a-clause)) 2)
                      (symbol? (stx-e (second (stx-e a-clause)))))
                 (begin
                   (unless (and (rbtree-member? symbol< (pinfo-defined-names a-pinfo) 
                                                (stx-e (second (stx-e a-clause))))
                                (binding:structure?
                                 (rbtree-ref symbol< 
                                             (pinfo-defined-names a-pinfo) 
                                             (stx-e (second (stx-e a-clause)))
                                             (lambda () #f))))
                     (raise (make-moby-error (stx-loc a-clause)
                                             (make-moby-error-type:provided-structure-not-structure
                                              (stx-e (second (stx-e a-clause)))))))
                   (pinfo-update-provided-names a-pinfo
                                                (rbtree-insert symbol<
                                                               (pinfo-provided-names a-pinfo)
                                                               (stx-e (second (stx-e a-clause)))
                                                               (make-provide-binding:struct-id 
                                                                (second (stx-e a-clause))))))]
                [else
                 (raise (make-moby-error (stx-loc a-clause)
                                         (make-moby-error-type:generic-syntactic-error
                                          (format "provide doesn't recognize the syntax of the clause: ~s" 
                                                  (stx->datum a-clause))
                                          (list))))])]
             [else
              (raise (make-moby-error (stx-loc a-clause)
                                      (make-moby-error-type:generic-syntactic-error
                                       (format "provide doesn't recognize the syntax of the clause: ~s" 
                                               (stx->datum a-clause))
                                       (list))))]))
         a-pinfo
         clauses))





;; bf: symbol path number boolean string -> binding:function
;; Helper function.
(define (bf name module-path arity vararity?)
  (make-binding:function name module-path arity vararity? empty false))


;; definition-analyze-collect-definitions: definition program-info -> program-info
;; Collects the defined names introduced by the definition.
(define (definition-analyze-collect-definitions a-definition pinfo)
  (case-analyze-definition 
   a-definition
   
   ;; For functions
   (lambda (id args body)
     (pinfo-accumulate-defined-binding (bf (stx-e id)
                                           false
                                           (length args) 
                                           false)
                                       pinfo
                                       (stx-loc id)))
   
   ;; For regular defintions
   (lambda (id expr)
     (pinfo-accumulate-defined-binding (make-binding:constant 
                                        (stx-e id)
                                        #f
                                        empty)
                                       pinfo
                                       (stx-loc id)))
   
   ;; For structure definitions
   (lambda (id fields)
     (pinfo-accumulate-defined-bindings (struct-definition-bindings (stx-e id) 
                                                                    (map stx-e fields))
                                        pinfo
                                        (stx-loc id)))
   
   ;; define-values
   (lambda (ids body)
     (foldl (lambda (id pinfo)
              (pinfo-accumulate-defined-binding 
               (make-binding:constant
                (stx-e id)
                #f
                empty)
               pinfo
               (stx-loc id)))
            pinfo
            ids))))




;; struct-definition-bindings: (listof symbol) -> (listof binding)
;; Makes the bindings for the identifiers introduced by a structure definition.
(define (struct-definition-bindings id fields)
  (local [(define constructor-id 
            (string->symbol (string-append "make-" (symbol->string id))))
          (define constructor-binding 
            (bf constructor-id false (length fields) false))
          (define predicate-id
            (string->symbol (string-append (symbol->string id) "?")))
          (define predicate-binding
            (bf predicate-id false 1 false))
          (define selector-ids
            (map (lambda (f)
                   (string->symbol (string-append (symbol->string id) "-" (symbol->string f))))
                 fields))
          (define selector-bindings
            (map (lambda (sel-id) 
                   (bf sel-id false 1 false))
                 selector-ids))
          (define mutator-ids
            (map (lambda (f)
                   (string->symbol (string-append "set-" (symbol->string id) "-" (symbol->string f) "!")))
                 fields))
          (define mutator-bindings
            (map (lambda (mut-id)
                   (bf mut-id false 2 false))
                 mutator-ids))
          
          (define structure-binding
            (make-binding:structure id
                                    #f
                                    fields
                                    constructor-id
                                    predicate-id
                                    selector-ids
                                    mutator-ids))]
    (append (list structure-binding)
            (list constructor-binding)
            (list predicate-binding)
            selector-bindings 
            mutator-bindings)))





;; definition-analyze-uses: definition program-info -> program-info
;; Collects the used names in a definition.
(define (definition-analyze-uses a-definition pinfo)
  (case-analyze-definition a-definition
                           (lambda (id args body)
                             ;; The body of a function may introduce a use.
                             (function-definition-analyze-uses id args body pinfo))
                           (lambda (id expr)
                             ;; A regular definition may introduce a use.
                             (expression-analyze-uses expr pinfo (pinfo-env pinfo)))
                           (lambda (id fields)
                             ;; Structures don't introduce any uses.
                             pinfo)
                           
                           ;; define-values can introduce uses
                           (lambda (ids body)
                             (expression-analyze-uses body 
                                                      pinfo 
                                                      (pinfo-env pinfo)))))


;; function-definition-analyze-uses: stx (listof stx) stx program-info -> program-info
(define (function-definition-analyze-uses fun args body pinfo)
  (local [(define env-1 (pinfo-env pinfo))
          (define env-2 
            (env-extend env-1 (bf (stx-e fun) false (length args) false)))]
    (lambda-expression-analyze-uses args body (pinfo-update-env pinfo env-2))))



;; lambda-expression-analyze-uses: (listof stx) stx program-info -> program-info
(define (lambda-expression-analyze-uses args body pinfo)
  (local [(define env-1 (pinfo-env pinfo))
          (define env-2
            (foldl (lambda (arg-id env) 
                     (env-extend env (make-binding:constant (stx-e arg-id)
                                                            #f
                                                            empty)))
                   env-1
                   args))]
    (expression-analyze-uses body pinfo env-2)))





;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; expression-analyze-uses: stx program-info env -> program-info
(define (expression-analyze-uses an-expression pinfo env)
  (cond
    
    [(stx-begins-with? an-expression 'local)
     (local-expression-analyze-uses an-expression pinfo env)]
    
    [(stx-begins-with? an-expression 'begin)
     (begin-expression-analyze-uses an-expression pinfo env)]
    
    [(stx-begins-with? an-expression 'if)
     (if-expression-analyze-uses an-expression pinfo env)]
    
    [(stx-begins-with? an-expression 'and)
     (local [(define exprs (rest (stx-e an-expression)))]
       (foldl (lambda (e p) (expression-analyze-uses e p env))
              pinfo 
              exprs))]
    
    [(stx-begins-with? an-expression 'or)
     (local [(define exprs (rest (stx-e an-expression)))]
       (foldl (lambda (e p) (expression-analyze-uses e p env))
              pinfo 
              exprs))]
    
    [(stx-begins-with? an-expression 'lambda)
     (local [(define args (stx-e (second (stx-e an-expression))))
             (define body (third (stx-e an-expression)))]
       (lambda-expression-analyze-uses args body pinfo))]
    
    ;; Numbers
    [(number? (stx-e an-expression))
     pinfo]
    
    ;; Strings
    [(string? (stx-e an-expression))
     pinfo]
    
    ;; Literal booleans
    [(boolean? (stx-e an-expression))
     pinfo]
    
    ;; Characters
    [(char? (stx-e an-expression))
     pinfo]
    
    ;; Identifiers
    [(symbol? (stx-e an-expression))
     (cond
       [(not (eq? (env-lookup/context env an-expression) #f))
        (pinfo-accumulate-binding-use (env-lookup/context env an-expression) pinfo)]
       [else
        ;; free variable
        (pinfo-accumulate-free-variable-use (stx-e an-expression) pinfo)])]
    
    
    ;; Quoted symbols
    [(stx-begins-with? an-expression 'quote)
     pinfo]
    
    ;; Function call/primitive operation call
    [(pair? (stx-e an-expression))
     (application-expression-analyze-uses an-expression pinfo env)]
    
    [else
     pinfo]))



;; local-definition-analyze-uses: stx pinfo env -> pinfo
(define (local-expression-analyze-uses an-expression pinfo env)
  (local [(define defns (stx-e (second (stx-e an-expression))))
          (define body (third (stx-e an-expression)))
          (define nested-pinfo (foldl (lambda (a-defn a-pinfo)
                                        (definition-analyze-uses a-defn a-pinfo))
                                      pinfo
                                      defns))]
    (pinfo-update-env 
     (expression-analyze-uses body
                              nested-pinfo
                              (pinfo-env nested-pinfo))
     (pinfo-env pinfo))))


;; begin-expression-analyze-uses: expr-stx pinfo env -> pinfo
(define (begin-expression-analyze-uses an-expression pinfo env)
  (foldl (lambda (e p)
           (expression-analyze-uses e p env))
         pinfo
         (rest (stx-e an-expression))))




(define (if-expression-analyze-uses an-expression pinfo env)
  (local [(define test (second (stx-e an-expression)))
          (define consequent (third (stx-e an-expression)))
          (define alternative (fourth (stx-e an-expression)))]
    (foldl (lambda (e p) (expression-analyze-uses e p env))
           pinfo 
           (list test consequent alternative))))


(define (application-expression-analyze-uses an-expression pinfo env)
  (foldl (lambda (e p)
           (expression-analyze-uses e p env))
         pinfo
         (stx-e an-expression)))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;






;; require-analyze: require-path-stx -> pinfo
;; When we hit a require, we have to extend our environment to include the list of module
;; bindings provided by that module.
(define (require-analyze-collect-definitions require-path pinfo)
  (local [(define (signal-error)
            (raise (make-moby-error (stx-loc require-path)
                                    (make-moby-error-type:unknown-module (stx-e require-path)))))
          
          (define maybe-module-name ((pinfo-module-path-resolver pinfo)
                                     (stx-e require-path)
                                     (pinfo-current-module-path pinfo)))]
    
    (cond
      [(module-name? maybe-module-name)
       (local [(define maybe-module-binding
                 ((pinfo-module-resolver pinfo) maybe-module-name))]
         (cond [(module-binding? maybe-module-binding)
                (begin
                  #;(printf "require-analyze-collect-definitions: installing ~s\n"
                          maybe-module-binding)
                  (pinfo-accumulate-module maybe-module-binding
                                           (pinfo-accumulate-module-bindings
                                            (module-binding-bindings maybe-module-binding)
                                            pinfo)))]
               [else
                (begin
                  #;(printf "~s doesn't mind to any known module: ~s\n" maybe-module-name maybe-module-binding)
                  (signal-error))]))]
      [else
       (begin
         #;(printf "not a module name: ~s~n" maybe-module-name)
         (signal-error))])))




(provide/contract [program-analyze (program?  . -> . pinfo?)]
                  [program-analyze/pinfo (program? pinfo? . -> . pinfo?)])
