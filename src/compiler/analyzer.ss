#lang scheme

(require "env.ss")
(require "pinfo.ss")
(require "toplevel.ss")
(require "stx.ss")
(require "helpers.ss")
(require "permission.ss")
(require "modules.ss")


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;; program-analyze: program [program-info] -> program-info
;; Collects which identifiers are defined by the program, and which identifiers
;; are actively used.

(define (program-analyze a-program)
  (program-analyze/pinfo a-program (get-base-pinfo 'base)))


(define (program-analyze/pinfo a-program pinfo)
  (local [(define pinfo-1
            (program-analyze-collect-definitions a-program pinfo))]
    (program-analyze-uses a-program pinfo-1)))




;; program-analyze-collect-definitions: program pinfo -> pinfo
;; Collects the definitions either imported or defined by this program.
(define (program-analyze-collect-definitions a-program pinfo)
  ;; FIXME: this does not yet say anything if a definition is introduced twice
  ;; in the same lexical scope.  We must do this error check!
  (cond [(empty? a-program)
         pinfo]
        [else
         (local [(define updated-pinfo
                   (cond [(defn? (first a-program))
                          (definition-analyze-collect-definitions (first a-program) pinfo)]
                         [(test-case? (first a-program))
                          pinfo]
                         [(library-require? (first a-program))
                          (require-analyze (second (first a-program)) pinfo)]
                         [(expression? (first a-program))
                          pinfo]))]
           (program-analyze-collect-definitions (rest a-program)
                                                updated-pinfo))]))



;; program-analyze-uses: program pinfo -> pinfo
;; Collects the uses of bindings that this program uses.
(define (program-analyze-uses a-program pinfo)
  (cond [(empty? a-program)
         pinfo]
        [else
         (local [(define updated-pinfo
                   (cond [(defn? (first a-program))
                          (definition-analyze-uses (first a-program) pinfo)]
                         [(test-case? (first a-program))
                          pinfo]
                         [(library-require? (first a-program))
                          pinfo]
                         [(expression? (first a-program))
                          (expression-analyze-uses (first a-program)
                                                   pinfo 
                                                   (pinfo-env pinfo))]))]
           (program-analyze-uses (rest a-program)
                                 updated-pinfo))]))

;; bf: symbol path number boolean string -> binding:function
;; Helper function.
(define (bf name module-path arity vararity? java-string)
  (make-binding:function name module-path arity vararity? java-string empty false))


;; definition-analyze-collect-definitions: definition program-info -> program-info
;; Collects the defined names introduced by the definition.
(define (definition-analyze-collect-definitions a-definition pinfo)
  (case-analyze-definition 
   a-definition
   
   ;; For functions
   (lambda (id args body)
     (pinfo-accumulate-binding (bf id
                                   false
                                   (length args) 
                                   false 
                                   (symbol->string
                                    (identifier->munged-java-identifier id)))
                               pinfo))
   
   ;; For regular defintions
   (lambda (id expr)
     (pinfo-accumulate-binding (make-binding:constant id
                                                      (symbol->string 
                                                       (identifier->munged-java-identifier id))
                                                      empty)
                               pinfo))
   
   ;; For structure definitions
   (lambda (id fields)
     (pinfo-update-env pinfo (extend-env/struct-defns (pinfo-env pinfo) id fields)))))




;; extend-env/struct-defns: env symbol (listof symbol) -> env
;; Extends the environment by adding bindings for those identifiers introduced
;; by a structure definition.
(define (extend-env/struct-defns an-env id fields)
  (local [(define constructor-id 
            (string->symbol (string-append "make-" (symbol->string id))))
          (define constructor-binding 
            (bf constructor-id false (length fields) false
                (symbol->string
                 (identifier->munged-java-identifier constructor-id))))
          (define predicate-id
            (string->symbol (string-append (symbol->string id) "?")))
          (define predicate-binding
            (bf predicate-id false 1 false
                (symbol->string
                 (identifier->munged-java-identifier predicate-id))))
          (define selector-ids
            (map (lambda (f)
                   (string->symbol (string-append (symbol->string id) "-" (symbol->string f))))
                 fields))
          (define selector-bindings
            (map (lambda (sel-id) 
                   (bf sel-id false 1 false 
                       (symbol->string
                        (identifier->munged-java-identifier sel-id))))
                 selector-ids))]
    (foldl (lambda (a-binding an-env)
             (env-extend an-env a-binding))
           an-env
           (list* constructor-binding predicate-binding selector-bindings))))





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
                             pinfo)))


;; function-definition-analyze-uses: symbol (listof symbol) expression program-info -> program-info
(define (function-definition-analyze-uses fun args body pinfo)
  
  (local [(define env-1 (pinfo-env pinfo))
          (define env-2 
            (env-extend env-1 (bf fun false (length args) false
                                  (symbol->string (identifier->munged-java-identifier fun)))))]
    (lambda-expression-analyze-uses args body (pinfo-update-env pinfo env-2))))



;; lambda-expression-analyze-uses: (listof symbol) expression program-info -> program-info
(define (lambda-expression-analyze-uses args body pinfo)
  (local [(define env-1 (pinfo-env pinfo))
          (define env-2
            (foldl (lambda (arg-id env) 
                     (env-extend env (make-binding:constant arg-id 
                                                            (symbol->string
                                                             arg-id)
                                                            empty)))
                   env-1
                   args))]
    (expression-analyze-uses body pinfo env-2)))





;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; expression-analyze-uses: expression program-info env -> program-info
(define (expression-analyze-uses an-expression pinfo env)
  (cond
    
    [(stx-begins-with? an-expression 'local)
     (local-expression-analyze-uses an-expression pinfo env)]
    
    [(stx-begins-with? an-expression 'cond)
     (expression-analyze-uses (desugar-cond an-expression)
                              pinfo
                              env)]
    
    [(stx-begins-with? an-expression 'if)
     (if-expression-analyze-uses an-expression pinfo env)]
    
    [(stx-begins-with? an-expression 'and)
     (local [(define exprs (rest an-expression))]
       (foldl (lambda (e p) (expression-analyze-uses e p env))
              pinfo 
              exprs))]
    
    [(stx-begins-with? an-expression 'or)
     (local [(define exprs (rest an-expression))]
       (foldl (lambda (e p) (expression-analyze-uses e p env))
              pinfo 
              exprs))]
    
    [(stx-begins-with? an-expression 'lambda)
     (local [(define args (second an-expression))
             (define body (third an-expression))]
       (lambda-expression-analyze-uses args body pinfo))]
    
    ;; Numbers
    [(number? an-expression)
     pinfo]
    
    ;; Strings
    [(string? an-expression)
     pinfo]
    
    ;; Literal booleans
    [(boolean? an-expression)
     pinfo]
    
    ;; Characters
    [(char? an-expression)
     pinfo]
    
    ;; Identifiers
    [(symbol? an-expression)
     (cond
       [(env-contains? env an-expression)
        (pinfo-accumulate-binding-use (env-lookup env an-expression) pinfo)]
       [else
        pinfo])]
    
    ;; Quoted symbols
    [(stx-begins-with? an-expression 'quote)
     pinfo]
    
    ;; Function call/primitive operation call
    [(pair? an-expression)
     (application-expression-analyze-uses an-expression pinfo env)]))



;; local-definition-analyze-uses: expression pinfo env -> pinfo
(define (local-expression-analyze-uses an-expression pinfo env)
  (local [(define defns (second an-expression))
          (define body (third an-expression))
          (define nested-pinfo (foldl (lambda (a-defn a-pinfo)
                                        (definition-analyze-uses a-defn a-pinfo))
                                      pinfo
                                      defns))]
    (pinfo-update-env 
     (expression-analyze-uses body
                              nested-pinfo
                              (pinfo-env nested-pinfo))
     (pinfo-env pinfo))))
  

(define (if-expression-analyze-uses an-expression pinfo env)
  (local [(define test (second an-expression))
          (define consequent (third an-expression))
          (define alternative (fourth an-expression))]
    (foldl (lambda (e p) (expression-analyze-uses e p env))
           pinfo 
           (list test consequent alternative))))


(define (application-expression-analyze-uses an-expression pinfo env)
  (local [(define updated-pinfo
            (foldl (lambda (e p)
                     (expression-analyze-uses e p env))
                   pinfo
                   an-expression))]
    updated-pinfo))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;






;; require-analyze: require-path -> pinfo
(define (require-analyze require-path pinfo)
  (local [(define (loop modules)
            (cond
              [(empty? modules)
               (error 'require-analyze 
                      (format "Moby doesn't know about module ~s yet"
                              require-path))]
              [(string=? require-path
                         (module-binding-source (first modules)))
               (pinfo-accumulate-module 
                (first modules)
                (pinfo-accumulate-bindings
                 (module-binding-bindings (first modules))
                 pinfo))]
              [else
               (loop (rest modules))]))]
    (loop known-modules)))


(provide/contract [program-analyze (program?  . -> . pinfo?)]
                  [program-analyze/pinfo (program? pinfo? . -> . pinfo?)])
