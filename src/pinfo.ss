#lang s-exp "lang.ss"

(require "env.ss")
(require "toplevel.ss")
(require "helpers.ss")
(require "permission.ss")
(require "modules.ss")



;; pinfo (program-info) is the "world" structure for the compilers; 
;; it captures the information we get from analyzing and compiling
;; the program, and also maintains some auxillary structures.
(define-struct pinfo (env                    ; env
                      modules                ; (listof module-binding) 
                      used-bindings-hash     ; (hashof symbol binding)
                      gensym-counter         ; number
                      ))


;; empty-pinfo: pinfo
;; An empty pinfo that doesn't know any toplevel environment bindings.
(define empty-pinfo
  (make-pinfo empty-env empty (make-immutable-hasheq empty) 0))



;; get-base-pinfo: pinfo symbol -> pinfo
;; Returns a pinfo that knows the base definitions.
;; Language can be one of the following:
;; 'base
;; 'moby
(define (get-base-pinfo language)
  ;; FIXME: currently ignores the language.  We should change this to
  ;; support different language levels.
  (cond
    [(symbol=? language 'moby)
     (make-pinfo (extend-env/module-binding toplevel-env
                                            moby-module-binding)
                 empty 
                 (make-immutable-hasheq empty) 
                 0)]
    [(symbol=? language 'base)
     (make-pinfo toplevel-env empty (make-immutable-hasheq empty) 0)]))



;; pinfo-used-bindings: pinfo -> (listof binding)
;; Returns the list of used bindings computed from the program analysis.
(define (pinfo-used-bindings a-pinfo)
  (hash-map (pinfo-used-bindings-hash a-pinfo)
            (lambda (k v) v)))


;; pinfo-update-env: pinfo env -> pinfo
;; Updates the env of a pinfo.
(define (pinfo-update-env a-pinfo an-env)
  (make-pinfo
   an-env
   (pinfo-modules a-pinfo)
   (pinfo-used-bindings-hash a-pinfo)
   (pinfo-gensym-counter a-pinfo)))


;; pinfo-accumulate-binding: binding pinfo -> pinfo
;; Adds a new binding to a pinfo's set.
(define (pinfo-accumulate-binding a-binding a-pinfo)
  (make-pinfo
   (env-extend (pinfo-env a-pinfo) a-binding)
   (pinfo-modules a-pinfo)
   (pinfo-used-bindings-hash a-pinfo)
   (pinfo-gensym-counter a-pinfo)))


;; pinfo-accumulate-bindings: (listof binding) pinfo -> pinfo
;; Adds a list of bindings to the pinfo's set.
(define (pinfo-accumulate-bindings bindings a-pinfo)
  (foldl pinfo-accumulate-binding
         a-pinfo
         bindings))


;; pinfo-accumulate-module: module-binding pinfo -> pinfo
;; Adds a module to the pinfo's set.
(define (pinfo-accumulate-module a-module a-pinfo)
  (make-pinfo (pinfo-env a-pinfo)
              (cons a-module (pinfo-modules a-pinfo))
              (pinfo-used-bindings-hash a-pinfo)
              (pinfo-gensym-counter a-pinfo)))


;; pinfo-accumulate-binding-use: binding pinfo -> pinfo
;; Adds a binding's use to a pinfo's set.
(define (pinfo-accumulate-binding-use a-binding a-pinfo)
  (make-pinfo (pinfo-env a-pinfo)
              (pinfo-modules a-pinfo)
              (hash-set (pinfo-used-bindings-hash a-pinfo)
                        (binding-id a-binding)
                        a-binding)
              (pinfo-gensym-counter a-pinfo)))


;; pinfo-gensym: pinfo symbol -> (list pinfo symbol)
;; Generates a unique symbol.
(define (pinfo-gensym a-pinfo a-label)
  (list (make-pinfo (pinfo-env a-pinfo)
                    (pinfo-modules a-pinfo)
                    (pinfo-used-bindings-hash a-pinfo)
                    (add1 (pinfo-gensym-counter a-pinfo)))

        (string->symbol
         (string-append (symbol->string a-label)
                        (number->string (pinfo-gensym-counter a-pinfo))))))

   


;; pinfo-permissions: pinfo -> (listof permission)
;; Given a pinfo, collect the list of permissions.
(define (pinfo-permissions a-pinfo)
  (local [;; unique: (listof X) -> (listof X)
          (define (unique lst)
            (cond [(empty? lst)
                   empty]
                  [(member? (first lst)
                            (rest lst))
                   (unique (rest lst))]
                  [else
                   (cons (first lst)
                         (unique (rest lst)))]))
          ;; member?: X (listof X) -> boolean
          (define (member? x lst)
            (cond
              [(empty? lst)
               false]
              [(eq? (first lst) x)
               true]
              [else
               (member? x (rest lst))]))]
    (unique
     (foldl (lambda (a-binding permissions)
              (cond [(binding:function? a-binding)
                     (append (binding:function-permissions a-binding)
                             permissions)]
                    [(binding:constant? a-binding)
                     (append (binding:constant-permissions a-binding)
                             permissions)]))
            empty
            (pinfo-used-bindings a-pinfo)))))



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
    
    [(list-begins-with? an-expression 'local)
     (local-expression-analyze-uses an-expression pinfo env)]
    
    [(list-begins-with? an-expression 'cond)
     (expression-analyze-uses (desugar-cond an-expression)
                              pinfo
                              env)]
    
    [(list-begins-with? an-expression 'if)
     (if-expression-analyze-uses an-expression pinfo env)]
    
    [(list-begins-with? an-expression 'and)
     (local [(define exprs (rest an-expression))]
       (foldl (lambda (e p) (expression-analyze-uses e p env))
              pinfo 
              exprs))]
    
    [(list-begins-with? an-expression 'or)
     (local [(define exprs (rest an-expression))]
       (foldl (lambda (e p) (expression-analyze-uses e p env))
              pinfo 
              exprs))]
    
    [(list-begins-with? an-expression 'lambda)
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
    [(list-begins-with? an-expression 'quote)
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
              [(path=? (resolve-module-path require-path false)
                       (module-binding-path (first modules)))
               (pinfo-accumulate-module 
                (first modules)
                (pinfo-accumulate-bindings
                 (module-binding-bindings (first modules))
                 pinfo))]
              [else
               (loop (rest modules))]))]
    (loop known-modules)))




(provide/contract [struct pinfo ([env env?]
                                 [modules (listof module-binding?)]
                                 [used-bindings-hash hash?]
                                 [gensym-counter number?])]
                  [empty-pinfo pinfo?]
                  [get-base-pinfo (symbol? . -> . pinfo?)]
                  [pinfo-used-bindings (pinfo? . -> . (listof binding?))]
                  [pinfo-accumulate-binding (binding? pinfo? . -> . pinfo?)]
                  [pinfo-update-env (pinfo? env? . -> . pinfo?)]
                  [pinfo-gensym (pinfo? symbol? . -> . (list/c pinfo? symbol?))]
                  [pinfo-permissions (pinfo? . -> . (listof permission?))]

                  [program-analyze (program?  . -> . pinfo?)]
                  [program-analyze/pinfo (program? pinfo? . -> . pinfo?)])
