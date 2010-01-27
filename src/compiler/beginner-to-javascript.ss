#lang s-exp "lang.ss"

;; This program translates beginner-level languages into Javascript.
;; We pattern match against the language given by:
;;
;; http://docs.plt-scheme.org/htdp-langs/beginner.html


(require "env.ss")
(require "pinfo.ss")
(require "analyzer.ss")
(require "helpers.ss")
(require "desugar.ss")
(require "labeled-translation.ss")
(require "rbtree.ss")
(require "version.ss")
(require "../collects/runtime/binding.ss")
(require "../collects/runtime/stx.ss")
(require "../collects/runtime/error-struct.ss")
(require "../collects/runtime/arity-struct.ss")


;; A compiled program is a:
(define-struct compiled-program
  (defns           ;; string
    toplevel-exprs ;; string
    pinfo          ;; pinfo
    ))


;; compiled-program-main: compiled-program ->string
;; Produces an output of the compiled program as a Javascript string to be evaluated.
;; No bindings should be exposed, as the toplevel definitions are wrapped lexically.
(define (compiled-program-main a-compiled-program)
  (string-append "(function() { "
                 (compiled-program-defns a-compiled-program)
                 "\n"
                 "return (function() { \n"
                 "  (" 
                 (compiled-program-toplevel-exprs a-compiled-program)
                 "  )(arguments[0] || plt.Kernel.identity);\n"
                 "}); })()"))


;; compiled-program-main/expose: compiled-program -> string
;; Like compiled-program-main, but exposes all of the provided definitions to the toplevel.
(define (compiled-program-main/expose a-compiled-program)
  (local [(define defined-names

            (expose-provided-names a-compiled-program)
            ;; FIXME: must expose only the provided names
            #;(rbtree-fold (pinfo-defined-names
                            (compiled-program-pinfo a-compiled-program))
                           (lambda (name binding acc)
                             (cons name acc))
                           empty))]
    (begin
      (string-append "(function(_that) {"
                     (compiled-program-defns a-compiled-program)
                     "\n"
                     "(function() { \n"
                     "  (" 
                     (compiled-program-toplevel-exprs a-compiled-program)
                     "  )(arguments[0] || plt.Kernel.identity);\n"
                     (apply string-append (map (lambda (a-name)
                                                 (local [(define munged-name (identifier->munged-java-identifier a-name))]
                                                   (format "_that[~s] = ~a;\n"
                                                           (symbol->string munged-name)
                                                           munged-name
                                                           )))
                                               defined-names))
                     "})();"
                     "})(this);\n"))))



;; compiled-program-main/expose-as-module: compiled-program symbol -> string
;; Like compiled-program-main, but constructs a string that, when evaluated, installs
;; a module into plt._MODULES.
(define (compiled-program-main/expose-as-module a-compiled-program module-name)
  (local [(define module-name-string (symbol->string module-name))
          (define defined-names (expose-provided-names a-compiled-program))]
    (string-append  "if (typeof(plt) == 'undefined') { plt = {}; }\n"
                    "if (typeof(plt._MODULES) == 'undefined') { plt._MODULES = {}; }\n"
                    "if (typeof(plt._MODULES[" (format "~s" module-name-string) "]) == 'undefined') {\n"
                    "    plt._MODULES[" (format "~s" module-name-string) "] = "
                    "        { COMPILER_VERSION: " (format "~s" VERSION) ",\n\tBINDINGS: {},\n\tEXPORTS : {}};\n"

                    "    (function() {\n"
                    ""       (compiled-program-defns a-compiled-program) "\n"
                    "        (" (compiled-program-toplevel-exprs a-compiled-program) ")(function(x){return x;});\n"
                    ;; FIXME: export the BINDINGS value that describes each exported value.
                    ""       (apply string-append (map (lambda (a-name)
                                                (local [(define munged-name
                                                          (identifier->munged-java-identifier a-name))]
                                                  (format "plt._MODULES[~s].EXPORTS[~s] = ~a;\n"
                                                          module-name-string
                                                          (symbol->string munged-name)
                                                          munged-name
                                                          )))
                                              defined-names))
                    "     }());\n"
                    "}\n")))




;; expose-provided-names: compiled-program -> (listof symbol)
;; Get all the names of the provided identifiers.

;; FIXME: use pinfo-get-exposed-bindings
(define (expose-provided-names a-compiled-program)
  (rbtree-fold (pinfo-provided-names (compiled-program-pinfo a-compiled-program))
               (lambda (name binding acc)
                 (append (expose-provided-names/provide-binding binding a-compiled-program)
                         acc))
               empty))


;; expose-provided-names: provide-binding compiled-program -> (listof symbol)
;; Get all the names of the identifiers provided by the provide-binding.
(define (expose-provided-names/provide-binding a-provide-binding a-compiled-program)
  (cond
    [(provide-binding:id? a-provide-binding)
     (list (binding-id (lookup-provide-binding-in-definition-bindings a-provide-binding 
                                                                      a-compiled-program)))]

    [(provide-binding:struct-id? a-provide-binding)
     (local [(define a-binding (lookup-provide-binding-in-definition-bindings a-provide-binding 
                                                                              a-compiled-program))]
       (cond
         [(binding:structure? a-binding)
          (append (list (binding:structure-name a-binding))
                  (list (binding:structure-constructor a-binding))
                  (list (binding:structure-predicate a-binding))
                  (binding:structure-accessors a-binding)
                  (binding:structure-mutators a-binding))]
         [else
          (raise (make-moby-error (stx-loc (provide-binding-stx a-binding))
                                  (make-moby-error-type:provided-structure-not-structure (stx-e (provide-binding-stx a-binding)))))]))]))


;; lookup-provide-binding-in-definition-bindings: provide-binding compiled-program -> binding
;; Lookup the provided bindings.
(define (lookup-provide-binding-in-definition-bindings a-provide-binding a-compiled-program)
  (local [(define list-or-false
            (rbtree-lookup symbol<
                           (pinfo-defined-names (compiled-program-pinfo a-compiled-program))
                           (stx-e (provide-binding-stx a-provide-binding))))]
    (cond
      [(list? list-or-false)
       (second list-or-false)]
      [else
       (raise (make-moby-error (stx-loc (provide-binding-stx a-provide-binding))
                               (make-moby-error-type:provided-name-not-defined 
                                (stx-e (provide-binding-stx a-provide-binding)))))])))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



;; program->compiled-program: program -> compiled-program
;; Consumes a program and returns a compiled program.
;; If pinfo is provided, uses that as the base set of known toplevel definitions.
(define (program->compiled-program program)
  (program->compiled-program/pinfo program 
                                   (get-base-pinfo 'base)))


;; program->compiled-program/pinfo: program pinfo -> compiled-program
;; Consumes a program and returns a compiled program.
;; The provided pinfo is used as the base set of known toplevel definitions.
(define (program->compiled-program/pinfo program input-pinfo)
  (program->compiled-program/pinfo/at-toplevel? program input-pinfo true))


;; program->compiled-program/pinfo/at-toplevel?: program pinfo -> compiled-program
;; Consumes a program and returns a compiled program.
;; The provided pinfo is used as the base set of known toplevel definitions.
;; If not at toplevel, we don't produce the set of shared definitions as a part
;; of the output.
(define (program->compiled-program/pinfo/at-toplevel? program input-pinfo at-toplevel?)
  (local [(define pinfo-1+gensym (pinfo-gensym input-pinfo 'toplevel-expression-show))
          (define toplevel-expression-show (second pinfo-1+gensym))
          
          (define desugared-program+pinfo (desugar-program program (first pinfo-1+gensym)))
          
          (define a-pinfo (program-analyze/pinfo (first desugared-program+pinfo)
                                                 (second desugared-program+pinfo)))
          (define toplevel-env (pinfo-env a-pinfo))
          
          (define (collect-shared-expression-translation-definitions a-pinfo)
            (rbtree-fold 
             (pinfo-shared-expressions a-pinfo)
             (lambda (an-expression a-labeled-translation acc)
               (string-append (format "_SHARED[~a] = ~a;\n"
                                      (labeled-translation-label
                                       a-labeled-translation)
                                      (labeled-translation-translation 
                                       a-labeled-translation)) 
                              acc))
             ""))
          
          
          (define (loop program defns tops a-pinfo)
            (cond [(empty? program)
                   
                   (make-compiled-program 
                    (cond [at-toplevel?
                           (string-append "var _SHARED = {};"
                                          defns
                                          (collect-shared-expression-translation-definitions a-pinfo))]
                          [else
                           defns])
                    (string-append "(function (" 
                                   (symbol->string
                                    (identifier->munged-java-identifier
                                     toplevel-expression-show))
                                   ") { " tops " })") 
                    a-pinfo)]
                  [else
                   (cond [(defn? (first program))
                          (local [(define defn-string+expr-string+pinfo
                                    (definition->javascript-strings 
                                      (first program) 
                                      toplevel-env
                                      a-pinfo))]
                            
                            (loop (rest program)
                                  (string-append defns
                                                 "\n"
                                                 (first defn-string+expr-string+pinfo))
                                  (string-append tops
                                                 "\n"
                                                 (second defn-string+expr-string+pinfo))
                                  (third defn-string+expr-string+pinfo)))]
                         
                         
                         [(library-require? (first program))
                          (loop (rest program)
                                defns
                                (string-append
                                 tops
                                 "\n"
                                 (module-require->javascript-string (first program)
                                                                   a-pinfo)
                                 ";\n")
                                a-pinfo)]

                         [(provide-statement? (first program))
                          (loop (rest program)
                                defns
                                tops
                                a-pinfo)]
                         
                         [(or (test-case? (first program))
                              (expression? (first program)))
                          (local [(define expression-string+pinfo
                                    (expression->javascript-string 
                                     (first program) 
                                     toplevel-env
                                     a-pinfo))]
                            
                            (loop (rest program)
                                  defns
                                  (string-append tops
                                                 "\n"
                                                 ;; NOTE: we must do something special
                                                 ;; for toplevel expressions so the user
                                                 ;; can see the values.  The toplevel expression is 
                                                 ;; evaluated and its value passed to the
                                                 ;; toplevel-expression-show function.
                                                 (symbol->string 
                                                  (identifier->munged-java-identifier
                                                   toplevel-expression-show))
                                                 "("
                                                 (first expression-string+pinfo)
                                                 ");")
                                  (second expression-string+pinfo)))])]))]
    
    (loop (first desugared-program+pinfo) "" "" a-pinfo)))


(define (module-require->javascript-string a-module-require a-pinfo)
  (string-join (map (lambda (a-path)
                      (format "plt.Kernel.invokeModule('~a');"
                              ((pinfo-module-path-resolver a-pinfo) (stx-e a-path)
                                                                    (pinfo-current-module-path a-pinfo))))
                    (rest (stx-e a-module-require)))
               "\n"))



;; definition->java-string: definition env pinfo -> (list string string pinfo)
;; Consumes a definition (define or define-struct) and produces two strings.
;; The first maps a definitions string.
;; The second value is the expression that will be evaluated at the toplevel.
;;
;; Structure definitions map to static inner classes with transparent fields.
(define (definition->javascript-strings defn env a-pinfo)
  (case-analyze-definition 
   defn
   (lambda (fun args body)
     (function-definition->java-string fun args body env a-pinfo))
   (lambda (id body)
     (variable-definition->javascript-strings id body env a-pinfo))
   (lambda (id fields)
     (struct-definition->javascript-string id fields env a-pinfo))))




;; function-definition->java-string: symbol-stx (listof symbol-stx) expr env pinfo -> (list string string pinfo)
;; Converts the function definition into a static function declaration whose
;; return value is an object.
(define (function-definition->java-string fun args body env a-pinfo)
  (local [(define munged-fun-id
            (identifier->munged-java-identifier (stx-e fun)))
          (define munged-arg-ids
            (map (lambda (id) (identifier->munged-java-identifier (stx-e id)))
                 args))
          (define new-env 
            (env-extend-function env (stx-e fun) false (length args) false
                                 (symbol->string munged-fun-id)))
          (define env-with-arg-bindings
            (foldl (lambda (arg-id env) 
                     (env-extend env (make-binding:constant (stx-e arg-id)
                                                            (symbol->string
                                                             (identifier->munged-java-identifier
                                                              (stx-e arg-id)))
                                                            empty)))
                   new-env
                   args))
          
          (define body-string+pinfo 
            (expression->javascript-string body env-with-arg-bindings a-pinfo))
          (define body-string (first body-string+pinfo))
          (define updated-pinfo (second body-string+pinfo))]
    (begin
      (check-duplicate-identifiers! (cons fun args))
      (list 
       (string-append "var " (symbol->string munged-fun-id) " = function("
                      (string-join (map (lambda (arg-id)
                                          (symbol->string arg-id))
                                        munged-arg-ids)
                                   ", ")
                      ") { return " body-string "; };"
                      )
       ""
       updated-pinfo))))


;; variable-definition->javascript-strings: symbol-stx expr env pinfo -> (list string string pinfo)
;; Converts the variable definition into a static variable declaration and its
;; initializer at the toplevel.
(define (variable-definition->javascript-strings id body env a-pinfo)
  (local [(define munged-id (identifier->munged-java-identifier (stx-e id)))
          (define new-env (env-extend env 
                                      (make-binding:constant 
                                       (stx-e id)
                                       (symbol->string munged-id)
                                       empty)))
          (define str+p (expression->javascript-string body new-env a-pinfo))]
    (list (string-append "var "
                         (symbol->string munged-id)
                         "; ")
          
          (string-append (symbol->string munged-id)
                         " = "
                         (first str+p)
                         ";")
          (second str+p))))




;; struct-definition->javascript-string: symbol-stx (listof symbol-stx) env pinfo -> (list string string pinfo)
(define (struct-definition->javascript-string id fields env a-pinfo)
  (local [
          ;; field->accessor-name: symbol symbol -> symbol
          ;; Given a structure name and a field, return the accessor.
          (define (field->accessor-name struct-name field-name)
            (string->symbol
             (string-append (symbol->string struct-name)
                            "-"
                            (symbol->string field-name))))
          
          
          (define pinfo-1+gensym (pinfo-gensym a-pinfo 'fresh-struct-name))
          (define updated-pinfo (first pinfo-1+gensym))
          
          ;; predicate-name: string
          (define predicate-name 
            (symbol->string (identifier->munged-java-identifier 
                             (string->symbol (string-append (symbol->string (stx-e id))
                                                            "?")))))
          
          ;; make-unmunged-accessor-name: symbol -> string
          (define (make-unmunged-accessor-name a-field)
            (string-append (symbol->string (stx-e id))
                           "-"
                           (symbol->string a-field)))
          
          ;; make-accessor-name: symbol -> string
          (define (make-accessor-name a-field)
            (symbol->string
             (identifier->munged-java-identifier
              (string->symbol
               (make-unmunged-accessor-name a-field)))))
          
          ;; make-mutator-name: symbol -> string
          (define (make-mutator-name a-field)
            (string-append "set_dash_" (make-accessor-name a-field) "_bang_"))]
    (begin
      (check-duplicate-identifiers! fields)
      (list (string-append
             
             ;; default constructor
             (string-append "var "(symbol->string (identifier->munged-java-identifier (stx-e id)))
                            " = function ("
                            (string-join (map (lambda (i) (symbol->string
                                                           (identifier->munged-java-identifier 
                                                            (stx-e i))))
                                              fields)
                                         ",")
                            ") { "
                            (format "plt.types.Struct.call(this, ~s, [~a]);"
                                    (string-append "make-" (symbol->string (stx-e id)))
                                    (string-join (map (lambda (i) (symbol->string
                                                                   (identifier->munged-java-identifier
                                                                    (stx-e i))))
                                                      fields)
                                                 ","))
                            (string-join (map (lambda (i) (string-append "this."
                                                                         (symbol->string 
                                                                          (identifier->munged-java-identifier (stx-e i)))
                                                                         " = "
                                                                         (symbol->string 
                                                                          (identifier->munged-java-identifier (stx-e i)))
                                                                         ";"))
                                              fields) 
                                         "\n")
                            
                            " };\n"
                            
                            (symbol->string (identifier->munged-java-identifier (stx-e id)))
                            ".prototype = new plt.types.Struct();\n"
                            
                            )
             
             "\n"
             
             
             ;; make-id
             (string-append "var " (local [(define make-id (string->symbol 
                                                            (string-append "make-" (symbol->string (stx-e id)))))]
                                     (symbol->string (identifier->munged-java-identifier make-id)))
                            " = function "
                            "(" (string-join (build-list (length fields) (lambda (i) 
                                                                           (string-append "id" (number->string i))))
                                             ",")
                            ") { return new "
                            (symbol->string (identifier->munged-java-identifier (stx-e id)))
                            "("
                            (string-join (build-list (length fields) (lambda (i) 
                                                                       (string-append "id" (number->string i))))
                                         ",")
                            "); };")
             
             "\n"
             
             ;; accessors
             (string-join 
              (map (lambda (a-field)
                     (string-append "var " (make-accessor-name (stx-e a-field)) " = function(obj) {\n"
                                    "     if (" predicate-name" (obj)) {\n"
                                    "        return obj." (symbol->string (identifier->munged-java-identifier (stx-e a-field))) ";\n"
                                    "     } else {\n"
                                    "        throw new plt.Kernel.MobyRuntimeError("
                                    "            plt.Kernel.format('" (make-unmunged-accessor-name (stx-e a-field)) ": not a " (symbol->string (stx-e id)) ": ~s', [obj]));\n"
                                    "     }\n"
                                    "};\n"))
                   fields)
              "\n")
             
             "\n"
             
             ;; mutators
             (string-join 
              (mapi (lambda (a-field an-index)
                      (string-append "var " (make-mutator-name (stx-e a-field)) " = function(obj,newVal) {\n"
                                     "	 if (" predicate-name" (obj)) {\n"
                                     "		obj." (symbol->string (identifier->munged-java-identifier (stx-e a-field))) " = newVal;\n"
                                     "           obj._fields[" (number->string an-index) "] = newVal;"
                                     "     } else {\n"
                                     "        throw new plt.Kernel.MobyRuntimeError("
                                     "            plt.Kernel.format('" (make-mutator-name (stx-e a-field)) ": not a " (symbol->string (stx-e id)) ": ~s', [obj]));\n"
                                     "     }\n"
                                     "};\n"))
                    fields)
              "\n")
             
             "\n"
             
             ;; structure predicate
             (string-append "var " predicate-name " = function(obj) { 
              return obj != null && obj != undefined && obj instanceof "
                            (symbol->string (identifier->munged-java-identifier 
                                             (stx-e id)))
                            "; };\n"))
            
            "" ;; no introduced toplevel expressions
            updated-pinfo))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; expression->java-string: expr-stx env pinfo -> (list string pinfo)
;; Translates an expression into a Java expression string whose evaluation
;; should produce an Object.
(define (expression->javascript-string expr env a-pinfo)
  (cond
    [(expression-sharable? expr a-pinfo)
     (sharable-expression->javascript-string expr env a-pinfo)]
    [else
     (unsharable-expression->javascript-string expr env a-pinfo)]))


;; Translates an expression into a Java expression string whose evaluation
;; should produce an Object.
(define (sharable-expression->javascript-string expr env a-pinfo)
  (cond
    [(rbtree-member? expression<?
                     (pinfo-shared-expressions a-pinfo)
                     expr)
     (list (format "_SHARED[~a]" (labeled-translation-label
                                  (second (rbtree-lookup 
                                           expression<?
                                           (pinfo-shared-expressions a-pinfo)
                                           expr))))
           a-pinfo)]
    [else
     (local [(define translation+pinfo
               (unsharable-expression->javascript-string expr env a-pinfo))
             (define updated-pinfo
               (pinfo-accumulate-shared-expression expr
                                                   (first translation+pinfo)
                                                   (second translation+pinfo)))]
       (sharable-expression->javascript-string expr env updated-pinfo))]))



;; Translates an expression into a Java expression string whose evaluation
;; should produce an Object.
(define (unsharable-expression->javascript-string expr env a-pinfo)
  (cond
    ;; (local ([define ...] ...) body)
    [(stx-begins-with? expr 'local)
     (local [(define defns (stx-e (second (stx-e expr))))
             (define body (third (stx-e expr)))]
       (local-expression->javascript-string defns body env a-pinfo))]
    
    ;; (begin ...)
    [(stx-begins-with? expr 'begin)
     (local [(define exprs (rest (stx-e expr)))]
       (begin-sequence->javascript-string expr exprs env a-pinfo))]
    
    ;; (set! identifier value)
    ;; Attention: it's evaluation doesn't produce an Object
    [(stx-begins-with? expr 'set!)
     (local [(define id (second (stx-e expr)))
             (define value (third (stx-e expr)))]
       (set!-expression->javascript-string id value env a-pinfo))]
    
    
    ;; (if test consequent alternative)
    [(stx-begins-with? expr 'if)
     (local [(define test (second (stx-e expr)))
             (define consequent (third (stx-e expr)))
             (define alternative (fourth (stx-e expr)))]
       (if-expression->javascript-string test consequent alternative env a-pinfo))]
    
    
    ;; (and exprs ...)
    [(stx-begins-with? expr 'and)
     (local [(define exprs (rest (stx-e expr)))]
       (boolean-chain->javascript-string "&&" exprs env a-pinfo))]
    
    ;; (or exprs ...)
    [(stx-begins-with? expr 'or)
     (local [(define exprs (rest (stx-e expr)))]
       (boolean-chain->javascript-string "||" exprs env a-pinfo))]
    
    ;; (lambda (args ...) body)
    [(stx-begins-with? expr 'lambda)
     (local [(define args (stx-e (second (stx-e expr))))
             (define body (third (stx-e expr)))]
       (lambda-expression->javascript-string expr args body env a-pinfo))]
    
    ;; Numbers
    [(number? (stx-e expr))
     (list
      (number->javascript-string (stx-e expr) expr)
      a-pinfo)]
    
    ;; Strings
    [(string? (stx-e expr))
     (list (string->javascript-string (stx-e expr))
           a-pinfo)]
    
    ;; Literal booleans
    [(boolean? (stx-e expr))
     (list (boolean->javascript-string (stx-e expr))
           a-pinfo)
     #;(expression->javascript-string (if (stx-e expr) 
                                        (make-stx:atom 'true (stx-loc expr))
                                        (make-stx:atom 'false (stx-loc expr)))
                                    env
                                    a-pinfo)]
    ;; Characters
    [(char? (stx-e expr))
     (list (char->javascript-string (stx-e expr))
           a-pinfo)]
    
    ;; Identifiers
    [(symbol? (stx-e expr))
     (list
      (identifier-expression->javascript-string expr env)
      a-pinfo)]
    
    ;; Quoted datums
    [(stx-begins-with? expr 'quote)
     (list (quote-expression->javascript-string (second (stx-e expr)))
           a-pinfo)]
    
    ;; Function call/primitive operation call
    [(pair? (stx-e expr))
     (local [(define operator (first (stx-e expr)))
             (define operands (rest (stx-e expr)))]
       (application-expression->javascript-string expr operator operands env a-pinfo))]))






;; expressions->javascript-strings: (listof expr-stx) env pinfo -> (list (listof string) pinfo)
;; Computes the string representation of all of the expressions, and returns those
;; as well as the updated pinfo.
(define (expressions->javascript-strings expressions env a-pinfo)
  (local [(define strings/rev+pinfo
            (foldl (lambda (e ss+p)
                     (local [(define new-string+p
                               (expression->javascript-string e env (second ss+p)))]
                       (list (cons (first new-string+p) 
                                   (first ss+p))
                             (second new-string+p))))
                   (list empty a-pinfo)
                   expressions))]
    (list (reverse (first strings/rev+pinfo))
          (second strings/rev+pinfo))))


;; set!-expression->javascript-string: expr-stx expr-stx env pinfo -> (list string pinfo)
(define (set!-expression->javascript-string id-stx newVal-stx env a-pinfo)
  (cond
    [(not (symbol? (stx-e id-stx))) 
     (raise (make-moby-error (stx-loc id-stx)
                             (make-moby-error-type:expected-identifier id-stx)))]
    [else
     (local [(define es+p
               (expressions->javascript-strings (list id-stx newVal-stx)
                                                env 
                                                a-pinfo))
             
             (define idExprString (first (first es+p)))
             (define valExprString (second (first es+p)))]
       (list (string-append "(function(){ \n"
                            idExprString
                            " = "
                            valExprString
                            ";})()")
             (second es+p)))]))


;; begin-sequence->javascript-string: expr-stx (listof expr-stx) env pinfo -> (list string pinfo)
(define (begin-sequence->javascript-string original-stx exprs env a-pinfo)
  (cond
    [(empty? exprs)
     (raise (make-moby-error (stx-loc original-stx)
                             (make-moby-error-type:missing-expression 'begin)))]
    [else
     (local [;; split-last-element: (listof any) -> (listof (listof any) any)
             ;; (split-last-element (list x y z)) --> (list (list x y) z)
             (define (split-last-element ls)
               (list (reverse (rest (reverse ls))) 
                     (first (reverse ls))))
             
             (define strings+pinfo
               (expressions->javascript-strings exprs env a-pinfo))
             
             (define exprs+last-expr
               (split-last-element (first strings+pinfo)))]
       (list (string-append "(function(){"
                            (string-join (first exprs+last-expr) ";\n")
                            ";\n"
                            "return "
                            (second exprs+last-expr) ";"
                            "})()")
             (second strings+pinfo)))]))



;; if-expression->javascript-string: expr expr expr env pinfo -> (list string pinfo)
(define (if-expression->javascript-string test consequent alternative env a-pinfo)  
  (local [(define es+p 
            (expressions->javascript-strings (list test consequent alternative)
                                             env 
                                             a-pinfo))
          (define s1 (first (first es+p)))
          (define s2 (second (first es+p)))
          (define s3 (third (first es+p)))]
    (list
     (string-append "(" s1 " ?\n " s2 " :\n " s3 ")")
     (second es+p))))


;; quote-expression->javascript-string: expr -> string
(define (quote-expression->javascript-string expr)
  (cond
    [(empty? (stx-e expr))
     "plt.types.Empty.EMPTY"]
    
    [(pair? (stx-e expr))
     (string-append "(plt.Kernel.list(["
                    (string-join 
                     (map quote-expression->javascript-string (stx-e expr))
                     ",")
                    "]))")]
    
    [(symbol? (stx-e expr))
     (string-append "(plt.types.Symbol.makeInstance(\""
                    (symbol->string (stx-e expr))
                    "\"))")]
    
    ;; Numbers
    [(number? (stx-e expr))
     (number->javascript-string (stx-e expr) expr)]
    
    ;; Strings
    [(string? (stx-e expr))
     (string->javascript-string (stx-e expr))]
    
    ;; Characters
    [(char? (stx-e expr))
     (char->javascript-string (stx-e expr))]
    
    ;; Booleans
    [(boolean? (stx-e expr))
     (boolean->javascript-string (stx-e expr))]
    
    [else
     ;; FIXME: This should never happen; all program values should be quotable.
     (raise (make-moby-error (stx-loc expr)
                             (make-moby-error-type:generic-syntactic-error 
                              (format "Unknown unquotable expression encountered: ~s" (stx->datum expr))
                              (list))))]))



;; boolean->javascript-string: boolean -> string
(define (boolean->javascript-string a-bool)
  (cond
    [a-bool
     "plt.types.Logic.TRUE"]
    [else
     "plt.types.Logic.FALSE"]))



;; boolean-chain->javascript-string: string (listof expr) env pinfo -> (list string pinfo)
(define (boolean-chain->javascript-string joiner exprs env a-pinfo)
  (local [(define strings+pinfo
            (expressions->javascript-strings exprs env a-pinfo))]
    (list (string-append "(" (string-join (first strings+pinfo) joiner) ")")
          (second strings+pinfo))))



;; local-expression->javascript-string: (listof defn) expr env pinfo -> (list string pinfo)
(define (local-expression->javascript-string defns body env a-pinfo)
  (local [(define inner-compiled-program 
            (program->compiled-program/pinfo/at-toplevel? defns
                                                          (pinfo-update-env a-pinfo env)
                                                          false))
          (define inner-body-string+pinfo
            (expression->javascript-string 
             body
             (pinfo-env (compiled-program-pinfo inner-compiled-program))
             (compiled-program-pinfo inner-compiled-program)))
          
          (define inner-body-string (first inner-body-string+pinfo))
          (define updated-pinfo (second inner-body-string+pinfo))]
    (list (string-append "((function() { \n"
                         (compiled-program-defns inner-compiled-program)
                         "\n"
                         ;; Apply the toplevel expressions with the identity function.
                         (compiled-program-toplevel-exprs inner-compiled-program) "(plt.Kernel.identity)"
                         "\n"
                         "return " inner-body-string ";
              })())")
          (pinfo-update-defined-names 
           (pinfo-update-env updated-pinfo (pinfo-env a-pinfo))
           (pinfo-defined-names a-pinfo)))))



;; maybe-emit-location-mark: string loc pinfo -> string
;; Provisionally add the mark for the last location.
(define (maybe-emit-location-mark a-str a-loc a-pinfo)
  (cond
    [(pinfo-with-location-emits? a-pinfo)
     (format "(plt.Kernel.setLastLoc(~a) && ~a)"
             (Loc->javascript-string a-loc)
             a-str)]
    [else
     a-str]))



;; application-expression->java-string: symbol-stx (listof expr) env pinfo -> (list string pinfo)
;; Converts the function application to a string.
(define (application-expression->javascript-string original-stx operator operands env a-pinfo)
  (cond 
    ;; Special case: when the operator is named
    [(and (symbol? (stx-e operator))
          (not (env-contains? env (stx-e operator))))
     (raise (make-moby-error (stx-loc operator)
                             (make-moby-error-type:undefined-identifier (stx-e operator))))]
    
    [(symbol? (stx-e operator))
     (local [(define operator-binding (env-lookup env (stx-e operator)))
             (define operand-strings+pinfo
               (expressions->javascript-strings operands env a-pinfo))
             
             (define operand-strings (first operand-strings+pinfo))
             (define updated-pinfo (second operand-strings+pinfo))]
       (cond
         
         [(binding:constant? operator-binding)
          (list 
           (maybe-emit-location-mark (string-append "plt.Kernel.apply(" (binding:constant-java-string operator-binding) ", "
                                                    "                    plt.Kernel.list([" (string-join operand-strings ", ") "]),"
                                                    "                    [])")
                                     (stx-loc original-stx)
                                     updated-pinfo)
           updated-pinfo)]
         
         [(binding:function? operator-binding)
          (cond
            [(< (length operands)
                (binding:function-min-arity operator-binding))
             (raise (make-moby-error (stx-loc original-stx)
                                     (make-moby-error-type:application-arity (stx-e operator)
                                                                             (make-arity:fixed (binding:function-min-arity operator-binding))
                                                                             (length operands))))]
            [(binding:function-var-arity? operator-binding)
             (cond [(> (binding:function-min-arity operator-binding) 0)
                    (list 
                     (maybe-emit-location-mark (string-append (binding:function-java-string operator-binding)
                                                              "("
                                                              (string-join (take operand-strings (binding:function-min-arity operator-binding)) ",")
                                                              ", ["
                                                              (string-join (list-tail operand-strings (binding:function-min-arity operator-binding))
                                                                           ",")
                                                              "])")
                                               (stx-loc original-stx)
                                               updated-pinfo)
                     updated-pinfo)]
                   [else
                    (list
                     (maybe-emit-location-mark (string-append (binding:function-java-string operator-binding) 
                                                              "(["
                                                              (string-join operand-strings ",")
                                                              "])")
                                               (stx-loc original-stx)
                                               updated-pinfo)
                     updated-pinfo)])]
            [else
             (cond
               [(> (length operands)
                   (binding:function-min-arity operator-binding))
                (raise (make-moby-error (stx-loc original-stx)
                                        (make-moby-error-type:application-arity (stx-e operator)
                                                                                (make-arity:fixed 
                                                                                 (binding:function-min-arity operator-binding))
                                                                                (length operands))))]
               [else
                (list 
                 (maybe-emit-location-mark (string-append (binding:function-java-string operator-binding)
                                                          "(" (string-join operand-strings ",") ")")
                                           (stx-loc original-stx)
                                           updated-pinfo)
                 updated-pinfo)])])]
         [(binding:structure? operator-binding)
          (raise (make-moby-error (stx-loc original-stx)
                                  (make-moby-error-type:application-operator-not-a-function 
                                   (stx-e operator))))]))]
    
    ;; General application
    [else
     (local [(define expression-strings+pinfo
               (expressions->javascript-strings (cons operator operands)
                                                env
                                                a-pinfo))
             (define operator-string (first (first expression-strings+pinfo)))
             (define operand-strings (rest (first expression-strings+pinfo)))
             (define updated-pinfo (second expression-strings+pinfo))]
       (list
        (maybe-emit-location-mark (string-append "plt.Kernel.apply(" operator-string ", "
                                                 "                   plt.Kernel.list([" (string-join operand-strings ", ") "]), "
                                                 "                   [])")
                                  (stx-loc original-stx)
                                  updated-pinfo)
        updated-pinfo))]))



;; identifier-expression->javascript-string: symbol-stx env -> string
;; Translates the use of a toplevel identifier to the appropriate
;; Java code.
(define (identifier-expression->javascript-string an-id an-env)
  (cond
    [(not (env-contains? an-env (stx-e an-id)))
     (raise (make-moby-error (stx-loc an-id)
                             (make-moby-error-type:undefined-identifier 
                              (stx-e an-id))))]
    [else     
     (local [(define binding (env-lookup an-env (stx-e an-id)))]
       (cond
         [(binding:constant? binding)
          (binding:constant-java-string binding)]
         [(binding:function? binding)
          (cond
            [(binding:function-var-arity? binding)
             (string-append "("
                            "plt.types.liftToplevelToFunctionValue(" 
                            (binding:function-java-string binding) ","
                            (string->javascript-string (symbol->string (binding-id binding))) ","
                            (number->string (binding:function-min-arity binding)) ","
                            "plt.Kernel.list([plt.types.Symbol.makeInstance('at-least'), " (rational-number->javascript-string (binding:function-min-arity binding)) "])"
                            "))")
             #;(string-append "((function() { var _result_ = (function(_args_) {
                    return " (binding:function-java-string binding)
                             "    .apply(null, _args_.slice(0, " (number->string (binding:function-min-arity binding)) 
                             "                        ).concat([_args_.slice("(number->string (binding:function-min-arity binding))")])); });"
                             "_result_._eqHashCode = plt.types.makeEqHashCode();"                      
                             "_result_.isEqual = function(other, cache) { return this === other; };"
                             "_result_.toWrittenString = function(cache) {return '<function:" (symbol->string (binding-id binding)) ">'; };"
                             "_result_.toDisplayedString = _result_.toWrittenString;"
                             "_result_.procedureArity = plt.Kernel.list([plt.types.Symbol.makeInstance('at-least'), " (rational-number->javascript-string (binding:function-min-arity binding)) "]);"
                             "return _result_; })())")]
            [else
             (string-append "("
                            "plt.types.liftToplevelToFunctionValue(" 
                            (binding:function-java-string binding) ","
                            (string->javascript-string (symbol->string (binding-id binding))) ","
                            (number->string (binding:function-min-arity binding)) ","
                            (rational-number->javascript-string (binding:function-min-arity binding))
                            "))")

             #;(string-append "(function() { var _result_ = (function(_args_) {
                    return " (binding:function-java-string binding)
                             "("
                             (string-join (map (lambda (i)
                                                 (string-append "_args_[" (number->string i)"]"))
                                               (range (binding:function-min-arity binding)))
                                          ", ")
                             ");});"
                             "_result_._eqHashCode = plt.types.makeEqHashCode();"
                             "_result_.isEqual = function(other, cache) { return this === other; };"
                             "_result_.toWrittenString = function(cache) {return '<function:"(symbol->string (binding-id binding))">'; };"
                             "_result_.toDisplayedString = _result_.toWrittenString; "
                             "_result_.procedureArity = " (rational-number->javascript-string (binding:function-min-arity binding)) ";"
                             "return _result_; })()")])]
         [(binding:structure? binding)
          (raise (make-moby-error (stx-loc an-id)
                                  (make-moby-error-type:structure-identifier-not-expression (stx-e an-id))))]))]))





;; lambda-expression->javascript-string stx (listof symbol-stx) expression env pinfo -> string
(define (lambda-expression->javascript-string original-stx args body env a-pinfo)
  (local [;; mapi: (X number -> Y) (listof X) -> (listof Y)
          (define (mapi f elts)
            (local [(define (loop i elts)
                      (cond
                        [(empty? elts)
                         empty]
                        [else
                         (cons (f (first elts) i)
                               (loop (add1 i) (rest elts)))]))]
              (loop 0 elts)))
          
          (define munged-arg-ids
            (map (lambda (id) (identifier->munged-java-identifier (stx-e id)))
                 args))
          
          (define new-env
            (foldl (lambda (arg-id env) 
                     (env-extend env 
                                 (make-binding:constant 
                                  (stx-e arg-id)
                                  (symbol->string
                                   (identifier->munged-java-identifier (stx-e arg-id)))
                                  empty)))
                   env
                   args))
          
          (define pinfo+args-sym
            (pinfo-gensym a-pinfo 'args))          
          
          (define a-pinfo-2 (first pinfo+args-sym))
          (define args-sym (second pinfo+args-sym))
          
          (define body-string+p
            (expression->javascript-string body new-env a-pinfo-2))
          (define body-string (first body-string+p))
          (define updated-pinfo (second body-string+p))]
    (begin
      (check-duplicate-identifiers! args)
      (list
       (string-append "((function() {\n"
                      "   var _result_ = (function(" (symbol->string args-sym) ") {\n"
                      (string-join (mapi (lambda (arg-id i)
                                           (string-append "var "
                                                          (symbol->string arg-id)
                                                          " = "
                                                          (symbol->string args-sym)
                                                          "[" (number->string i) "];"))
                                         munged-arg-ids)
                                   "\n")
                      "  return " body-string "; });"
                      "_result_._eqHashCode = plt.types.makeEqHashCode();"
                      "_result_.toWrittenString = function (cache) { return '<function:lambda>'; };"
                      "_result_.isEqual = function(other, cache) { return this === other; };"
                      "_result_.procedureArity = " (rational-number->javascript-string (length args)) ";"
                      "_result_.toDisplayedString = _result_.toWrittenString;"
                      "return _result_;  })())")
       updated-pinfo))))


;; Loc->javascript-string: Loc -> string
;; Produces a hashtable literal.
(define (Loc->javascript-string a-loc)
  (format "{offset:~a,line:~a,column:~a,span:~a,id:~s}" 
          (Loc-offset a-loc) 
          (Loc-line a-loc) 
          (Loc-column a-loc)
          (Loc-span a-loc) 
          ;; DEFENSIVE: make sure this is a string.
          (format "~a" (Loc-id a-loc))))


;; floating-number->javascript-string: number -> string
(define (floating-number->javascript-string a-num)
  (string-append "(plt.types.FloatPoint.makeInstance("
                 (cond
                   [(eqv? a-num +inf.0)
                    "Number.POSITIVE_INFINITY"]
                   [(eqv? a-num -inf.0)
                    "Number.NEGATIVE_INFINITY"]
                   [(eqv? a-num +nan.0)
                    "Number.NaN"]
                   [else
                    (number->string a-num)])
                 "))"))


;; rational-number->javascript-string: number -> string
(define (rational-number->javascript-string a-num)
  (string-append "(plt.types.Rational.makeInstance("
                 (number->string (numerator a-num))
                 ", "
                 (number->string (denominator a-num))
                 "))"))



;; number->java-string: number stx -> string
(define (number->javascript-string a-num original-stx)
  (cond 
    [(rational? a-num)
     (rational-number->javascript-string a-num)]
    
    [(real? a-num)
     (floating-number->javascript-string a-num)]
    
    [(complex? a-num)
     (string-append "(plt.types.Complex.makeInstance("
                    (number->javascript-string (real-part a-num) original-stx)
                    ", "
                    (number->javascript-string (imag-part a-num) original-stx)
                    "))")]))



;; char->javascript-string: char -> string
(define (char->javascript-string a-char)
  (string-append "(plt.types.Char.makeInstance(String.fromCharCode("
                 (number->string (char->integer a-char))
                 ")))"))


;; string->javascript-string: string -> string
(define (string->javascript-string a-str)
  ;; FIXME: escape all character codes!
  (local [(define (escape-char-code a-char)
            (cond
              [(char=? a-char #\")
               (string #\\ #\")]
              [(char=? a-char #\\)
               (string #\\ #\\)]
              [(char=? a-char #\newline)
               (string #\\ #\n)]
              [else
               (string a-char)]))]
    (string-append "(plt.types.String.makeInstance(\""
                   (string-join (map escape-char-code (string->list a-str))
                                "")
                   "\"))")))


;; weird-number?: number -> boolean
;; Returns true if the number is one of the very strange ones.
(define (weird-number? x)
  (or (eqv? x +inf.0)
      (eqv? x -inf.0)
      (eqv? x +nan.0)))


;; expression-sharable?: expression program-info -> boolean
;; Returns true if the expression syntax denotes a value that can be shared.
(define (expression-sharable? an-expr a-pinfo)
  #;false
  (or (and (number? (stx-e an-expr))
           ;; KLUDGE.  Something breaks when I try to share a weird number.
           (not (weird-number? (stx-e an-expr)))
           (or (integer? (stx-e an-expr))
               (real? (stx-e an-expr))))
      (string? (stx-e an-expr))
      (boolean? (stx-e an-expr))
      (char? (stx-e an-expr))
      ;; FIXME: allow quoted things.
      ;; FIXME: add definition that allows toplevel identifiers to be sharable.
      ))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;





(provide/contract [struct compiled-program ([defns 
                                              string?]
                                            [toplevel-exprs 
                                             string?]
                                            [pinfo pinfo?])]
                  
                  [compiled-program-main
                   (compiled-program? . -> . string?)]
                  [compiled-program-main/expose
                   (compiled-program? . -> . string?)]
                  [compiled-program-main/expose-as-module
                   (compiled-program? module-name? . -> . string?)]
                  
                  [program->compiled-program 
                   (program? . -> . compiled-program?)]
                  [program->compiled-program/pinfo
                   (program? pinfo? . -> . compiled-program?)])
