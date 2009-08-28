#lang s-exp "lang.ss"

;; This program translates beginner-level languages into Javascript.
;; We pattern match against the language given by:
;;
;; http://docs.plt-scheme.org/htdp-langs/beginner.html


(require "stx.ss")
(require "env.ss")
(require "pinfo.ss")
(require "analyzer.ss")
(require "helpers.ss")


;; A compiled program is a:
(define-struct compiled-program
  (defns           ;; string
    toplevel-exprs ;; string
    pinfo          ;; pinfo
    ))


;; compiled-program-main: compiled-program ->string
;; Produces the main output source, given the compiled program.
(define (compiled-program-main a-compiled-program)
  (string-append (compiled-program-defns a-compiled-program)
                 "\n"
                 "(function() { \n"
                 "("(compiled-program-toplevel-exprs a-compiled-program) ")"
                 "(plt.Kernel.identity)"
                 "\n})();"))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



;; program->compiled-program: program -> compiled-program
;; Consumes a program and returns a compiled program.
;; If pinfo is provided, uses that as the base set of known toplevel definitions.

(define (program->compiled-program program)
  (program->compiled-program/pinfo program 
                                   (get-base-pinfo 'base)))


;; program->compiled-program/pinfo: program pinfo -> compiled-program
(define (program->compiled-program/pinfo program input-pinfo)
  (local [(define pinfo-1+gensym (pinfo-gensym input-pinfo 'toplevel-expression-show))
          
          
          (define toplevel-expression-show (second pinfo-1+gensym))
          (define a-pinfo (program-analyze/pinfo program (first pinfo-1+gensym)))
          (define toplevel-env (pinfo-env a-pinfo))
          
          (define (loop program defns tops a-pinfo)
            
            (cond [(empty? program)
                   (make-compiled-program defns 
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
                         
                         [(test-case? (first program))
                          (syntax-error 'program->compiled-program/pinfo
                                        "Test case support (check-*) is unimplemented at the moment."
                                        (first program))
                          ;(loop (rest program)
                          ;      (string-append defns
                          ;                     "\n"
                          ;                     "// Test case erased\n")
                          ;      tops
                          ;      a-pinfo)
                          ]
                         
                         [(library-require? (first program))
                          (loop (rest program)
                                (string-append defns
                                               "\n"
                                               "// Module require erased\n")
                                tops
                                a-pinfo)] 
                         
                         [(expression? (first program))
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
    
    (loop program "" "" a-pinfo)))




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
    (list 
     (string-append "function " (symbol->string munged-fun-id) "("
                    (string-join (map (lambda (arg-id)
                                        (symbol->string arg-id))
                                      munged-arg-ids)
                                 ", ")
                    ") { return " body-string "; }"
                    )
     ""
     updated-pinfo)))


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
          
          
          (define updated-pinfo a-pinfo)
          
          ;; predicate-name: string
          (define predicate-name 
            (symbol->string (identifier->munged-java-identifier 
                             (string->symbol (string-append (symbol->string (stx-e id))
                                                            "?")))))
          ;; make-accessor-name: symbol -> string
          (define (make-accessor-name a-field)
            (symbol->string
             (identifier->munged-java-identifier
              (string->symbol
               (string-append (symbol->string (stx-e id))
                              "-"
                              (symbol->string a-field))))))]
    
    (list  (string-append
            
            ;; default constructor
            (string-append "function "
                           (symbol->string (identifier->munged-java-identifier (stx-e id)))
                           "("
                           (string-join (map (lambda (i) (symbol->string
                                                          (identifier->munged-java-identifier 
                                                           (stx-e i))))
                                             fields)
                                        ",")
                           ") { "
                           (format "plt.Kernel.Struct.call(this, ~s, [~a]);"
                                   (string-append "make-"
                                                  (symbol->string 
                                                   (identifier->munged-java-identifier (stx-e id))))
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
                           
                           " }
                    "
                           (symbol->string (identifier->munged-java-identifier (stx-e id)))
                           ".prototype = new plt.Kernel.Struct();\n"
                           
                           )
            
            "\n"
            
            ;; make-id
            (string-append "function "
                           (local [(define make-id (string->symbol 
                                                    (string-append "make-" (symbol->string (stx-e id)))))]
                             (symbol->string (identifier->munged-java-identifier make-id)))
                           "(" (string-join (build-list (length fields) (lambda (i) 
                                                                          (string-append "id" (number->string i))))
                                            ",")
                           ") { return new "
                           (symbol->string (identifier->munged-java-identifier (stx-e id)))
                           "("
                           (string-join (build-list (length fields) (lambda (i) 
                                                                      (string-append "id" (number->string i))))
                                        ",")
                           "); }")
            
            "\n"
            
            ;; accessors
            (string-join 
             (map (lambda (a-field)
                    (string-append "function " (make-accessor-name (stx-e a-field)) "(obj) {\n"
                                   "     if (" predicate-name" (obj)) {\n"
                                   "        return obj." (symbol->string (identifier->munged-java-identifier (stx-e a-field))) ";\n"
                                   "     } else {\n"
                                   "        throw new plt.Kernel.MobyRuntimeError("
                                   "            plt.Kernel.format('" (make-accessor-name (stx-e a-field)) ": not a " (symbol->string (stx-e id)) ": ~s', [obj]));\n"
                                   "     }\n"
                                   "}\n"))
                  fields)
             "\n")
            
            "\n"
            
            ;; structure predicate
            (string-append "function " predicate-name "(obj) { 
              return obj != null && obj != undefined && obj instanceof "
                           (symbol->string (identifier->munged-java-identifier (stx-e id)))
                           "; }"))
           
           "" ;; no introduced toplevel expressions
           updated-pinfo)))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



;; expression->java-string: expr-stx env pinfo -> (list string pinfo)
;; Translates an expression into a Java expression string whose evaluation
;; should produce an Object.
(define (expression->javascript-string expr env a-pinfo)
  (cond
    ;; (local ([define ...] ...) body)
    [(stx-begins-with? expr 'local)
     (local [(define defns (stx-e (second (stx-e expr))))
             (define body (third (stx-e expr)))]
       (local-expression->javascript-string defns body env a-pinfo))]

    ;; (begin ...)
    [(stx-begins-with? expr 'begin)
     (local [(define exprs (rest expr))]
	    (begin-sequence->javascript-string exprs env a-pinfo))]

    
    ;; (cond ...)
    [(stx-begins-with? expr 'cond)
     (expression->javascript-string (desugar-cond expr) env a-pinfo)]    
    
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
     (expression->javascript-string (if (stx-e expr) 
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


;; begin-sequence->javascript-string: (listof expr) env pinfo -> (list string pinfo)
(define (begin-sequence->javascript-string exprs env a-pinfo)
  (local [;; exclude-last-element: (listof any) -> (listof (listof any) any)
          ;; (exclude-last-element (list x y z)) --> (list (list x y) z)
          (define (exclude-last-element ls)
            (cond
              [(empty? ls) (error 'exclude-last-element "There should be at least one element")]
              [else
               (local [(define rev-ls (reverse ls))]
                 (list (reverse (rest rev-ls)) (first rev-ls)))]))

          (define strings+pinfo
	    (expressions->javascript-strings exprs env a-pinfo))
	  (define exprs+last-expr
	    (exclude-last-element (first strings+pinfo)))]
	 (list (string-append "(function(){"
			      (string-join (first exprs+last-expr) ";\n")
			      ";\n"
			      "return "
			      (second exprs+last-expr) ";"
			      "})()")
	       (second strings+pinfo))))



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
    
    [else
     (syntax-error 'quote-expression->javascript-string 
                   "Unknown quoted expression encountered"
                   expr)]))



;; boolean-chain->javascript-string: string (listof expr) env pinfo -> (list string pinfo)
(define (boolean-chain->javascript-string joiner exprs env a-pinfo)
  (local [(define strings+pinfo
            (expressions->javascript-strings exprs env a-pinfo))]
    (list (string-append "(" (string-join (first strings+pinfo) joiner) ")")
          (second strings+pinfo))))



;; local-expression->javascript-string: (listof defn) expr env pinfo -> (list string pinfo)
(define (local-expression->javascript-string defns body env a-pinfo)
  (local [(define inner-compiled-program 
            (program->compiled-program/pinfo defns
                                             (pinfo-update-env a-pinfo env)))
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
          (pinfo-update-env updated-pinfo (pinfo-env a-pinfo)))))



;; application-expression->java-string: symbol-stx (listof expr) env pinfo -> (list string pinfo)
;; Converts the function application to a string.
(define (application-expression->javascript-string original-stx operator operands env a-pinfo)
  (cond 
    ;; Special case: when the operator is named
    [(and (symbol? (stx-e operator))
          (not (env-contains? env (stx-e operator))))
     (syntax-error 'application-expression->java-string
                   (format "name ~s is not defined, not a parameter, and not a primitive name" (stx-e operator))
                   operator)]
    
    [(symbol? (stx-e operator))
     (local [(define operator-binding (env-lookup env (stx-e operator)))
             (define operand-strings+pinfo
               (expressions->javascript-strings operands env a-pinfo))
             
             (define operand-strings (first operand-strings+pinfo))
             (define updated-pinfo (second operand-strings+pinfo))]
       (cond
         
         [(binding:constant? operator-binding)
          (list (string-append "(" (format "plt.Kernel.setLastLoc(~s)" (Loc->string (stx-loc original-stx)))
                               "  && "
                               (binding:constant-java-string operator-binding)".apply(null, [["
                               (string-join operand-strings ", ")
                               "]]))")
                updated-pinfo)]
         
         [(binding:function? operator-binding)
          (cond
            [(< (length operands)
                (binding:function-min-arity operator-binding))
             (syntax-error 'application-expression->javascript-string
                           (format "Too few arguments passed to ~s.  Expects at least ~a arguments, given ~a."
                                   (stx-e operator)
                                   (binding:function-min-arity operator-binding)
                                   (length operands))
                           original-stx)]
            [(binding:function-var-arity? operator-binding)
             (cond [(> (binding:function-min-arity operator-binding) 0)
                    (list 
                     (string-append "(" (format "plt.Kernel.setLastLoc(~s)" (Loc->string (stx-loc original-stx)))
                                    " && "
                                    (binding:function-java-string operator-binding)
                                    "("
                                    (string-join (take operand-strings (binding:function-min-arity operator-binding)) ",")
                                    ", ["
                                    (string-join (list-tail operand-strings (binding:function-min-arity operator-binding))
                                                 ",")
                                    "]))")
                     updated-pinfo)]
                   [else
                    (list
                     (string-append "(" (format "plt.Kernel.setLastLoc(~s)" (Loc->string (stx-loc original-stx)))
                                    " && "
                                    (binding:function-java-string operator-binding) 
                                    "(["
                                    (string-join operand-strings ",")
                                    "]))")
                     updated-pinfo)])]
            [else
             (cond
               [(> (length operands)
                   (binding:function-min-arity operator-binding))
                (syntax-error 'application-expression->javascript-string 
                              (format "Too many arguments passed to ~s.  Expects at most ~a arguments, given ~a."
                                      (stx-e operator)
                                      (binding:function-min-arity operator-binding)
                                      (length operands))
                              original-stx)]
               [else
                (list 
                 (string-append "("
                                (format "plt.Kernel.setLastLoc(~s)" (Loc->string (stx-loc original-stx)))
                                "   && "
                                (binding:function-java-string operator-binding)
                                "(" (string-join operand-strings ",") "))")
                 updated-pinfo)])])]))]
    
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
        (string-append "(" (format "plt.Kernel.setLastLoc(~s)" (Loc->string (stx-loc original-stx))) 
                       " && "
                       "(" operator-string ").apply(null, [["
                       (string-join operand-strings ", ")
                       "]]))")
        updated-pinfo))]))



;; identifier-expression->javascript-string: symbol-stx env -> string
;; Translates the use of a toplevel identifier to the appropriate
;; Java code.
(define (identifier-expression->javascript-string an-id an-env)
  (cond
    [(not (env-contains? an-env (stx-e an-id)))
     (syntax-error 'translate-toplevel-id 
                   (format "name ~s is not defined, not a parameter, and not a primitive name." (stx-e an-id))
                   an-id)]
    [else     
     (local [(define binding (env-lookup an-env (stx-e an-id)))]
       (cond
         [(binding:constant? binding)
          (binding:constant-java-string binding)]
         [(binding:function? binding)
          (cond
            [(binding:function-var-arity? binding)
             (string-append "((function() { var result = (function(args) {
                    return "
                            (binding:function-java-string binding)
                            ".apply(null, args.slice(0, " (number->string (binding:function-min-arity binding)) ").concat([args.slice("(number->string (binding:function-min-arity binding))")]));
                  }); result.toWrittenString = function() {return '<function:" (symbol->string (binding-id binding)) ">'; }
                      result.toDisplayedString = function() {return '<function:" (symbol->string (binding-id binding)) ">';}
                      return result; })())")]
            [else
             (string-append "(function() { var result = (function(args) {
                    return "
                            (binding:function-java-string binding)
                            "("
                            (string-join (map (lambda (i)
                                                (string-append "args[" (number->string i)"]"))
                                              (range (binding:function-min-arity binding)))
                                         ", ")
                            ");
                 }); result.toWrittenString = function() {return '<function:"(symbol->string (binding-id binding))">'; }
                     result.toDisplayedString = function() {return '<function:"(symbol->string (binding-id binding))">';}
                     return result; })()")])]))]))



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
    (list
     (string-append "((function() {\n"
                    "   plt.Kernel.setLastLoc(" (format "~s" (Loc->string (stx-loc original-stx))) ");\n"
                    "   var result = (function(" (symbol->string args-sym) ") {\n"
                    (string-join (mapi (lambda (arg-id i)
                                         (string-append "var "
                                                        (symbol->string arg-id)
                                                        " = "
                                                        (symbol->string args-sym)
                                                        "[" (number->string i) "];"))
                                       munged-arg-ids)
                                 "\n")
                    "
                             return "
                    body-string
                    "; });
                      result.toWrittenString = function () {
                          return '<function:lambda>';
                      };
                      result.toDisplayedString = result.toWrittenString;
                      return result;
                   })())")
     updated-pinfo)))



;; number->java-string: number -> string
(define (number->javascript-string a-num original-stx)
  (cond [(integer? a-num)
         (string-append "(plt.types.Rational.makeInstance("
                        (number->string (inexact->exact a-num))
                        ", 1))")]
        [(rational? a-num)
         (string-append "(plt.types.Rational.makeInstance("
                        (number->string (numerator a-num))
                        ", "
                        (number->string (denominator a-num))
                        "))")]
        [(real? a-num)
         (string-append "(plt.types.FloatPoint.makeInstance(" 
                        (number->string a-num)"))")]
        [(complex? a-num)
         (string-append "(plt.types.Complex.makeInstance("
                        (number->string (real-part a-num))
                        ", "
                        (number->string (imag-part a-num))"))")]
        
        [else
         (syntax-error 'number->java-string 
                       (format "Don't know how to handle ~s yet" a-num)
                       original-stx)]))



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



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;





(provide/contract [struct compiled-program ([defns 
                                              string?]
                                            [toplevel-exprs 
                                             string?]
                                            [pinfo pinfo?])]
                  
                  
                  [compiled-program-main
                   (compiled-program? . -> . string?)]
                  
                  [program->compiled-program 
                   (program? . -> . compiled-program?)]
                  [program->compiled-program/pinfo
                   (program? pinfo? . -> . compiled-program?)])