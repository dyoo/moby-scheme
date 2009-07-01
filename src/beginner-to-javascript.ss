#lang s-exp "lang.ss"

;; This program translates beginner-level languages into Javascript.
;; We pattern match against the language given by:
;;
;; http://docs.plt-scheme.org/htdp-langs/beginner.html



(require "env.ss")
(require "pinfo.ss")
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
                          (loop (rest program)
                                (string-append defns
                                               "\n"
                                               "// Test case erased\n")
                                tops
                                a-pinfo)]
                         
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

     


;; function-definition->java-string: symbol (listof symbol) expr env pinfo -> (list string string pinfo)
;; Converts the function definition into a static function declaration whose
;; return value is an object.
(define (function-definition->java-string fun args body env a-pinfo)
  (local [(define munged-fun-id
            (identifier->munged-java-identifier fun))
          (define munged-arg-ids
            (map identifier->munged-java-identifier args))
          (define new-env 
            (env-extend-function env fun false (length args) false
                                 (symbol->string munged-fun-id)))
          (define env-with-arg-bindings
            (foldl (lambda (arg-id env) 
                     (env-extend env (make-binding:constant arg-id 
                                                            (symbol->string
                                                             (identifier->munged-java-identifier arg-id))
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


;; variable-definition->javascript-strings: symbol expr env pinfo -> (list string string pinfo)
;; Converts the variable definition into a static variable declaration and its
;; initializer at the toplevel.
(define (variable-definition->javascript-strings id body env a-pinfo)
  (local [(define munged-id (identifier->munged-java-identifier id))
          (define new-env (env-extend env 
                                      (make-binding:constant 
                                       id 
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






;; struct-definition->javascript-string: symbol (listof symbol) env pinfo -> (list string string pinfo)
(define (struct-definition->javascript-string id fields env a-pinfo)
  (local [
          ;; field->accessor-name: symbol symbol -> symbol
          ;; Given a structure name and a field, return the accessor.
          (define (field->accessor-name struct-name field-name)
            (string->symbol
             (string-append (symbol->string struct-name)
                            "-"
                            (symbol->string field-name))))
                    
          (define new-env-1 (env-extend env
                                        (make-binding:constant
                                         'this
                                         (symbol->string
                                          (identifier->munged-java-identifier 'this))
                                         empty)))
          (define new-env-2 (env-extend new-env-1
                                        (make-binding:constant
                                         'other
                                         (symbol->string
                                          (identifier->munged-java-identifier 'other))
                                         empty)))
          
          (define equality-expression
            (foldl (lambda (a-field acc)
                     (local [(define acc-id (field->accessor-name id a-field))]
                       (list 'and 
                             (list 'equal? (list acc-id 'this) (list acc-id 'other))
                             acc)))
                   'true
                   fields))

          (define equality-expression-string+pinfo
            (expression->javascript-string equality-expression
                                                        new-env-2
                                                        a-pinfo))
          
          (define equality-expression-string (first equality-expression-string+pinfo))
          (define updated-pinfo (second equality-expression-string+pinfo))]
    
    (list  (string-append
            
            ;; default constructor
            (string-append "function "
                           (symbol->string (identifier->munged-java-identifier id))
                           "("
                           (string-join (map (lambda (i) (symbol->string
                                                          (identifier->munged-java-identifier i)))
                                             fields)
                                        ",")
                           ") { "
                           (string-join (map (lambda (i) (string-append "this."
                                                                        (symbol->string 
                                                                         (identifier->munged-java-identifier i))
                                                                        " = "
                                                                        (symbol->string 
                                                                         (identifier->munged-java-identifier i))
                                                                        ";"))
                                             fields) 
                                        "\n")
                           
                           " }
                    "
                           (symbol->string (identifier->munged-java-identifier id))
                           ".prototype = new plt.Kernel.Struct();"
                           )
            "\n"
            
            ;; equality
            (string-append (symbol->string (identifier->munged-java-identifier id))
                           ".prototype.isEqual = function(other) {
              if (other instanceof " (symbol->string (identifier->munged-java-identifier id)) ") {
                return " equality-expression-string ";
              } else {
                return false;
              }
           } ")
            
            "\n"
            
            ;; make-id
            (string-append "function "
                           (local [(define make-id (string->symbol 
                                                    (string-append "make-" (symbol->string id))))]
                             (symbol->string (identifier->munged-java-identifier make-id)))
                           "(" (string-join (build-list (length fields) (lambda (i) 
                                                                          (string-append "id" (number->string i))))
                                            ",")
                           ") { return new "
                           (symbol->string (identifier->munged-java-identifier id))
                           "("
                           (string-join (build-list (length fields) (lambda (i) 
                                                                      (string-append "id" (number->string i))))
                                        ",")
                           "); }")
            
            "\n"
            
            ;; accessors
            (string-join 
             (map (lambda (a-field)
                    (string-append "function "
                                   (local [(define acc-id (string->symbol
                                                           (string-append (symbol->string id)
                                                                          "-"
                                                                          (symbol->string a-field))))]
                                     (symbol->string (identifier->munged-java-identifier acc-id)))
                                   "(obj) { return obj."
                                   (symbol->string (identifier->munged-java-identifier a-field))
                                   "; }"))
                  fields)
             "\n")
            
            "\n"
            
            ;; structure predicate
            (string-append "function "
                           (symbol->string (identifier->munged-java-identifier (string->symbol (string-append (symbol->string id)
                                                                                                              "?"))))
                           "(obj) { 
              return obj instanceof "
                           (symbol->string (identifier->munged-java-identifier id))
                           "; }"))
           
           "" ;; no introduced toplevel expressions
           updated-pinfo)))
  


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



;; expression->java-string: expr env pinfo -> (list string pinfo)
;; Translates an expression into a Java expression string whose evaluation
;; should produce an Object.
(define (expression->javascript-string expr env a-pinfo)
  (cond
    ;; (local ([define ...] ...) body)
    [(list-begins-with? expr 'local)
     (local [(define defns (second expr))
             (define body (third expr))]
       (local-expression->javascript-string defns body env a-pinfo))]
    
    ;; (cond ...)
    [(list-begins-with? expr 'cond)
     (expression->javascript-string (desugar-cond expr) env a-pinfo)]    
    
    ;; (if test consequent alternative)
    [(list-begins-with? expr 'if)
     (local [(define test (second expr))
             (define consequent (third expr))
             (define alternative (fourth expr))]
       (if-expression->javascript-string test consequent alternative env a-pinfo))]
    
    ;; (and exprs ...)
    [(list-begins-with? expr 'and)
     (local [(define exprs (rest expr))]
       (boolean-chain->javascript-string "&&" exprs env a-pinfo))]

    ;; (or exprs ...)
    [(list-begins-with? expr 'or)
     (local [(define exprs (rest expr))]
       (boolean-chain->javascript-string "||" exprs env a-pinfo))]

    ;; (lambda args body)
    [(list-begins-with? expr 'lambda)
     (local [(define args (second expr))
             (define body (third expr))]
       (lambda-expression->javascript-string args body env a-pinfo))]
    
    ;; Numbers
    [(number? expr)
     (list
      (number->javascript-string expr)
      a-pinfo)]
    
    ;; Strings
    [(string? expr)
     (list (string->javascript-string expr)
           a-pinfo)]

    ;; Literal booleans
    [(boolean? expr)
     (expression->javascript-string (if expr 'true 'false)
                                    env
                                    a-pinfo)]
    ;; Characters
    [(char? expr)
     (list (char->javascript-string expr)
           a-pinfo)]
    
    ;; Identifiers
    [(symbol? expr)
     (list
      (identifier-expression->javascript-string expr env)
      a-pinfo)]
    
    ;; Quoted datums
    [(list-begins-with? expr 'quote)
     (list (quote-expression->javascript-string (second expr))
           a-pinfo)]
     
    ;; Function call/primitive operation call
    [(pair? expr)
     (local [(define operator (first expr))
             (define operands (rest expr))]
       (application-expression->javascript-string operator operands env a-pinfo))]))



;; expressions->javascript-strings: (listof expr) env pinfo -> (list (listof string) pinfo)
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
    [(empty? expr)
     "plt.types.Empty.EMPTY"]
    
    [(pair? expr)
     (string-append "(plt.Kernel.list(["
                          (string-join 
                           (map quote-expression->javascript-string expr)
                           ",")
                          "]))")]
    
    [(symbol? expr)
     (string-append "(plt.types.Symbol.makeInstance(\""
                    (symbol->string expr)
                    "\"))")]
    
    ;; Numbers
    [(number? expr)
     (number->javascript-string expr)]
    
    ;; Strings
    [(string? expr)
     (string->javascript-string expr)]
    
    ;; Characters
    [(char? expr)
     (char->javascript-string expr)]
    
    [else
     (error 'quote-expression->javascript-string 
            (format "I don't know how to deal with ~s" expr))]))



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



;; application-expression->java-string: symbol (listof expr) env pinfo -> (list string pinfo)
;; Converts the function application to a string.
(define (application-expression->javascript-string operator operands env a-pinfo)
  (cond 
    ;; Special case: when the operator is named
    [(and (symbol? operator)
          (not (env-contains? env operator)))
     (error 'application-expression->java-string
            (format "Moby doesn't know about ~s" operator))]
    
    [(symbol? operator)
     (local [(define operator-binding (env-lookup env operator))
             (define operand-strings+pinfo
               (expressions->javascript-strings operands env a-pinfo))
             
             (define operand-strings (first operand-strings+pinfo))
             (define updated-pinfo (second operand-strings+pinfo))]
       (cond
         
         [(binding:constant? operator-binding)
          (list (string-append "(("
                               (binding:constant-java-string operator-binding)
                               ").apply(null, [["
                               (string-join operand-strings ", ")
                               "]]))")
                updated-pinfo)]
         
         [(binding:function? operator-binding)
          (cond
            [(< (length operands)
                (binding:function-min-arity operator-binding))
             (error 'application-expression->java-string
                    (format "Minimal arity of ~s not met.  Operands were ~s"
                            operator
                            operands))]
            [(binding:function-var-arity? operator-binding)
             (cond [(> (binding:function-min-arity operator-binding) 0)
                    (list 
                     (string-append (binding:function-java-string operator-binding)
                                    "("
                                    (string-join (take operand-strings (binding:function-min-arity operator-binding)) ",")
                                    ", ["
                                    (string-join (list-tail operand-strings (binding:function-min-arity operator-binding))
                                                 ",")
                                    "])")
                     updated-pinfo)]
                   [else
                    (list
                     (string-append (binding:function-java-string operator-binding) 
                                    "(["
                                    (string-join operand-strings ",")
                                    "])")
                     updated-pinfo)])]
            [else
             (list 
              (string-append "("
                             (binding:function-java-string operator-binding)
                             "("
                             (string-join operand-strings ",")
                             "))")
              updated-pinfo)])]))]
    
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
        (string-append "(("
                       operator-string
                       ").apply(null, [["
                       (string-join operand-strings ", ")
                       "]]))")
        updated-pinfo))]))



;; identifier-expression->javascript-string: symbol env -> string
;; Translates the use of a toplevel identifier to the appropriate
;; Java code.
(define (identifier-expression->javascript-string an-id an-env)
  (cond
    [(not (env-contains? an-env an-id))
     (error 'translate-toplevel-id (format "Moby doesn't know about ~s." an-id))]
    [else     
     (local [(define binding (env-lookup an-env an-id))]
       (cond
         [(binding:constant? binding)
          (binding:constant-java-string binding)]
         [(binding:function? binding)
          (cond
            [(binding:function-var-arity? binding)
             (string-append "(function(args) {
                    return "
                            (binding:function-java-string binding)
                            ".apply(null, args);
                  })")]
            [else
             (string-append "(function(args) {
                    return "
                            (binding:function-java-string binding)
                            "("
                            (string-join (map (lambda (i)
                                                (string-append "args[" (number->string i)"]"))
                                              (range (binding:function-min-arity binding)))
                                         ", ")
                            ");
                 })")])]))]))



;; lambda-expression->javascript-string (listof symbol) expression env pinfo -> string
(define (lambda-expression->javascript-string args body env a-pinfo)
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
            (map identifier->munged-java-identifier args))
          
          (define new-env
            (foldl (lambda (arg-id env) 
                     (env-extend env 
                                 (make-binding:constant 
                                  arg-id 
                                  (symbol->string
                                   (identifier->munged-java-identifier arg-id))
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
     (string-append "(function("
                    (symbol->string args-sym)
                    ") { "
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
                   "; })")
     updated-pinfo)))



;; number->java-string: number -> string
(define (number->javascript-string a-num)
  (cond [(integer? a-num)
         ;; Fixme: we need to handle exact/vs/inexact issue.
         ;; We probably need the numeric tower.
         (string-append "(plt.types.Rational.makeInstance("
                        (number->string (inexact->exact a-num))
                        ", 1))")]
        [(and (inexact? a-num)
              (real? a-num))
         (string-append "(plt.types.FloatPoint.makeInstance(\"" 
                        (number->string a-num)"\"))")]
        [(rational? a-num)
         (string-append "(plt.types.Rational.makeInstance("
                        (number->string (numerator a-num))
                        ", "
                        (number->string (denominator a-num))
                        "))")]
        [(complex? a-num)
         (string-append "(plt.types.Complex.makeInstance("
                        (number->string (real-part a-num))
                        ", "
                        (number->string (imag-part a-num))"))")]
        
        [else
         (error 'number->java-string "Don't know how to handle ~s yet" a-num)]))



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