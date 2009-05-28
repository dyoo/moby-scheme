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
                 "function toplevel() {\n"
                 (compiled-program-toplevel-exprs a-compiled-program)
                 "\n}"))



;; program->compiled-program: program [pinfo] -> compiled-program
;; Consumes a program and returns a compiled program.
;; If pinfo is provided, uses that as the base set of known toplevel definitions.

(define (program->compiled-program program)
  (-program->compiled-program program (get-base-pinfo 'js)))


(define (-program->compiled-program program input-pinfo)
  (local [(define a-pinfo (program-analyze/pinfo program input-pinfo))
          (define toplevel-env (pinfo-env a-pinfo))
          (define (loop program defns tops)
            
            
            (cond [(empty? program)
                   (make-compiled-program defns tops a-pinfo)]
                  [else
                   (cond [(defn? (first program))
                          (local [(define defn-string+expr-string
                                    (definition->javascript-strings 
                                      (first program) 
                                      toplevel-env
                                      a-pinfo))]
                            
                            (loop (rest program)
                                  (string-append defns
                                                 "\n"
                                                 (first defn-string+expr-string))
                                  (string-append tops
                                                 "\n"
                                                 (second defn-string+expr-string))))]
                         
                         [(test-case? (first program))
                          (loop (rest program)
                                (string-append defns
                                               "\n"
                                               "// Test case erased\n")
                                tops)]
                         
                         [(library-require? (first program))
                          (loop (rest program)
                                (string-append defns
                                               "\n"
                                               "// Module require erased\n")
                                tops)] 
                         
                         [(expression? (first program))
                          (loop (rest program)
                                defns
                                (string-append tops
                                               "\n"
                                               "org.plt.Kernel.identity("
                                               (expression->javascript-string 
                                                (first program) 
                                                toplevel-env
                                                a-pinfo)
                                               ");"))])]))]
    
    (loop program "" "")))




;; definition->java-string: definition env pinfo -> (list string string)
;; Consumes a definition (define or define-struct) and produces two strings.
;; The first maps a definitions string.
;; The second value is the expression that will be evaluated at the toplevel.
;;
;; Structure definitions map to static inner classes with transparent fields.
(define (definition->javascript-strings defn env a-pinfo)
  (case-analyze-definition 
   defn
   (lambda (fun args body)
     (list (function-definition->java-string fun args body env a-pinfo)
           ""))
   (lambda (id body)
     (variable-definition->javascript-strings id body env a-pinfo))
   (lambda (id fields)
     (list (struct-definition->javascript-string id fields env a-pinfo)
           ""))))
     


;; function-definition->java-string: symbol (listof symbol) expr env pinfo -> string
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
                   args))]
    (format "function ~a(~a) { return ~a; }"
            munged-fun-id
            (string-join (map (lambda (arg-id)
                                (symbol->string arg-id))
                              munged-arg-ids)
                         ", ")
            (expression->javascript-string body env-with-arg-bindings a-pinfo))))



;; variable-definition->javascript-strings: symbol expr env pinfo -> (list string string)
;; Converts the variable definition into a static variable declaration and its
;; initializer at the toplevel.
(define (variable-definition->javascript-strings id body env a-pinfo)
  (local [(define munged-id (identifier->munged-java-identifier id))
          (define new-env (env-extend env (make-binding:constant id 
                                                                 (symbol->string munged-id)
                                                                 empty)))]
    (list (format "var ~a; "
                  munged-id)
          (format "~a = ~a;" 
                  munged-id
                  (expression->javascript-string body new-env a-pinfo)))))




;; struct-definition->javascript-string: symbol (listof symbol) env pinfo -> string
(define (struct-definition->javascript-string id fields env a-pinfo)
  (local [
          ;; field->accessor-name: symbol symbol -> symbol
          ;; Given a structure name and a field, return the accessor.
          (define (field->accessor-name struct-name field-name)
            (string->symbol
             (string-append (symbol->string struct-name)
                            "-"
                            (symbol->string field-name))))
          ]
    
    
    (string-append
     
     ;; default constructor
     (format "function ~a(~a) { ~a }
            ~a.prototype = new org.plt.Kernel.Struct();"
             (identifier->munged-java-identifier id)
             (string-join (map (lambda (i) (format "~a"
                                                   (identifier->munged-java-identifier i)))
                               fields) 
                          ",")
             (string-join (map (lambda (i) (format "this.~a = ~a;" 
                                                   (identifier->munged-java-identifier i)
                                                   (identifier->munged-java-identifier i)))
                               fields) 
                          "\n")
             (identifier->munged-java-identifier id))
     "\n"
     
     ;; equality
     (format "~a.prototype.isEqual = function(other) {
              if (other instanceof ~a) {
                return ~a;
              } else {
                return false;
              }
           } "
             (identifier->munged-java-identifier id)
             (identifier->munged-java-identifier id)
             (expression->javascript-string (foldl (lambda (a-field acc)
                                                     (local [(define acc-id (field->accessor-name id a-field))]
                                                       (list 'and 
                                                             (list 'equal? (list acc-id 'this) (list acc-id 'other))
                                                             acc)))
                                                   'true
                                                   fields)
                                            (local [(define new-env-1 (env-extend env
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
                                                                                   empty)))]
                                              new-env-2)
                                            a-pinfo))
     
     "\n"
     
     ;; make-id
     (format "function ~a(~a) { return new ~a(~a); }"
             (local [(define make-id (string->symbol 
                                      (string-append "make-" (symbol->string id))))]
               (identifier->munged-java-identifier  make-id))
             (string-join (build-list (length fields) (lambda (i) (format "id~a" i)))
                          ",")
             (identifier->munged-java-identifier id)
             (string-join (build-list (length fields) (lambda (i) (format "id~a" i)))
                          ","))
     
     "\n"
     
     ;; accessors
     (string-join 
      (map (lambda (a-field)
             (format "function ~a(obj) { return obj.~a; }"
                     (local [(define acc-id (string->symbol
                                             (string-append (symbol->string id)
                                                            "-"
                                                            (symbol->string a-field))))]
                       (identifier->munged-java-identifier acc-id))
                     (identifier->munged-java-identifier a-field)))
           fields)
      "\n")
     
     "\n"
     
     ;; structure predicate
     (format "function ~a(obj) { 
              return obj instanceof ~a ; 
            }"
             (identifier->munged-java-identifier (string->symbol (format "~a?" id)))
             (identifier->munged-java-identifier id)))))



;; expression->java-string: expr env pinfo -> string
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
       (format "((~a) ? (~a) : (~a))"
               (expression->javascript-string test env a-pinfo)
               (expression->javascript-string consequent env a-pinfo)
               (expression->javascript-string alternative env a-pinfo)))]
    
    ;; (and exprs ...)
    [(list-begins-with? expr 'and)
     (local [(define exprs (rest expr))]
       (string-append "("
                      (string-join (map (lambda (e)
                                          (format "(~a)"
                                                  (expression->javascript-string e env a-pinfo)))
                                        exprs) 
                                   "&&")
                      ")"))]
    ;; (or exprs ...)
    [(list-begins-with? expr 'or)
     (local [(define exprs (rest expr))]
       (string-append "("
                      (string-join  (map (lambda (e)
                                           (format "(~a)"
                                                   (expression->javascript-string e env a-pinfo)))
                                         exprs) 
                                    "||")
                      ")"))]
    ;; (lambda args body)
    [(list-begins-with? expr 'lambda)
     (local [(define args (second expr))
             (define body (third expr))]
       (lambda-expression->javascript-string args body env a-pinfo))]
    
    ;; Numbers
    [(number? expr)
     (number->javascript-string expr)]
    
    ;; Strings
    [(string? expr)
     (string->javascript-string expr)]
    
    ;; Characters
    [(char? expr)
     (char->javascript-string expr)]
    
    ;; Identifiers
    [(symbol? expr)
     (identifier-expression->javascript-string expr env a-pinfo)]
    
    ;; Quoted datums
    [(list-begins-with? expr 'quote)
     (quote-expression->javascript-string (second expr))]
     
    ;; Function call/primitive operation call
    [(pair? expr)
     (local [(define operator (first expr))
             (define operands (rest expr))]
       (application-expression->javascript-string operator operands env a-pinfo))]))



(define (quote-expression->javascript-string expr)
  (cond
    [(empty? expr)
     "org.plt.types.Empty.EMPTY"]
    
    [(pair? expr)
     (format "(org.plt.Kernel.cons(~a, ~a))"
             (quote-expression->javascript-string (first expr))
             (quote-expression->javascript-string (rest expr)))]

    [(symbol? expr)
     (format "(org.plt.types.Symbol.makeInstance(\"~a\"))"
             expr)]

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



;; local-expression->javascript-string: (listof defn) expr env pinfo -> string
(define (local-expression->javascript-string defns body env a-pinfo)
  (local [(define inner-compiled-program 
            (-program->compiled-program defns
                                        (pinfo-update-env a-pinfo env)))
          (define inner-body-string
            (expression->javascript-string 
             body
             (pinfo-env (compiled-program-pinfo inner-compiled-program))
             (compiled-program-pinfo inner-compiled-program)))]

    (format "(function() {
               // Local
               ~a
               ~a
               return ~a;
              })()"
            (compiled-program-defns inner-compiled-program)
            (compiled-program-toplevel-exprs inner-compiled-program)
            inner-body-string)))



;; application-expression->java-string: symbol (listof expr) env pinfo -> string
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
             (define operand-strings 
               (map (lambda (e) 
                      (expression->javascript-string e env a-pinfo))
                    operands))]
       (cond
         
         [(binding:constant? operator-binding)
          (format "((~a).apply(null, [~a]))" 
                  (binding:constant-java-string operator-binding)
                  (string-join operand-strings ", "))]
         
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
                    (format "~a(~a, [~a])"
                            (binding:function-java-string operator-binding)
                            (string-join (take operand-strings (binding:function-min-arity operator-binding)) ",")
                            (string-join (list-tail operand-strings (binding:function-min-arity operator-binding))
                                         ","))]
                   [else
                    (format "~a([~a])"
                            (binding:function-java-string operator-binding)
                            (string-join (list-tail operand-strings (binding:function-min-arity operator-binding))
                                         ","))])]
            [else
             (format "(~a(~a))" 
                     (binding:function-java-string operator-binding)
                     (string-join operand-strings ","))])]))]
    
    ;; General application
    [else
     (local [(define operator-string (expression->javascript-string operator env a-pinfo))
             (define operand-strings 
               (map (lambda (e) 
                      (expression->javascript-string e env a-pinfo))
                    operands))]
       (format "((~a).apply(null, [~a]))" 
               operator-string
               (string-join operand-strings ", ")))]))



;; identifier-expression->javascript-string: symbol env pinfo -> string
;; Translates the use of a toplevel identifier to the appropriate
;; Java code.
(define (identifier-expression->javascript-string an-id an-env a-pinfo)
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
             (format "(function(args) {
                    return ~a.apply(null, args);
                  })"
                     (binding:function-java-string binding))]
            [else
             (format "(function(args) {
                    return ~a(~a);
                 })"
                     (binding:function-java-string binding)
                     (string-join (map (lambda (i)
                                         (format "args[~a]" i))
                                       (range (binding:function-min-arity binding)))
                                  ", "))])]))]))

;; mapi: (X number -> Y) (listof X) -> (listof Y)
(define (mapi f elts)
  (local [(define (loop i elts)
            (cond
              [(empty? elts)
               empty]
              [else
               (cons (f (first elts) i)
                     (loop (add1 i) (rest elts)))]))]
    (loop 0 elts)))



(define (make-args-symbol context)
  (gensym 'args))


;; lambda-expression->javascript-string (listof symbol) expression env pinfo -> string
(define (lambda-expression->javascript-string args body env a-pinfo)
  (local [(define munged-arg-ids
            (map identifier->munged-java-identifier args))
          
          (define new-env
            (foldl (lambda (arg-id env) 
                     (env-extend env (make-binding:constant arg-id 
                                                            (symbol->string
                                                             (identifier->munged-java-identifier arg-id))
                                                            empty)))
                   env
                   args))
          
          (define args-sym
            (make-args-symbol 'lambda-expression->javascript-string))]
    (format "(function(~a) { ~a
                             return ~a; })"
            args-sym
            (string-join (mapi (lambda (arg-id i)
                                 (format "var ~a = ~a[~a];" 
                                         (symbol->string arg-id)
                                         args-sym
                                         i))
                               munged-arg-ids)
                         "\n")
            (expression->javascript-string body new-env a-pinfo))))



;; number->java-string: number -> string
(define (number->javascript-string a-num)
  (cond [(integer? a-num)
         ;; Fixme: we need to handle exact/vs/inexact issue.
         ;; We probably need the numeric tower.
         (format "(org.plt.types.Rational.makeInstance(~a, 1))" (inexact->exact a-num))]
        [(and (inexact? a-num)
              (real? a-num))
         (format "(org.plt.types.FloatPoint.makeInstance(\"~a\"))" a-num)]
        [(rational? a-num)
         (format "(org.plt.types.Rational.makeInstance(~a, ~a))" 
                 (numerator a-num) 
                 (denominator a-num))]
        [(complex? a-num)
         (format "(org.plt.types.Complex.makeInstance(~a, ~a))"
                 (number->javascript-string (real-part a-num))
                 (number->javascript-string (imag-part a-num)))]
        [else
         (error 'number->java-string "Don't know how to handle ~s yet" a-num)]))


(define (string->javascript-string a-str)
  (format "(org.plt.types.String.makeInstance(~s))" a-str))


(define (char->javascript-string a-char)
  (format "(org.plt.types.Character.makeInstance(String.fromCharCode(~s)))"
          (char->integer a-char)))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;





(provide/contract [struct compiled-program ([defns 
                                              string?]
                                            [toplevel-exprs 
                                             string?]
                                            [pinfo pinfo?])]
                  [compiled-program-main
                   (compiled-program? . -> . string?)]
                  
                  [program->compiled-program 
                   (program? . -> . compiled-program?)])