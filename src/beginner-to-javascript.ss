#lang scheme/base

;; This program translates beginner-level languages into Javascript.
;; We pattern match against the language given by:
;;
;; http://docs.plt-scheme.org/htdp-langs/beginner.html


(require scheme/match
         scheme/list
         scheme/string
         scheme/contract
         "env.ss"
         "toplevel.ss"
         "pinfo.ss"
         "helpers.ss")


;; A compiled program is a:
(define-struct compiled-program
  (defns           ;; (listof string)
    toplevel-exprs ;; (listof string)
    pinfo          ;; pinfo
    ))



;; program->compiled-program: program -> compiled-program
;; Consumes a program and returns a compiled program.
(define (program->compiled-program program)
  (let* ([a-pinfo (program-analyze program)]
         [toplevel-env (pinfo-env a-pinfo)])
    
    (let loop ([program program]
               [defns ""]
               [tops ""])
      (cond [(empty? program)
             (make-compiled-program defns tops a-pinfo)]
            [else
             (cond [(defn? (first program))
                    (let-values ([(defn-string expr-string)
                                  (definition->javascript-strings 
                                           (first program) 
                                           toplevel-env)])
                                         
                    (loop (rest program)
                          (string-append defns
                                         "\n"
                                         defn-string)
                          (string-append tops
                                         "\n"
                                         expr-string)))]
                   
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
                                         "identity("
                                         (expression->java-string 
                                          (first program) 
                                          toplevel-env)
                                         ");"))])]))))



;; definition->java-string: definition env -> (values string string)
;; Consumes a definition (define or define-struct) and produces two strings.
;; The first maps a definitions string.
;; The second value is the expression that will be evaluated at the toplevel.
;;
;; Structure definitions map to static inner classes with transparent fields.
(define (definition->javascript-strings defn env)
  (match defn
    [(list 'define (list fun args ...) body)
     (values (function-definition->java-string fun args body env)
             "")]
    [(list 'define (? symbol? fun) (list 'lambda (list args ...) body))
     (values (function-definition->java-string fun args body env)
             "")]
    [(list 'define (? symbol? id) body)
     (variable-definition->java-strings id body env)]

    [(list 'define-struct id (list fields ...))
     (values (struct-definition->java-string id fields env)
             "")]))


;; function-definition->java-string: symbol (listof symbol) expr env -> string
;; Converts the function definition into a static function declaration whose
;; return value is an object.
(define (function-definition->java-string fun args body env)
  (let* ([munged-fun-id
          (identifier->munged-java-identifier fun)]
         [munged-arg-ids
          (map identifier->munged-java-identifier args)]
         [new-env 
          (env-extend env (make-binding:function fun #f (length args) #f
                                                 (symbol->string munged-fun-id)
                                                 empty))]
         [new-env
          (foldl (lambda (arg-id env) 
                   (env-extend env (make-binding:constant arg-id 
                                                          (symbol->string
                                                           (identifier->munged-java-identifier arg-id))
                                                          empty)))
                 new-env
                 args)])
    (format "function ~a(~a) { return ~a; }"
            munged-fun-id
            (string-join (map (lambda (arg-id)
                                (symbol->string arg-id))
                              munged-arg-ids)
                         ", ")
            (expression->java-string body new-env))))



;; variable-definition->java-string: symbol expr env -> (values string string)
;; Converts the variable definition into a static variable declaration and its
;; initializer at the toplevel.
(define (variable-definition->java-strings id body env)
  (let* ([munged-id (identifier->munged-java-identifier id)]
         [new-env (env-extend env (make-binding:constant id 
                                                         (symbol->string munged-id)
                                                         empty))])
    (values (format "var ~a; "
                    munged-id)
            (format "~a = ~a;" 
                    munged-id
                    (expression->java-string body new-env)))))




;; struct-definition->java-string: symbol (listof symbol) env -> string
(define (struct-definition->java-string id fields env)
  
  ;; field->accessor-name: symbol symbol -> symbol
  ;; Given a structure name and a field, return the accessor.
  (define (field->accessor-name struct-name field-name)
    (string->symbol
     (string-append (symbol->string struct-name)
                    "-"
                    (symbol->string field-name))))
  
  (format "static public class ~a implements org.plt.types.Struct { ~a \n ~a\n ~a\n}\n~a \n ~a\n ~a" 
          (identifier->munged-java-identifier id)
          (string-join (map (lambda (a-field)
                              (format "public Object ~a;"
                                      (identifier->munged-java-identifier a-field)))
                            fields)
                       "\n")
          
          ;; default constructor
          (format "public ~a(~a) { ~a }"
                  (identifier->munged-java-identifier id)
                  (string-join (map (lambda (i) (format "Object ~a"
                                                        (identifier->munged-java-identifier i)))
                                    fields) 
                               ",")
                  (string-join (map (lambda (i) (format "this.~a = ~a;" 
                                                        (identifier->munged-java-identifier i)
                                                        (identifier->munged-java-identifier i)))
                                    fields) 
                               "\n"))
          

          ;; equality
          (format "public boolean equals(Object other) {
                     if (other instanceof ~a) {
                       return ~a.isTrue();
                     } else {
                       return false;
                     }
                   } "
                  (identifier->munged-java-identifier id)
                  (expression->java-string (foldl (lambda (a-field acc)
                                                    (let ([acc-id (field->accessor-name id a-field)])
                                                      `(and (equal? (,acc-id this)
                                                                    (,acc-id other)) 
                                                            ,acc)))
                                                  'true
                                                  fields)
                                           (let* ([new-env (env-extend env
                                                                       (make-binding:constant
                                                                        'this
                                                                        (symbol->string
                                                                         (identifier->munged-java-identifier 'this))
                                                                        empty))]
                                                  [new-env (env-extend new-env
                                                                       (make-binding:constant
                                                                        'other
                                                                        (symbol->string
                                                                         (identifier->munged-java-identifier 'other))
                                                                        empty))])
                                             new-env)))
          
          ;; make-id
          (format "static public Object ~a(~a) { return new ~a(~a); }"
                  (let ([make-id (string->symbol 
                                  (string-append "make-" (symbol->string id)))])
                    (identifier->munged-java-identifier  make-id))
                  (string-join (build-list (length fields) (lambda (i) (format "Object id~a" i)))
                               ",")
                  (identifier->munged-java-identifier id)
                  (string-join (build-list (length fields) (lambda (i) (format "id~a" i)))
                               ","))
          
          ;; accessors
          (string-join 
           (map (lambda (a-field)
                  (format "static public Object ~a(Object obj) { return ((~a)obj).~a; }"
                          (let ([acc-id (string->symbol
                                         (string-append (symbol->string id)
                                                        "-"
                                                        (symbol->string a-field)))])
                            (identifier->munged-java-identifier acc-id))
                          (identifier->munged-java-identifier id)
                          (identifier->munged-java-identifier a-field)))
                fields)
           "\n")
          
          ;; predicate
          (format "static public org.plt.types.Logic ~a(Object obj) { return obj instanceof ~a ? org.plt.types.Logic.TRUE : org.plt.types.Logic.FALSE; }"
                  (identifier->munged-java-identifier (string->symbol (format "~a?" id)))
                  (identifier->munged-java-identifier id))))



;; expression->java-string: expr env -> string
;; Translates an expression into a Java expression string whose evaluation
;; should produce an Object.
(define (expression->java-string expr env)
  (match expr
    [(list 'cond [list questions answers] ... [list 'else answer-last])
     (let loop ([questions questions]
                [answers answers])
       (cond
         [(empty? questions)
          (expression->java-string answer-last env)]
         [else
          (format "(((org.plt.types.Logic)(~a)).isTrue() ? (~a) : (~a))"
                  (expression->java-string (first questions) env)
                  (expression->java-string (first answers) env)
                  (loop (rest questions) (rest answers)))]))]
    
    
    [(list 'cond [list questions answers] ... [list question-last answer-last])
     (let loop ([questions questions]
                [answers answers])
       (cond
         [(empty? questions)
          (format "(((org.plt.types.Logic)(~a)).isTrue() ? (~a) : 
                      org.plt.Kernel.error(org.plt.types.Symbol.makeInstance(\"cond\"), \"Fell out of cond\"))"
                  (expression->java-string question-last env)
                  (expression->java-string answer-last env))]
         [else
          (format "(((org.plt.types.Logic)(~a)).isTrue() ? (~a) : (~a))"
                  (expression->java-string (first questions) env)
                  (expression->java-string (first answers) env)
                  (loop (rest questions) (rest answers)))]))]
    
    [(list 'if test consequent alternative)
     (format "(((org.plt.types.Logic)(~a)).isTrue() ? (~a) : (~a))"
             (expression->java-string test env)
             (expression->java-string consequent env)
             (expression->java-string alternative env))]
    
    [(list 'and expr ...)
     (string-append "(("
                    (string-join (map (lambda (e)
                                        (format "(((org.plt.types.Logic)~a).isTrue())"
                                                (expression->java-string e env)))
                                      expr) 
                                 "&&")
                    ") ? org.plt.types.Logic.TRUE : org.plt.types.Logic.FALSE)")]
    
    [(list 'or expr ...)
     (string-append "(("
                    (string-join  (map (lambda (e)
                                         (format "(((org.plt.types.Logic)~a).isTrue())"
                                                 (expression->java-string e env)))
                                       expr) 
                                  "||")
                    ") ? org.plt.types.Logic.TRUE : org.plt.types.Logic.FALSE)")]
    
    ;; Numbers
    [(? number?)
     (number->java-string expr)]
    
    ;; Strings
    [(? string?)
     (format "(new String(~s))" expr)]
    
    ;; Characters
    [(? char?)
     (string-append "(new Character(\""
                    (if (char=? expr #\") "\\" (string expr))
                    "\"))")]
    
    ;; Identifiers
    [(? symbol?)
     (identifier-expression->java-string expr env)]
    
    ;; Quoted symbols
    [(list 'quote datum)
     (format "(org.plt.types.Symbol.makeInstance(\"~a\"))"
             datum)]
    
    ;; Function call/primitive operation call
    [(list (? symbol? id) exprs ...)
     (application-expression->java-string id exprs env)]))



;; application-expression->java-string: symbol (listof expr) env -> string
;; Converts the function application to a string.
(define (application-expression->java-string id exprs env)
  (let ([operator-binding (env-lookup env id)]
        [operand-strings 
         (map (lambda (e) 
                             (expression->java-string e env))
                           exprs)])
    (match operator-binding
      ['#f
       (error 'application-expression->java-string
              "Moby doesn't know about ~s" id)]
      
      [(struct binding:constant (name java-string permissions))
       (format "(((org.plt.types.Callable) ~a).call(new Object[] {~a}))" 
               java-string 
               (string-join operand-strings ", "))]
      
      [(struct binding:function (name module-path min-arity var-arity? 
                                      java-string permissions))
       (unless (>= (length exprs)
                   min-arity)
         (error 'application-expression->java-string
                "Minimal arity of ~s not met.  Operands were ~s"
                id
                exprs))
       (cond
         [var-arity?
          (cond [(> min-arity 0)
                 (format "~a(~a, new Object[] {~a})"
                         java-string
                         (string-join (take operand-strings min-arity) ",")
                         (string-join (list-tail operand-strings min-arity)
                                      ","))]
                [else
                 (format "~a(new Object[] {~a})"
                         java-string
                         (string-join (list-tail operand-strings min-arity)
                                      ","))])]
         [else
          (format "(~a(~a))" 
                  java-string 
                  (string-join operand-strings ","))])])))



;; identifier-expression->java-string: symbol -> string
;; Translates the use of a toplevel identifier to the appropriate
;; Java code.
(define (identifier-expression->java-string an-id an-env)
  (match (env-lookup an-env an-id)
    ['#f
     (error 'translate-toplevel-id "Moby doesn't know about ~s." an-id)]
    [(struct binding:constant (name java-string permissions))
     java-string]
    [(struct binding:function (name module-path min-arity var-arity? java-string permissions))
     (cond
       [var-arity?
        (format "(new org.plt.types.Callable() {
                      public Object call(Object[] args) {
                          return ~a(args);
                      }
                  })"
                java-string)]
       [else
        (format "(new org.plt.types.Callable() {
                   public Object call(Object[] args) {
                       return ~a(~a);
                   }
                 })"
                java-string
                (string-join (for/list ([i (in-range min-arity)])
                               (format "args[~a]" i))
                             ", "))])
     #;(error 'translate-toplevel-id
              "Moby doesn't yet allow higher-order use of ~s." an-id)]))




;; number->java-string: number -> string
(define (number->java-string a-num)
  (cond [(integer? a-num)
         ;; Fixme: we need to handle exact/vs/inexact issue.
         ;; We probably need the numeric tower.
         (format "(new org.plt.types.Rational(~a, 1))" (inexact->exact a-num))]
        [(and (inexact? a-num)
              (real? a-num))
         (format "(org.plt.types.FloatPoint.fromString(\"~s\"))" a-num)]
        [(rational? a-num)
         (format "(new org.plt.types.Rational(~s, ~s))" 
                 (numerator a-num) 
                 (denominator a-num))]
        [(complex? a-num)
         (format "(new org.plt.types.Complex(~a, ~a))"
                 (number->java-string (real-part a-num))
                 (number->java-string (imag-part a-num)))]
        [else
         (error 'number->java-string "Don't know how to handle ~s yet" a-num)]))




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;





(provide/contract [struct compiled-program ([defns 
                                              string?]
                                            [toplevel-exprs 
                                             string?]
                                            [pinfo pinfo?])]
                  
                  [program->compiled-program 
                   (program? . -> . compiled-program?)])