#lang scheme/base

;; This program translates beginner-level languages into Java source.
;; We pattern match against the language given by:
;;
;; http://docs.plt-scheme.org/htdp-langs/beginner.html


(require scheme/match
         scheme/contract
         (only-in scheme/list empty? first rest empty)
         "env.ss"
         "pinfo.ss"
         "helpers.ss")


;; A compiled program is a:
(define-struct compiled-program
  (defns           ;; string
    toplevel-exprs ;; string
    pinfo          ;; pinfo
    ))



;; program->compiled-program: program [pinfo] -> compiled-program
;; Consumes a program and returns a compiled program.
;; If pinfo is provided, uses that as the base set of known toplevel definitions.
(define (program->compiled-program program [input-pinfo (get-base-pinfo 
                                                         'base)])
  (let* ([a-pinfo (program-analyze/pinfo program input-pinfo)]
         [toplevel-env (pinfo-env a-pinfo)])
    
    (let loop ([program program]
               [defns ""]
               [tops ""])
      (cond [(empty? program)
             (make-compiled-program defns tops a-pinfo)]
            [else
             (cond [(defn? (first program))
                    (let-values ([(defn-string expr-string)
                                  (definition->java-strings 
                                    (first program) 
                                    toplevel-env
                                    a-pinfo)])
                      
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
                                         "plt.Kernel.identity("
                                         (expression->java-string 
                                          (first program) 
                                          toplevel-env
                                          a-pinfo)
                                         ");"))])]))))



;; definition->java-string: definition env pinfo -> (values string string)
;; Consumes a definition (define or define-struct) and produces two strings.
;; The first maps a definitions string.
;; The second value is the expression that will be evaluated at the toplevel.
;;
;; Structure definitions map to static inner classes with transparent fields.
(define (definition->java-strings defn env a-pinfo)
  (match defn
    [(list 'define (list fun args ...) body)
     (values (function-definition->java-string fun args body env a-pinfo)
             "")]
    [(list 'define (? symbol? fun) (list 'lambda (list args ...) body))
     (values (function-definition->java-string fun args body env a-pinfo)
             "")]
    [(list 'define (? symbol? id) body)
     (variable-definition->java-strings id body env a-pinfo)]

    [(list 'define-struct id (list fields ...))
     (values (struct-definition->java-string id fields env a-pinfo)
             "")]))


;; function-definition->java-string: symbol (listof symbol) expr env pinfo -> string
;; Converts the function definition into a method declaration whose
;; return value is an object.
(define (function-definition->java-string fun args body env a-pinfo)
  (let* ([munged-fun-id
          (identifier->munged-java-identifier fun)]
         [munged-arg-ids
          (map identifier->munged-java-identifier args)]
         [new-env 
          (env-extend-function env fun #f (length args) #f
                               (symbol->string munged-fun-id))]
         [new-env
          (foldl (lambda (arg-id env) 
                   (env-extend env (make-binding:constant arg-id 
                                                          (symbol->string
                                                           (identifier->munged-java-identifier arg-id))
                                                          empty)))
                 new-env
                 args)])
    (format "public Object ~a(~a) { return ~a; }"
            munged-fun-id
            (string-join (map (lambda (arg-id)
                                (string-append "Object " (symbol->string arg-id)))
                              munged-arg-ids)
                         ", ")
            (expression->java-string body new-env a-pinfo))))



;; variable-definition->java-string: symbol expr env pinfo -> (values string string)
;; Converts the variable definition into a static variable declaration and its
;; initializer at the toplevel.
(define (variable-definition->java-strings id body env a-pinfo)
  (let* ([munged-id (identifier->munged-java-identifier id)]
         [new-env (env-extend env (make-binding:constant id 
                                                         (symbol->string munged-id)
                                                         empty))])
    (values (format "Object ~a; "
                    munged-id)
            (format "~a = ~a;" 
                    munged-id
                    (expression->java-string body new-env a-pinfo)))))




;; struct-definition->java-string: symbol (listof symbol) env pinfo -> string
(define (struct-definition->java-string id fields env a-pinfo)
  
  ;; field->accessor-name: symbol symbol -> symbol
  ;; Given a structure name and a field, return the accessor.
  (define (field->accessor-name struct-name field-name)
    (string->symbol
     (string-append (symbol->string struct-name)
                    "-"
                    (symbol->string field-name))))
  
  (format "public class ~a implements plt.types.Struct { ~a \n ~a\n ~a\n}\n~a \n ~a\n ~a" 
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
                                             new-env)
                                           a-pinfo))
          
          ;; make-id
          (format "public Object ~a(~a) { return new ~a(~a); }"
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
                  (format "public Object ~a(Object obj) { return ((~a)obj).~a; }"
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
          (format "public plt.types.Logic ~a(Object obj) { return obj instanceof ~a ? plt.types.Logic.TRUE : plt.types.Logic.FALSE; }"
                  (identifier->munged-java-identifier (string->symbol (format "~a?" id)))
                  (identifier->munged-java-identifier id))))



;; expression->java-string: expr env pinfo -> string
;; Translates an expression into a Java expression string whose evaluation
;; should produce an Object.
(define (expression->java-string expr env a-pinfo)
  (match expr
    [(list 'local [list defns ...] body)
     (local-expression->java-string defns body env a-pinfo)]
    [(list 'cond [list questions answers] ... [list 'else answer-last])
     (let loop ([questions questions]
                [answers answers])
       (cond
         [(empty? questions)
          (expression->java-string answer-last env a-pinfo)]
         [else
          (format "(((plt.types.Logic)(~a)).isTrue() ? (~a) : (~a))"
                  (expression->java-string (first questions) env a-pinfo)
                  (expression->java-string (first answers) env a-pinfo)
                  (loop (rest questions) (rest answers)))]))]
    
    
    [(list 'cond [list questions answers] ... [list question-last answer-last])
     (let loop ([questions questions]
                [answers answers])
       (cond
         [(empty? questions)
          (format "(((plt.types.Logic)(~a)).isTrue() ? (~a) : 
                      plt.Kernel.error(plt.types.Symbol.makeInstance(\"cond\"), \"Fell out of cond\"))"
                  (expression->java-string question-last env a-pinfo)
                  (expression->java-string answer-last env a-pinfo))]
         [else
          (format "(((plt.types.Logic)(~a)).isTrue() ? (~a) : (~a))"
                  (expression->java-string (first questions) env a-pinfo)
                  (expression->java-string (first answers) env a-pinfo)
                  (loop (rest questions) (rest answers)))]))]
    
    [(list 'if test consequent alternative)
     (format "(((plt.types.Logic)(~a)).isTrue() ? (~a) : (~a))"
             (expression->java-string test env a-pinfo)
             (expression->java-string consequent env a-pinfo)
             (expression->java-string alternative env a-pinfo))]
    
    [(list 'and expr ...)
     (string-append "(("
                    (string-join (map (lambda (e)
                                        (format "(((plt.types.Logic)~a).isTrue())"
                                                (expression->java-string e env a-pinfo)))
                                      expr) 
                                 "&&")
                    ") ? plt.types.Logic.TRUE : plt.types.Logic.FALSE)")]
    
    [(list 'or expr ...)
     (string-append "(("
                    (string-join  (map (lambda (e)
                                         (format "(((plt.types.Logic)~a).isTrue())"
                                                 (expression->java-string e env a-pinfo)))
                                       expr) 
                                  "||")
                    ") ? plt.types.Logic.TRUE : plt.types.Logic.FALSE)")]
    
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
     (identifier-expression->java-string expr env a-pinfo)]
    
    ;; Quoted symbols
    [(list 'quote datum)
     (format "(plt.types.Symbol.makeInstance(\"~a\"))"
             datum)]
    
    ;; Function call/primitive operation call
    [(list (? symbol? id) exprs ...)
     (application-expression->java-string id exprs env a-pinfo)]))



;; local-expression->java-string: (listof defn) expr env pinfo -> string
(define (local-expression->java-string defns body env a-pinfo)
  (let ([inner-compiled-program 
         (program->compiled-program (append defns (list body)) 
                                    (pinfo-update-env a-pinfo env))])
    (format "(new plt.types.Callable() {
               ~a
               public Object call(Object[] args) {
                 return ~a
               }
              }).call(new Object[] {})"
            (compiled-program-defns inner-compiled-program)
            ;; Complete kludge... How do we do this better?
            (regexp-replace #px"^\\s+"
                            (compiled-program-toplevel-exprs inner-compiled-program)
                            ""))))



;; application-expression->java-string: symbol (listof expr) env pinfo -> string
;; Converts the function application to a string.
(define (application-expression->java-string id exprs env a-pinfo)
  (let ([operator-binding (env-lookup env id)]
        [operand-strings 
         (map (lambda (e) 
                             (expression->java-string e env a-pinfo))
                           exprs)])
    (match operator-binding
      ['#f
       (error 'application-expression->java-string
              "Moby doesn't know about ~s" id)]
      
      [(struct binding:constant (name java-string permissions))
       (format "(((plt.types.Callable) ~a).call(new Object[] {~a}))" 
               java-string 
               (string-join operand-strings ", "))]
      
      [(struct binding:function (name module-path min-arity var-arity? 
                                      java-string permissions primitive?))
       (unless (>= (length exprs)
                   min-arity)
         (error 'application-expression->java-string
                "Minimal arity (~s) of ~s not met.  Operands were ~s"
                (length exprs)
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



;; identifier-expression->java-string: symbol env pinfo -> string
;; Translates the use of a toplevel identifier to the appropriate
;; Java code.
(define (identifier-expression->java-string an-id an-env a-pinfo)
  (match (env-lookup an-env an-id)
    ['#f
     (error 'translate-toplevel-id "Moby doesn't know about ~s." an-id)]
    [(struct binding:constant (name java-string permissions))
     java-string]
    [(struct binding:function (name module-path min-arity var-arity? java-string permissions primitive?))
     (cond
       [var-arity?
        (format "(new plt.types.Callable() {
                      public Object call(Object[] args) {
                          return ~a(args);
                      }
                  })"
                java-string)]
       [else
        (format "(new plt.types.Callable() {
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
         (format "(new plt.types.Rational(~a, 1))" (inexact->exact a-num))]
        [(and (inexact? a-num)
              (real? a-num))
         (format "(plt.types.FloatPoint.fromString(\"~s\"))" a-num)]
        [(rational? a-num)
         (format "(new plt.types.Rational(~s, ~s))" 
                 (numerator a-num) 
                 (denominator a-num))]
        [(complex? a-num)
         (format "(new plt.types.Complex(~a, ~a))"
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
