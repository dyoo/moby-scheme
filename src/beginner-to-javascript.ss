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
  (defns           ;; string
    toplevel-exprs ;; string
    pinfo          ;; pinfo
    ))



;; program->compiled-program: program [pinfo] -> compiled-program
;; Consumes a program and returns a compiled program.
;; If pinfo is provided, uses that as the base set of known toplevel definitions.
(define (program->compiled-program program [input-pinfo (get-base-pinfo)])
  (let* ([a-pinfo (program-analyze program input-pinfo)]
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
                                         "org.plt.Kernel.identity("
                                         (expression->javascript-string 
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
(define (definition->javascript-strings defn env a-pinfo)
  (match defn
    [(list 'define (list fun args ...) body)
     (values (function-definition->java-string fun args body env a-pinfo)
             "")]
    [(list 'define (? symbol? fun) (list 'lambda (list args ...) body))
     (values (function-definition->java-string fun args body env a-pinfo)
             "")]
    [(list 'define (? symbol? id) body)
     (variable-definition->javascript-strings id body env a-pinfo)]

    [(list 'define-struct id (list fields ...))
     (values (struct-definition->javascript-string id fields env a-pinfo)
             "")]))


;; function-definition->java-string: symbol (listof symbol) expr env pinfo -> string
;; Converts the function definition into a static function declaration whose
;; return value is an object.
(define (function-definition->java-string fun args body env a-pinfo)
  (let* ([munged-fun-id
          (identifier->munged-java-identifier fun)]
         [munged-arg-ids
          (map identifier->munged-java-identifier args)]
         [new-env 
          (env-extend-function env fun #f (length args) #f
                               (symbol->string munged-fun-id)
                               #:primitive? #f)]
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
            (expression->javascript-string body new-env a-pinfo))))



;; variable-definition->javascript-strings: symbol expr env pinfo -> (values string string)
;; Converts the variable definition into a static variable declaration and its
;; initializer at the toplevel.
(define (variable-definition->javascript-strings id body env a-pinfo)
  (let* ([munged-id (identifier->munged-java-identifier id)]
         [new-env (env-extend env (make-binding:constant id 
                                                         (symbol->string munged-id)
                                                         empty))])
    (values (format "var ~a; "
                    munged-id)
            (format "~a = ~a;" 
                    munged-id
                    (expression->javascript-string body new-env a-pinfo)))))




;; struct-definition->javascript-string: symbol (listof symbol) env pinfo -> string
(define (struct-definition->javascript-string id fields env a-pinfo)
  
  ;; field->accessor-name: symbol symbol -> symbol
  ;; Given a structure name and a field, return the accessor.
  (define (field->accessor-name struct-name field-name)
    (string->symbol
     (string-append (symbol->string struct-name)
                    "-"
                    (symbol->string field-name))))
  

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
   
   "\n"
   
   ;; make-id
   (format "function ~a(~a) { return new ~a(~a); }"
           (let ([make-id (string->symbol 
                           (string-append "make-" (symbol->string id)))])
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
                   (let ([acc-id (string->symbol
                                  (string-append (symbol->string id)
                                                 "-"
                                                 (symbol->string a-field)))])
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
           (identifier->munged-java-identifier id))))



;; expression->java-string: expr env pinfo -> string
;; Translates an expression into a Java expression string whose evaluation
;; should produce an Object.
(define (expression->javascript-string expr env a-pinfo)
  (match expr
    [(list 'local [list defns ...] body)
     (local-expression->javascript-string defns body env a-pinfo)]
    
    [(list 'cond [list questions answers] ... [list 'else answer-last])
     (let loop ([questions questions]
                [answers answers])
       (cond
         [(empty? questions)
          (expression->javascript-string answer-last env a-pinfo)]
         [else
          (format "((~a) ? (~a) : (~a))"
                  (expression->javascript-string (first questions) env a-pinfo)
                  (expression->javascript-string (first answers) env a-pinfo)
                  (loop (rest questions) (rest answers)))]))]
    
    
    [(list 'cond [list questions answers] ... [list question-last answer-last])
     (let loop ([questions questions]
                [answers answers])
       (cond
         [(empty? questions)
          (format "((~a) ? (~a) : 
                    org.plt.Kernel.error(org.plt.types.Symbol.makeInstance(\"cond\"),
                                         \"Fell out of cond\"))"
                  (expression->javascript-string question-last env a-pinfo)
                  (expression->javascript-string answer-last env a-pinfo))]
         [else
          (format "((~a) ? (~a) : (~a))"
                  (expression->javascript-string (first questions) env a-pinfo)
                  (expression->javascript-string (first answers) env a-pinfo)
                  (loop (rest questions) (rest answers)))]))]
    
    [(list 'if test consequent alternative)
     (format "((~a) ? (~a) : (~a))"
             (expression->javascript-string test env a-pinfo)
             (expression->javascript-string consequent env a-pinfo)
             (expression->javascript-string alternative env a-pinfo))]
    
    [(list 'and expr ...)
     (string-append "("
                    (string-join (map (lambda (e)
                                        (format "(~a)"
                                                (expression->javascript-string e env a-pinfo)))
                                      expr) 
                                 "&&")
                    ")")]
    
    [(list 'or expr ...)
     (string-append "("
                    (string-join  (map (lambda (e)
                                         (format "(~a)"
                                                 (expression->javascript-string e env a-pinfo)))
                                       expr) 
                                  "||")
                    ")")]
        
    ;; Numbers
    [(? number?)
     (number->javascript-string expr)]
    
    ;; Strings
    [(? string?)
     (string->javascript-string expr)]
    
    ;; Characters
    [(? char?)
     (char->javascript-string expr)]
    
    ;; Identifiers
    [(? symbol?)
     (identifier-expression->javascript-string expr env a-pinfo)]
    
    ;; Quoted symbols
    [(list 'quote datum)
     (format "(org.plt.types.Symbol.makeInstance(\"~a\"))"
             datum)]
    
    ;; Function call/primitive operation call
    [(list (? symbol? id) exprs ...)
     (application-expression->javascript-string id exprs env a-pinfo)]))


;; local-expression->javascript-string: (listof defn) expr env pinfo -> string
(define (local-expression->javascript-string defns body env a-pinfo)
  (let ([inner-compiled-program 
         (program->compiled-program (append defns (list body)) 
                                    (pinfo-update-env a-pinfo env))])
    (format "(function() {
               ~a

               return ~a
              })()"
            (compiled-program-defns inner-compiled-program)
            ;; Complete kludge... How do we do this better?
            (regexp-replace #px"^\\s+"
                            (compiled-program-toplevel-exprs inner-compiled-program)
                            ""))))



;; application-expression->java-string: symbol (listof expr) env pinfo -> string
;; Converts the function application to a string.
(define (application-expression->javascript-string id exprs env a-pinfo)
  (let ([operator-binding (env-lookup env id)]
        [operand-strings 
         (map (lambda (e) 
                (expression->javascript-string e env a-pinfo))
              exprs)])
    (match operator-binding
      ['#f
       (error 'application-expression->java-string
              "Moby doesn't know about ~s" id)]
      
      [(struct binding:constant (name java-string permissions))
       (format "((~a).apply(null, [~a]))" 
               java-string 
               (string-join operand-strings ", "))]
      
      [(struct binding:function (name module-path min-arity var-arity? 
                                      java-string permissions primitive?))
       (unless (>= (length exprs)
                   min-arity)
         (error 'application-expression->java-string
                "Minimal arity of ~s not met.  Operands were ~s"
                id
                exprs))
       (cond
         [var-arity?
          (cond [(> min-arity 0)
                 (format "~a(~a, [~a])"
                         java-string
                         (string-join (take operand-strings min-arity) ",")
                         (string-join (list-tail operand-strings min-arity)
                                      ","))]
                [else
                 (format "~a([~a])"
                         java-string
                         (string-join (list-tail operand-strings min-arity)
                                      ","))])]
         [else
          (format "(~a(~a))" 
                  java-string 
                  (string-join operand-strings ","))])])))



;; identifier-expression->javascript-string: symbol env pinfo -> string
;; Translates the use of a toplevel identifier to the appropriate
;; Java code.
(define (identifier-expression->javascript-string an-id an-env a-pinfo)
  (match (env-lookup an-env an-id)
    ['#f
     (error 'translate-toplevel-id "Moby doesn't know about ~s." an-id)]
    [(struct binding:constant (name java-string permissions))
     java-string]
    [(struct binding:function (name module-path min-arity var-arity? java-string permissions primitive?))
     (cond
       [var-arity?
        (format "(function(args) {
                    return ~a.apply(null, args);
                  })"
                java-string)]
       [else
        (format "(function(args) {
                    return ~a(~a);
                 })"
                java-string
                (string-join (for/list ([i (in-range min-arity)])
                               (format "args[~a]" i))
                             ", "))])]))





;; number->java-string: number -> string
(define (number->javascript-string a-num)
  (cond [(integer? a-num)
         ;; Fixme: we need to handle exact/vs/inexact issue.
         ;; We probably need the numeric tower.
         (format "(org.plt.types.Rational.makeInstance(~a, 1))" (inexact->exact a-num))]
        [(and (inexact? a-num)
              (real? a-num))
         (format "(org.plt.types.FloatPoint.makeInstance(\"~s\"))" a-num)]
        [(rational? a-num)
         (format "(org.plt.types.Rational.makeInstance(~s, ~s))" 
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
  (string-append "(org.plt.types.Character.makeInstance(\""
                 (if (char=? a-char #\") "\\" (string a-char))
                 "\"))"))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;





(provide/contract [struct compiled-program ([defns 
                                              string?]
                                            [toplevel-exprs 
                                             string?]
                                            [pinfo pinfo?])]
                  
                  [program->compiled-program 
                   (program? . -> . compiled-program?)])