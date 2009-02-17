#lang scheme/base

;; This program translates beginner-level languages into Java source.
;; We pattern match against the language given by:
;;
;; http://docs.plt-scheme.org/htdp-langs/beginner.html


(require scheme/match
         scheme/list
         scheme/string
         scheme/contract
         "env.ss")




;; A program is a (listof (or/c defn? expr? test-case? library-require?))


;; program: any -> boolean
;; Returns true if the datum is a program.
(define (program? datum)
  (and (list? datum)
       (andmap (lambda (x) 
                 (or (defn? x)
                     (expression? x)
                     (test-case? x)
                     (library-require? x)))
               datum)))


;; expression?: any -> boolean
;; Returns true if the datum is an expression.
(define (expression? an-expr)
  (and (not (defn? an-expr))
       (not (test-case? an-expr))
       (not (library-require? an-expr))))


;; defn?: s-expression -> boolean
(define (defn? an-s-exp)
  (match an-s-exp
    [(list 'define rest ...)
     #t]
    [(list 'define-struct rest ...)
     #t]
    [else
     #f]))

;; test-case?: s-expression -> boolean
(define (test-case? an-sexp)
  (match an-sexp
    [(list 'check-expect e ...)
     #t]
    [(list 'check-within e ...)
     #t]
    [(list 'check-error e ...)
     #t]
    [else
     #f]))

;; library-require?: s-expression -> boolean
(define (library-require? an-sexp)
  (match an-sexp
    [(list 'require s ...)
     #t]
    [else
     #f]))




;; program->java-string: program -> (values string program-info)
;; Consumes a program and returns a java string fragment that represents the
;; content of the program.
(define (program->java-string program)
  (let* ([pinfo (program-analyze program)]
         [toplevel-defined-ids (program-info-defined-ids pinfo)])
    (let ([compiled-code
           (let loop ([program program])
             (cond [(empty? program) ""]
                   [else
                    (let ([new-java-code
                           (cond [(defn? (first program))
                                  (definition->java-string 
                                    (first program) 
                                    toplevel-defined-ids)]
                                 [(test-case? (first program))
                                  "// Test case erased\n"]
                                 [(library-require? (first program))
                                  (error 'program->java-string 
                                         "I don't know how to handle require")]
                                 [(expression? (first program))
                                  (string-append 
                                   "static { org.plt.Kernel.identity("
                                   (expression->java-string 
                                    (first program) 
                                    toplevel-defined-ids) 
                                   "); }")])]
                          [rest-java-code (loop (rest program))])
                      (string-append new-java-code "\n" rest-java-code))]))])
      (values compiled-code pinfo))))




;; definition->java-string: definition (listof symbol)-> string
;; Consumes a definition (define or define-struct) and produces a string
;; that maps that definition to a static, public function that consumes
;; Object arguments and produces Objects.  The second value is the
;; list of bound symbols from the definition.
;;
;; Structure definitions map to static inner classes with transparent fields.
(define (definition->java-string defn bound-ids)
  (match defn
    [(list 'define (list fun args ...) body)
     (function-definition->java-string fun args body bound-ids)]
    [(list 'define (? symbol? fun) (list 'lambda (list args ...) body))
     (function-definition->java-string fun args body bound-ids)]
    [(list 'define (? symbol? id) body)
     (variable-definition->java-string id body bound-ids)]
    [(list 'define-struct id (list fields ...))
     (struct-definition->java-string id fields)]))


;; function-definition->java-string: symbol (listof symbol) expr (listof symbol) -> string
;; Converts the function definition into a static function declaration whose
;; return value is an object.
(define (function-definition->java-string fun args body bound-ids)
  (format "static public Object ~a(~a) { return ~a; }"
          (identifier->java-identifier fun (list fun))
          (string-join (map (lambda (x)
                              (string-append 
                               "Object "
                               (symbol->string
                                (identifier->java-identifier x (cons fun args)))))
                            args)
                       ", ")
          (expression->java-string body (append (cons fun args) bound-ids))))



;; variable-definition->java-string: symbol expr (listof symbol) -> string
;; Converts the variable definition into a static variable declaration.
(define (variable-definition->java-string id body bound-ids)
  (format "static Object ~a; static { ~a = ~a; }" 
          (identifier->java-identifier id (list id))
          (identifier->java-identifier id (list id))
          (expression->java-string body (cons id bound-ids))))




;; struct-definition->java-string: symbol (listof symbol) -> string
(define (struct-definition->java-string id fields)
  
  ;; field->accessor-name: symbol symbol -> symbol
  ;; Given a structure name and a field, return the accessor.
  (define (field->accessor-name struct-name field-name)
    (string->symbol
     (string-append (symbol->string struct-name)
                    "-"
                    (symbol->string field-name))))
  
  (format "static public class ~a implements org.plt.types.Struct { ~a \n ~a\n ~a\n}\n~a \n ~a" 
          (identifier->java-identifier id (list id))
          (string-join (map (lambda (a-field)
                              (format "public Object ~a;"
                                      (identifier->java-identifier a-field (cons id fields))))
                            fields)
                       "\n")
          
          ;; default constructor
          (format "public ~a(~a) { ~a }"
                  (identifier->java-identifier id (list id))
                  (string-join (map (lambda (i) (format "Object ~a"
                                                        (identifier->java-identifier i (list i))))
                                    fields) 
                               ",")
                  (string-join (map (lambda (i) (format "this.~a = ~a;" 
                                                        (identifier->java-identifier i (list i))
                                                        (identifier->java-identifier i (list i))))
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
                  (identifier->java-identifier id (list id))
                  (expression->java-string (foldl (lambda (a-field acc)
                                                    (let ([acc-id (field->accessor-name id a-field)])
                                                      `(and (equal? (,acc-id this)
                                                                    (,acc-id other)) 
                                                            ,acc)))
                                                  'true
                                                  fields)
                                           (list* 'other 'this 
                                                  (map (lambda (f) (field->accessor-name id f)) 
                                                       fields))))
          
          ;; make-id
          (format "static public Object ~a(~a) { return new ~a(~a); }"
                  (let ([make-id (string->symbol 
                                  (string-append "make-" (symbol->string id)))])
                    (identifier->java-identifier  make-id (list make-id)))
                  (string-join (build-list (length fields) (lambda (i) (format "Object id~a" i)))
                               ",")
                  (identifier->java-identifier id (list id))
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
                            (identifier->java-identifier  acc-id
                                                          (list acc-id)))
                          (identifier->java-identifier id (list id))
                          (identifier->java-identifier a-field (list a-field))))
                fields)
           "\n")))



;; expression->java-string: expr (listof symbol) -> string
;; Translates an expression into a Java expression string whose evaluation
;; should produce an Object.
(define (expression->java-string expr bound-ids)
  (let ([expr (desugar-var-arity-application expr)])
    (match expr
      [(list 'cond [list questions answers] ... [list 'else answer-last])
       (let loop ([questions questions]
                  [answers answers])
         (cond
           [(empty? questions)
            (expression->java-string answer-last bound-ids)]
           [else
            (format "(((org.plt.types.Logic)(~a)).isTrue() ? (~a) : (~a))"
                    (expression->java-string (first questions) bound-ids)
                    (expression->java-string (first answers) bound-ids)
                    (loop (rest questions) (rest answers)))]))]
      
      
      [(list 'cond [list questions answers] ... [list question-last answer-last])
       (let loop ([questions questions]
                  [answers answers])
         (cond
           [(empty? questions)
            (format "(((org.plt.types.Logic)(~a)).isTrue() ? (~a) : org.plt.Kernel.error(org.plt.types.Symbol.makeInstance(\"cond\"), \"Fell out of cond\"))"
                    (expression->java-string question-last bound-ids)
                    (expression->java-string answer-last bound-ids))]
           [else
            (format "(((org.plt.types.Logic)(~a)).isTrue() ? (~a) : (~a))"
                    (expression->java-string (first questions) bound-ids)
                    (expression->java-string (first answers) bound-ids)
                    (loop (rest questions) (rest answers)))]))]
      
      [(list 'if test consequent alternative)
       (format "(((org.plt.types.Logic)(~a)).isTrue() ? (~a) : (~a))"
               (expression->java-string test bound-ids)
               (expression->java-string consequent bound-ids)
               (expression->java-string alternative bound-ids))]
      
      [(list 'and expr ...)
       (string-append "(("
                      (string-join (map (lambda (e)
                                          (format "(((org.plt.types.Logic)~a).isTrue())"
                                                  (expression->java-string e bound-ids)))
                                        expr) 
                                   "&&")
                      ") ? org.plt.types.Logic.TRUE : org.plt.types.Logic.FALSE)")]
      
      [(list 'or expr ...)
       (string-append "(("
                      (string-join  (map (lambda (e)
                                           (format "(((org.plt.types.Logic)~a).isTrue())"
                                                   (expression->java-string e bound-ids)))
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
       (cond [(member expr bound-ids)
              (symbol->string
               (identifier->java-identifier expr bound-ids))]
             [else
              (translate-toplevel-id expr)])]
      
      ;; Quoted symbols
      [(list 'quote datum)
       (format "(org.plt.types.Symbol.makeInstance(\"~a\"))"
               datum)]
      
      ;; Function call/primitive operation call
      [(list (? symbol? id) exprs ...)
       (application-expression->java-string id exprs bound-ids)
       ])))


;; application-expression->java-string: symbol (listof expr) (listof symbol) -> string
;; Converts the function application to a string.
(define (application-expression->java-string id exprs bound-ids)
  (cond
    [(unimplemented-java-kernel-id? id)
     (error 'expression->java-string 
            "Function ~s hasn't yet been implemented in this compiler."
            id)]
    [else
     (format "(~a(~a))"
             (identifier->java-identifier id bound-ids)
             (string-join (map (lambda (e) 
                                 (expression->java-string e bound-ids))
                               exprs) ","))]))


(define toplevel-env empty-env)

(define (lookup-toplevel-id a-name)
  (env-lookup toplevel-env a-name))

(define (register-plain-toplevel-id-constant! a-name java-string)
  (set! toplevel-env
        (env-extend-constant toplevel-env a-name java-string)))




;; translate-toplevel-id: symbol -> string
;; Translates the use of a toplevel identifier to the appropriate
;; Java code.
(define (translate-toplevel-id an-id)
  (match (lookup-toplevel-id an-id)
    ['#f
     (error 'translate-toplevel-id 
            "Moby doesn't know about toplevel primitive ~s."
            an-id)]
    [(struct binding:constant (name java-string))
     java-string]
    #;[(struct id-info:function (name args optargs ->java-string))
       (error 'translate-toplevel-id
              "Moby doesn't allow higher-order use of ~s." an-id)]))


;; translate-toplevel-application: symbol expression -> string
(define (translate-toplevel-application an-operator-id operands)
  (void)
  #;(let* ([fail-f 
          (lambda ()
            (error 'translate-toplevel-id 
                   "Moby doesn't know about toplevel primitive ~s."
                   an-id))]
         [record (hash-ref (registered-toplevel-ids) an-id fail-f)])
    (match record
      [(struct id-info:constant (name ->java-string))
       (->java-string an-id)]
      [(struct id-info:function (name args optargs ->java-string))
       (error 'translate-toplevel-id
              "Moby doesn't allow higher-order use of ~s." an-id)])))





;; We register the toplevel identifiers here.
(register-plain-toplevel-id-constant! 'null "org.plt.types.Empty.EMPTY")
(register-plain-toplevel-id-constant! 'empty "org.plt.types.Empty.EMPTY")
(register-plain-toplevel-id-constant! 'true "org.plt.types.Logic.TRUE")
(register-plain-toplevel-id-constant! 'false "org.plt.types.Logic.FALSE")
(register-plain-toplevel-id-constant! 'eof "org.plt.types.EofObject.EOF")
(for ([kernel-constant '(pi e)])
  (register-plain-toplevel-id-constant! kernel-constant
                                        (format "org.plt.Kernel.~a" 
                                                kernel-constant)))







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
        [else
         (error 'number->java-string "Don't know how to handle ~s yet" a-num)]))





;; identifier->java-identifier: symbol (listof symbol) -> symbol
;; Converts identifiers into ones compatible with Java.
(define (identifier->java-identifier an-id bound-ids)
  ;; Special character mappings for identifiers
  (define char-mappings 
    #hash((#\- . "_dash_")
          (#\_ . "_underline_")
          (#\? . "_question_")
          (#\! . "_bang_")
          (#\. . "_dot_")
          (#\: . "_colon_")
          (#\= . "_equal_")
          (#\# . "_pound_")
          (#\$ . "_dollar_")
          (#\% . "_percent_")
          (#\^ . "_tilde_")
          (#\& . "_and_")
          (#\* . "_star_")
          (#\+ . "_plus_")
          (#\* . "_star_")
          (#\/ . "_slash_")
          (#\< . "_lessthan_")
          (#\> . "_greaterthan_")
          (#\~ . "_tilde_")))
  (let* ([chars (string->list (symbol->string an-id))]
         [translated-chunks 
          (map (lambda (ch) (hash-ref char-mappings ch (string ch))) chars)]
         [translated-id
          (string->symbol
           (string-join translated-chunks ""))])
    (cond
      [(and (not (member an-id bound-ids))
            (implemented-java-kernel-id? an-id))
       (string->symbol
        (string-append 
         (if (world-primitive-id? an-id)
             "org.plt.WorldKernel."
             "org.plt.Kernel.")
         (symbol->string translated-id)))]
      [else
       translated-id])))




(define ALL-JAVA-KERNEL-SYMBOLS
  '(+
    -
    *
    /
    >=
    >
    <=
    <
    =
    =~
    abs
    acos
    add1
    angle
    asin
    atan
    ceiling
    complex?
    conjugate
    cos
    cosh
    current-seconds
    denominator
    e
    even?
    exact->inexact
    exact?
    exp
    expt
    floor
    gcd
    imag-part
    inexact->exact
    inexact?
    integer->char
    integer?
    lcm
    log
    magnitude
    make-polar
    max
    min
    modulo
    negative?
    number->string
    number?
    numerator
    odd?
    pi
    positive?
    quotient
    random
    rational?
    real-part
    real?
    remainder
    round
    sgn
    sin
    sinh
    sqr
    sqrt
    sub1
    tan
    zero?
    boolean=?
    boolean?
    false?
    not
    symbol->string
    symbol=?
    symbol?
    append
    assq
    caaar
    caadr
    caar
    cadar
    cadddr
    caddr
    cadr
    car
    cdaar
    cdadr
    cdar
    cddar
    cdddr
    cddr
    cdr
    cons
    cons?
    eighth
    empty?
    fifth
    first
    fourth
    length
    list
    list*
    list-ref
    member
    memq
    memv
    null
    null?
    pair?
    rest
    reverse
    second
    seventh
    sixth
    third
    make-posn
    posn-x
    posn-y
    posn?
    char->integer
    char-alphabetic?
    char-ci<=?
    char-ci<?
    char-ci=?
    char-ci>=?
    char-ci>?
    char-downcase
    char-lower-case?
    char-numeric?
    char-upcase
    char-upper-case?
    char-whitespace?
    char<=?
    char<?
    char=?
    char>=?
    char>?
    char?
    format
    list->string
    make-string
    string
    string->list
    string->number
    string->symbol
    string-append
    string-ci<=?
    string-ci<?
    string-ci=?
    string-ci>=?
    string-ci>?
    string-copy
    string-length
    string-ref
    string<=?
    string<?
    string=?
    string>=?
    string>?
    string?
    substring
    image=?
    image?
    =~
    eof
    eof-object?
    eq?
    equal?
    equal~?
    eqv?
    error
    exit
    identity
    struct?
    
    
    ))



(define IMPLEMENTED-JAVA-KERNEL-SYMBOLS
  '(identity
    
    ;; Numerics
    +
    -
    *
    /
    >=
    >
    <=
    <
    =
    =~
    number->string
    even?
    odd?
    positive?
    negative?
    number?
    rational?
    quotient
    remainder
    numerator
    denominator
    integer?
    real?
    
    abs
    acos
    asin
    atan
    random
    max
    min
    sqr
    sqrt
    modulo
    add1
    sub1
    zero?
    exp
    expt
    sgn
    log
    gcd
    lcm
    round
    
    pi
    e
    floor
    ceiling
    sin
    cos
    tan
    sinh
    cosh
    
    angle
    conjugate
    magnitude
    
    ;; Logic
    not
    false?
    boolean?
    boolean=?
    equal?
    eq?
    eqv?
    equal~?
    
    ;; Characters
    char?
    char=?
    char<?
    char<=?
    char>?
    char>=?
    char-downcase
    char-lower-case?
    char-numeric?
    char-upcase
    char-upper-case?
    char-whitespace?
    char-alphabetic?
    char-ci<=?
    char-ci<?
    char-ci=?
    char-ci>=?
    char-ci>?
    char->integer
    integer->char
    
    ;; Symbols
    symbol=?
    symbol->string
    ;; Strings
    string=?
    symbol?
    string?
    string>?
    string>=?
    string<?
    string<=?
    substring
    string-length
    string-ref
    string-copy
    string->number
    string-ci<=?
    string-ci<?
    string-ci=?
    string-ci>=?
    string-ci>?
    string->list
    string->symbol 
    string-append 
    list->string 
    make-string 
    string 
    
    ;; World
    empty-scene
    place-image
    circle
    nw:rectangle
    rectangle
    key=?
    text
    
    ;; Images
    
    ;; Fixme: -kernel-create-image is a special case of a function not in the original language.
    ;; We can fix this by extending expression to include a special "magic" identifier.  We should
    ;; ensure students don't accidently hit this function.
    -kernel-create-image 
    
    image-width
    image-height
    image?    
    
    ;; Pairs
    empty?
    first
    second
    third
    fourth
    fifth
    sixth
    seventh
    eighth
    rest
    cons
    pair?
    cons?
    null?
    length
    list
    list*
    empty
    null
    list-ref
    append
    member
    memq
    memv
    
    reverse
    
    caaar
    caadr
    caar
    cadar
    cadddr
    caddr
    cadr
    car
    cdaar
    cdadr
    cdar
    cddar
    cdddr
    cddr
    cdr
    
    struct?
    ;; Posn
    make-posn
    posn-x
    posn-y
    posn?
    
    ;; Eof
    eof
    eof-object?
    
    ;; Misc
    error
    current-seconds
    
    ))

(define WORLD-PRIMITIVE-SYMBOLS
  '(empty-scene
    place-image
    circle
    nw:rectangle
    rectangle
    key=?
    text
    -kernel-create-image     
    image-width
    image-height
    image?))



;; java-kernel-id?: symbol -> boolean
;; Returns true if the java-identifier should be treated as part of the Kernel.
(define (implemented-java-kernel-id? an-id)
  (and (member an-id IMPLEMENTED-JAVA-KERNEL-SYMBOLS)
       #t))

(define (world-primitive-id? an-id)
  (and (member an-id WORLD-PRIMITIVE-SYMBOLS)
       #t))


;; unimplemented-java-kernel-id?: symbol -> boolean
;; Returns true if the java-identifier hasn't been implemented yet.
(define (unimplemented-java-kernel-id? an-id)
  (and (member an-id ALL-JAVA-KERNEL-SYMBOLS)
       (not (implemented-java-kernel-id? an-id))
       #t))


;; todo-list: -> (listof symbol)
;; Produces the list of symbols of kernel functions we haven't yet implemented.
(define (todo-list)
  (filter unimplemented-java-kernel-id? ALL-JAVA-KERNEL-SYMBOLS))



;; desugar-var-arity-application: expr -> expr
;; Desugars the multi-arity functions into binary applications.
(define (desugar-var-arity-application an-expr)
  (match an-expr
    [(list '+ x ...)
     (cond
       [(empty? x)
        0]
       [else
        (foldl (lambda (arg acc)
                 `(+ ,acc ,arg))
               (first x)
               (rest x))])]
    
    [(list '- x z ...)
     (cond [(empty? z)
            `(- 0 ,x)]
           [else
            (foldl (lambda (arg acc)
                     `(- ,acc ,arg))
                   x
                   z)])]
    
    [(list '* x ...)
     (cond [(empty? x)
            1]
           [else
            (foldl (lambda (arg acc)
                     `(* ,acc ,arg))
                   (first x)
                   (rest x))])]
    
    [(list '/ x z ...)
     (cond
       [(empty? z)
        x]
       [else
        (foldl (lambda (arg acc)
                 `(/ ,acc ,arg))
               x
               z)])]
    
    [(list '< x y ...)
     (desugar-comparisons '< x y)]
    
    [(list '<= x y ...)
     (desugar-comparisons '<= x y)]
    
    [(list '> x y ...)
     (desugar-comparisons '> x y)]
    
    [(list '>= x y ...)
     (desugar-comparisons '>= x y)]
    
    [(list 'max x y ...)
     (foldl (lambda (arg acc) `(max ,acc ,arg)) x y)]
    
    [(list 'min x y ...)
     (foldl (lambda (arg acc) `(min ,acc ,arg)) x y)]
    
    [(list 'append x ...)
     (cond
       [(empty? x)
        'empty]
       [else
        (foldl (lambda (arg acc) `(append ,acc ,arg)) 
               (first x) 
               (rest x))])]
    
    [(list 'list x ...)
     (cond
       [(empty? x)
        'empty]
       [else
        (foldr (lambda (arg acc) `(cons ,arg ,acc)) empty x)])]
    
    [(list 'list* x ... ys)
     `(append ,(desugar-var-arity-application (cons 'list x))
              ,ys)]
    
    [else
     an-expr]))


;; desugar-comparisons: symbol expr (listof expr) -> expr
(define (desugar-comparisons op x ys)
  (cond [(empty? ys)
         'true]
        [(empty? (rest ys))
         `(,op ,x ,(first ys))]
        [else
         `(and (,op ,x ,(first ys))
               ,(desugar-comparisons op (first ys) (rest ys)))]))




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; program-info captures the information we get from analyzing 
;; the program.
(define-struct program-info (defined-ids free-ids) #:transparent)
(define empty-program-info (make-program-info empty empty))

;; pinfo-accumulate-free-id: symbol program-info -> program-info
(define (pinfo-accumulate-free-id free-id pinfo)
  (struct-copy program-info pinfo
               [free-ids (cons free-id (program-info-free-ids pinfo))]))

;; pinfo-accumulate-defined-id: symbol program-info -> program-info
(define (pinfo-accumulate-defined-id defined-id pinfo)
  (struct-copy program-info pinfo
               [defined-ids (cons defined-id (program-info-defined-ids pinfo))]))


;; program-analyze: program [program-info] -> program-info
;; Collects which identifiers are free or definition-bound by the program.
(define (program-analyze a-program [pinfo empty-program-info])
  ;; fixme to do free variable analysis.  We want to error early if the user
  ;; tries to use an identifier that hasn't been bound.
  (program-analyze-collect-definitions a-program pinfo))


;; program-analyze-collect-definitions: program pinfo -> pinfo
(define (program-analyze-collect-definitions a-program pinfo)
  (cond [(empty? a-program)
         pinfo]
        [else
         (let ([updated-pinfo
                (cond [(defn? (first a-program))
                       (definition-analyze-collect-definitions (first a-program) pinfo)]
                      [(test-case? (first a-program))
                       ;; Test cases don't introduce any new definitions, so just return.
                       pinfo]
                      [(library-require? (first a-program))
                       ;; Fixme!
                       (error 'program-top-level-identifiers 
                              "I don't know how to handle require yet")]
                      [(expression? (first a-program))
                       ;; Expressions don't introduce any new definitions, so just return.
                       pinfo])])
           (program-analyze-collect-definitions (rest a-program)
                                                updated-pinfo))]))


;; definition-analyze-collect-definitions: definition program-info -> program-info
(define (definition-analyze-collect-definitions a-definition pinfo)
  (match a-definition
    [(list 'define (list fun args ...) body)
     (pinfo-accumulate-defined-id fun pinfo)]
    [(list 'define (? symbol? fun-id) (list 'lambda (list args ...) body))
     (pinfo-accumulate-defined-id fun-id pinfo)]
    [(list 'define (? symbol? id) body)
     ;; Expressions don't introduce any new definitions, so just return.
     (pinfo-accumulate-defined-id id pinfo)]
    [(list 'define-struct id (list fields ...))
     (foldl pinfo-accumulate-defined-id pinfo 
            (cons (string->symbol (format "make-~a" id))
                  (map (lambda (f)
                         (string->symbol (format "~a-~a" id f)))
                       fields)))]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;





(provide/contract [program->java-string (program? . -> . (values string? program-info?))]
                  [expression->java-string (expression? (listof symbol?) . -> . string?)]
                  [defn? (any/c . -> . boolean?)]
                  [expression? (any/c . -> . boolean?)]
                  
                  [struct program-info ([defined-ids (listof symbol?)]
                                        [free-ids (listof symbol?)])]
                  [program-analyze ((program?) (program-info?) . ->* . program-info?)])
