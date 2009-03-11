#lang scheme/base
(require "env.ss"
         "helpers.ss"
         syntax/modresolve
         scheme/contract)



(define toplevel-env empty-env)


(define (lookup-toplevel-id a-name)
  (env-lookup toplevel-env a-name))

;; register-toplevel-constant!: symbol string -> void
(define (register-toplevel-constant! a-name java-string)
  (set! toplevel-env
        (env-extend-constant toplevel-env a-name java-string)))


;; register-toplevel-function!: symbol (or/c module-path #f) number boolean string -> void
(define (register-toplevel-function! a-name module-path arity vararity? java-string)
  (set! toplevel-env
        (env-extend-function toplevel-env a-name module-path arity vararity? java-string)))


;; get-toplevel-env: -> env
(define (get-toplevel-env)
  toplevel-env)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(define (register-htdp!)
  ;; We register the toplevel identifiers here.
  (register-toplevel-constant! 'null "org.plt.types.Empty.EMPTY")
  (register-toplevel-constant! 'empty "org.plt.types.Empty.EMPTY")
  (register-toplevel-constant! 'true "org.plt.types.Logic.TRUE")
  (register-toplevel-constant! 'false "org.plt.types.Logic.FALSE")
  (register-toplevel-constant! 'eof "org.plt.types.EofObject.EOF")
  
  (for ([kernel-constant '(pi e)])
    (register-toplevel-constant! kernel-constant
                                 (format "org.plt.Kernel.~a" 
                                         kernel-constant)))
  
  ;; Now let's register the htdp primitive functions
  (let ([r (lambda (id arity (vararity? #f))
             (register-toplevel-function! id 
                                          (resolve-module-path 'lang/htdp-beginner #f)
                                          arity
                                          vararity?
                                          (format "org.plt.Kernel.~a"
                                                  (identifier->munged-java-identifier id))))])
    
    ;; Numerics
    (r '+ 0 #t)
    (r '- 1 #t)
    (r '* 0 #t)
    (r '/ 1 #t)
    (r '>= 2 #t)
    (r '> 2 #t)
    (r '<= 2 #t)
    (r '< 2 #t)
    (r '= 2 #t)
    (r '=~ 3)
    (r 'number->string 1)
    (r 'even? 1)
    (r 'odd? 1)
    (r 'positive? 1)
    (r 'negative? 1)
    (r 'number? 1)
    (r 'rational? 1)
    (r 'quotient 1)
    (r 'remainder 1)
    (r 'numerator 1)
    (r 'denominator 1)
    (r 'integer? 1)
    (r 'real? 1)
    (r 'abs 1)
    (r 'acos 1)
    (r 'asin 1)
    (r 'atan 1)
    (r 'random 1)
    (r 'max 1 #t)
    (r 'min 1 #t)
    (r 'sqr 1)
    (r 'sqrt 1)
    (r 'modulo 2)
    (r 'add1 1)
    (r 'sub1 1)
    (r 'zero? 1)
    (r 'exp 1)
    (r 'expt 2)
    (r 'sgn 1)
    (r 'log 1)
    (r 'gcd 2 #t)
    (r 'lcm 2 #t)
    (r 'round 1)
    (r 'floor 1)
    (r 'ceiling 1)
    (r 'sin 1)
    (r 'cos 1)
    (r 'tan 1)
    (r 'sinh 1)
    (r 'cosh 1)
    (r 'angle 1)
    (r 'conjugate 1)
    (r 'magnitude 1)
    
    ;; Logic
    (r 'not 1)
    (r 'false? 1)
    (r 'boolean? 1)
    (r 'boolean=? 2)
    
    ;; Characters
    (r 'char? 1)
    (r 'char=? 2)
    (r 'char<? 2 #t)
    (r 'char<=? 2 #t)
    (r 'char>? 2 #t)
    (r 'char>=? 2 #t)
    (r 'char-ci<=? 2 #t)
    (r 'char-ci<? 2 #t)
    (r 'char-ci=? 2 #t)
    (r 'char-ci>=? 2 #t)
    (r 'char-ci>? 2 #t)
    (r 'char-downcase 1)
    (r 'char-lower-case? 1)
    (r 'char-numeric? 1)
    (r 'char-upcase 1)
    (r 'char-upper-case? 1)
    (r 'char-whitespace? 1)
    (r 'char-alphabetic? 1)
    (r 'char->integer 1)
    (r 'integer->char 1)
    
    ;; Symbols
    (r 'symbol=? 2)
    (r 'symbol->string 1)
    
    ;; Strings
    (r 'string=? 2 #t)
    (r 'symbol? 1)
    (r 'string? 1)
    (r 'string>? 2 #t)
    (r 'string>=? 2 #t)
    (r 'string<? 2 #t)
    (r 'string<=? 2 #t)
    (r 'string-ci<=? 2 #t)
    (r 'string-ci<? 2 #t)
    (r 'string-ci=? 2 #t)
    (r 'string-ci>=? 2 #t)
    (r 'string-ci>? 2 #t)
    (r 'substring 3 )
    (r 'string-length 1)
    (r 'string-ref 2)
    (r 'string-copy 1)
    (r 'string->number 1)
    (r 'string->list 1)
    (r 'string->symbol  1)
    (r 'string-append 1 #t)
    (r 'list->string 1)
    (r 'make-string 2)
    (r 'string 1 #t)
    
    ;; Pairs
    (r 'empty? 1)
    (r 'first 1)
    (r 'second 1)
    (r 'third 1)
    (r 'fourth 1)
    (r 'fifth 1)
    (r 'sixth 1)
    (r 'seventh 1)
    (r 'eighth 1)
    (r 'rest 1)
    (r 'cons 2)
    (r 'pair? 1)
    (r 'cons? 1)
    (r 'null? 1)
    (r 'length 1)
    (r 'list 1 #t)
    (r 'list* 1 #t)
    (r 'list-ref 2)
    (r 'append 1 #t)
    (r 'member 2)
    (r 'memq 2)
    (r 'memv 2)
    (r 'reverse 1)
    (r 'caaar 1)
    (r 'caadr 1)
    (r 'caar 1)
    (r 'cadar 1)
    (r 'cadddr 1)
    (r 'caddr 1)
    (r 'cadr 1)
    (r 'car 1)
    (r 'cdaar 1)
    (r 'cdadr 1)
    (r 'cdar 1)
    (r 'cddar 1)
    (r 'cdddr 1)
    (r 'cddr 1)
    (r 'cdr 1)
    
    ;; Posn
    (r 'make-posn 2)
    (r 'posn-x 1)
    (r 'posn-y 1)
    (r 'posn? 1)
    
    ;; Eof
    (r 'eof-object? 1)
    
    ;; Misc
    (r 'equal? 2)
    (r 'eq? 2)
    (r 'eqv? 2)
    (r 'equal~? 3)
    (r 'error 2)
    (r 'struct? 1)
    (r 'identity 1 #f)
    (r 'current-seconds 0)))
  


(define (register-world!)
  ;; The World kernel functions.
  (let ([r (lambda (id arity (vararity? #f))
             (register-toplevel-function! id 
                                          (resolve-module-path 'htdp/world #f)
                                          arity
                                          vararity?
                                          (format "org.plt.WorldKernel.~a"
                                                  (identifier->munged-java-identifier id))))])
    (r 'empty-scene 2)
    (r 'place-image 4)
    (r 'circle 3)
    (r 'nw:rectangle 4)
    (r 'rectangle 4)
    (r 'key=? 2)
    (r 'text 3)
    (r '-kernel-create-image 1)
    ;; Fixme: -kernel-create-image is a special case of a function not in the original language.
    ;; We can fix this by extending expression to include a special "magic" identifier.  We should
    ;; ensure students don't accidently hit this function.
    (r 'image-width 1)
    (r 'image-height 1)
    (r 'image? 1)
    (r 'image=? 2)
    (r 'image-rotate 2)))
  
  

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;




(register-htdp!)
(register-world!)


(provide/contract
 [get-toplevel-env (-> env?)]
 [lookup-toplevel-id (symbol? . -> . (or/c binding? false/c))]
 [register-toplevel-constant! (symbol? string? . -> . void?)]
 [register-toplevel-function! (symbol? (or/c false/c module-path?) number? boolean? string? . -> . void?)])