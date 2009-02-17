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


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Now let's register the htdp primitive functions

(let ([r (lambda (id arity (vararity? #f))
           (register-toplevel-function! id 
                                        (resolve-module-path 'lang/htdp-beginner)
                                        arity
                                        vararity?
                                        (format "org.plt.Kernel.~a"
                                                (identifier->munged-java-identifier id))))])
  
  (r 'identity 1 #f)
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
  (r '=~ 2 #t)
  
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
  (r 'equal? 2)
  (r 'eq? 2)
  (r 'eqv? 2)
  (r 'equal~? 3)
  
  ;; Characters
  'char?
  'char=?
  'char<?
  'char<=?
  'char>?
  'char>=?
  'char-downcase
  'char-lower-case?
  'char-numeric?
  'char-upcase
  'char-upper-case?
  'char-whitespace?
  'char-alphabetic?
  'char-ci<=?
  'char-ci<?
  'char-ci=?
  'char-ci>=?
  'char-ci>?
  'char->integer
  'integer->char
  
  ;; Symbols
  'symbol=?
  'symbol->string
  ;; Strings
  'string=?
  'symbol?
  'string?
  'string>?
  'string>=?
  'string<?
  'string<=?
  'substring
  'string-length
  'string-ref
  'string-copy
  'string->number
  'string-ci<=?
  'string-ci<?
  'string-ci=?
  'string-ci>=?
  'string-ci>?
  'string->list
  'string->symbol 
  'string-append 
  'list->string 
  'make-string 
  'string 
  
  ;; Pairs
  'empty?
  'first
  'second
  'third
  'fourth
  'fifth
  'sixth
  'seventh
  'eighth
  'rest
  'cons
  'pair?
  'cons?
  'null?
  'length
  'list
  'list*
  'empty
  'null
  'list-ref
  'append
  'member
  'memq
  'memv
  
  'reverse
  
  'caaar
  'caadr
  'caar
  'cadar
  'cadddr
  'caddr
  'cadr
  'car
  'cdaar
  'cdadr
  'cdar
  'cddar
  'cdddr
  'cddr
  'cdr
  
  'struct?
  ;; Posn
  'make-posn
  'posn-x
  'posn-y
  'posn?
  
  ;; Eof
  'eof
  'eof-object?
  
  ;; Misc
  'error
  'current-seconds)






(define WORLD-PRIMITIVE-SYMBOLS
  '(empty-scene
    place-image
    circle
    nw:rectangle
    rectangle
    key=?
    text
    -kernel-create-image     
    ;; Fixme: -kernel-create-image is a special case of a function not in the original language.
    ;; We can fix this by extending expression to include a special "magic" identifier.  We should
    ;; ensure students don't accidently hit this function.
    image-width
    image-height
    image?))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;





(provide/contract
 [get-toplevel-env (-> env?)]
 [lookup-toplevel-id (symbol? . -> . (or/c binding? false/c))]
 [register-toplevel-constant! (symbol? string? . -> . void?)]
 [register-toplevel-function! (symbol? (or/c false/c module-path?) number? boolean? string? . -> . void?)])