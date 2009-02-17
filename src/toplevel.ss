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
;; Now let's register the htdp primitives.

(let ([r! (lambda (id arity vararity?)
            (register-toplevel-function! id 
                                         (resolve-module-path 'lang/htdp-beginner)
                                         arity
                                         vararity?
                                         (format "org.plt.Kernel.~a"
                                                 (identifier->munged-java-identifier id))))])
  
  (r! 'identity 1 #f)
  ;; Numerics
  (r! '+ 0 #t)
  (r! '- 1 #t)
  (r! '* 0 #t)
  (r! '/ 1 #t)
  (r! '>= 2 #t)
  (r! '> 2 #t)
  (r! '<= 2 #t)
  (r! '< 2 #t)
  (r! '= 2 #t)
  (r! '=~ 2 #t)
  
  'number->string
  'even?
  'odd?
  'positive?
  'negative?
  'number?
  'rational?
  'quotient
  'remainder
  'numerator
  'denominator
  'integer?
  'real?
  
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
  current-seconds)

  




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