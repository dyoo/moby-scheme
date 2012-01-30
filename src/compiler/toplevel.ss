#lang s-exp "lang.ss"

(require "env.ss")
(require "helpers.ss")


;; get-toplevel-env: symbol ->env
(define (get-toplevel-env lang)
  ;; fixme: use the language to limit what symbols get in the toplevel.
  (local [(define base-constants-env
            (foldl (lambda (id env)
                     (env-extend-constant env id "moby/toplevel"))
                   empty-env
                   '(null
                     empty
                     true
                     false
                     eof
                     pi
                     e
                     ;effect:do-nothing
		     js-undefined
		     js-null)))
          
          ;; Registers a new toplevel function, munging the name
          (define (r env a-name arity vararity?)
            (env-extend-function env
                                 a-name 
                                 "moby/toplevel"
                                 arity 
                                 vararity?))
          
          ;; The core environment includes bindings to Javascript-written functions.
          (define core-env
            (foldl (lambda (name+arity env)
                     (cond
                       [(= (length name+arity) 2)
                        (r env 
                           (first name+arity)
                           (second name+arity)
                           false)]
                       [(= (length name+arity) 3)
                        (r env
                           (first name+arity)
                           (second name+arity)
                           (if (symbol=? (third name+arity) 'true) true false))]))
                   
                   base-constants-env
                   
                   ;; Numerics
                   '((< 2 true)
                     (<= 2 true)
                     (= 2 true)
                     (> 2 true)
                     (>= 2 true)
                     
                     (=~ 3)
                     (number->string 1)
                     (even? 1)
                     (odd? 1)
                     (positive? 1)
                     (negative? 1)
                     (number? 1)
                     (rational? 1)
                     (quotient 2)
                     (remainder 2)
                     (numerator 1)
                     (denominator 1)
                     (integer? 1)
                     (real? 1)
                     (abs 1)
                     (acos 1)
                     (add1 1)
                     (angle 1)
                     (asin 1)
                     (atan 1 true)           ;; arity is either 1 or 2
                     (ceiling 1)
                     (complex? 1)
                     (conjugate 1)
                     (cos 1)
                     (cosh 1)
                     (denominator 1)
                     (even? 1)
                     (exact->inexact 1)
                     (exact? 1)               ;; *
                     (exp 1)
                     (expt 2)
                     (floor 1)
                     (gcd 1 true)
                     (imag-part 1)
                     (inexact->exact 1)
                     (inexact? 1)
                     (integer->char 1)
                     (integer-sqrt 1)         ;; *
                     (integer? 1)
                     (lcm 1 true)
                     (log 1)
                     (magnitude 1)
                     (make-polar 2)           ;; *
                     (make-rectangular 2)     ;; *
                     (max 1 true)
                     (min 1 true)
                     (modulo 2)
                     (negative? 1)
                     (number? 1)
                     (numerator 1)
                     (odd? 1)
                     (positive? 1)
                     (random 1)
                     (rational? 1)
                     (real-part 1)
                     (real? 1)
                     (round 1)
                     (sgn 1)
                     (sin 1)
                     (sinh 1)
                     (sqr 1)
                     (sqrt 1)
                     (sub1 1)
                     (tan 1)
                     (zero? 1)
                     
                     (+ 0 true)
                     (- 1 true)
                     (* 0 true)
                     (/ 1 true)
                     
                     ;; Logic
                     (not 1)
                     (false? 1)
                     (boolean? 1)
                     (boolean=? 2)
                     
                     ;; Symbols
                     (symbol->string 1)
                     (symbol=? 2)
                     (symbol? 1)
                     
                     ;; Lists
                     (append 0 true)
                     (assq 2)                 ;; *
                     (assv 2)                 ;; *
                     (assoc 2)                 ;; *
                     (caaar 1)
                     (caadr 1)
                     (caar 1)
                     (cadar 1)
                     (cadddr 1)
                     (caddr 1)
                     (cadr 1)
                     (car 1)
                     (cddar 1)
                     (cdddr 1)
                     (cddr 1)
                     (cdr 1)
                     (cdaar 1)
                     (cdadr 1)
                     (cdar 1)
                     (cons? 1)
                     (list? 1)
                     (cons 2)
                     (empty? 1)
                     (length 1)
                     (list 0 true)
                     (list* 1 true)
                     (list-ref 2)
                     (remove 2)
                     (member 2)
                     (member? 2)
                     (memq 2)
                     (memv 2)
                     (null? 1)
                     (pair? 1)
                     (rest 1)
                     (reverse 1)
                     (first 1)
                     (second 1)
                     (third 1)
                     (fourth 1)
                     (fifth 1)
                     (sixth 1)
                     (seventh 1)
                     (eighth 1)
                     
                     ;; We're commenting out the mutation operation on pairs
                     ;; because they're not supported in ISL/ASL anymore.
                     #;(set-car! 2)
                     #;(set-cdr! 2)
                     
                     ;; Box
                     (box 1)
                     (unbox 1)
                     (set-box! 2)
                     (box? 1)
                     
                     ;; Posn
                     (make-posn 2)
                     (posn-x 1)
                     (posn-y 1)
                     (posn? 1)
                     
                     ;; Characters
                     (char->integer 1)
                     (char-alphabetic? 1)
                     (char-ci<=? 2 true)
                     (char-ci<? 2 true)
                     (char-ci=? 2 true)
                     (char-ci>=? 2 true)
                     (char-ci>? 2 true)
                     (char-downcase 1)
                     (char-lower-case? 1)
                     (char-numeric? 1)
                     (char-upcase 1)
                     (char-upper-case? 1)
                     (char-whitespace? 1)
                     (char<=? 2 true)
                     (char<? 2 true)
                     (char=? 2 true)
                     (char>=? 2 true)
                     (char>? 2 true)
                     (char? 1)
                     
                     ;; Strings
                     (format 1 true)
                     (list->string 1)
                     (make-string 2)
                     (replicate 2)
                     (string 0 true)
                     (string->list 1)
                     (string->number 1)
                     (string->symbol 1)
                     (string-alphabetic? 1)
                     (string-append 0 true)
                     (string-ci<=? 2 true)
                     (string-ci<? 2 true)
                     (string-ci=? 2 true)
                     (string-ci>=? 2 true)
                     (string-ci>? 2 true)
                     (string-copy 1)
                     (string-length 1)
                     (string-lower-case? 1)   ;; *
                     (string-numeric? 1)      ;; *
                     (string-ref 2)
                     (string-upper-case? 1)   ;; *
                     (string-whitespace? 1)   ;; *
                     (string<=? 2 true)
                     (string<? 2 true)
                     (string=? 2 true)
                     (string>=? 2 true)
                     (string>? 2 true)
                     (string? 1)
                     (substring 3 )
                     (string-ith 2)
                     (int->string 1)
                     (string->int 1)
                     (explode 1)
                     (implode 1)
                     
                     ;; Eof
                     (eof-object? 1)
                     
                     ;; Misc
                     (=~ 3)
                     (eq? 2)
                     (equal? 2)
                     (equal~? 3)
                     (eqv? 2)
                     (error 2)

                     (identity 1)
                     (struct? 1)
                     (current-seconds 0)
                     
                     ;; Higher-Order Functions
                     (andmap 1 true)
                     (apply 2 true)           ;; *
                     (argmax 2)               ;; *
                     (argmin 2)               ;; *
                     (build-list 2)
                     (build-string 2)         ;; *
                     (compose 0 true)         ;; *
                     (filter 2)               ;; *
                     (foldl 2 true)
                     (foldr 2 true)                ;; *
                     (map 1 true)
		     (for-each 1 true)
                     (memf 2)                 ;; *
                     (ormap 1 true)                ;; *
                     (procedure? 1)           ;; *
                     (quicksort 2)            ;; *
                     (sort 2)                 ;; *
                     
                                         
                     (void 0 true)
                     
                     ;; Parsing
                     (xml->s-exp 1)
                     
                     ;; Vectors
                     
                     (build-vector 2)
                     
                     ;; FIXME: should only take one or two arguments, not vararity
                     (make-vector 1 true)

                     (vector 0 true)
                     (vector-length 1)
                     (vector-ref 2)
                     (vector-set! 3)
                     (vector->list 1)
                     (list->vector 1)
                     (vector? 1)

		     (printf 1 true)
                     (display 1)                     
                     (write 1)
		     (newline 0)

		     (call/cc 1)

                     (procedure-arity 1)
                     
                     
                     ;; Testing functions.
                     ;; NOTE: the desugar.ss module converts use of check-expect into ones that
                     ;; thunk its arguments, and pass an additional location argument.
                     (check-expect 2)
                     (EXAMPLE 2)  ;; temporary hack: for emmanuel, since I don't yet have
                                  ;; extensible macros
                     (check-within 3)
                     (check-error 2)

                     (make-hasheq 0)
                     (make-hash 0)
                     (hash-set! 3 )
                     (hash-ref 3)
                     (hash-remove! 2)
                     (hash-map 2)
                     (hash-for-each 2)
                     (hash? 1)
                     
                     
                     ;; Exception raising
                     (raise 1)
                     

                     ;; Checking for undefined
                     (undefined? 1)
                     

		     ;; values for multiple value definition
		     (values 0 true)

                     
                     ;; structures
                     (make-struct-type 4 true)
                     (make-struct-field-accessor 2 true)
                     (make-struct-field-mutator 2 true)
                     
                     ;; continuation mark stuff
                     ;; FIXME: add support for prompt optional argument
                     (current-continuation-marks 0 false)
                     (continuation-mark-set->list 2 false)
                     
                     ;; Things for javascript FFI and world
                     (scheme->prim-js 1 false)
                     (prim-js->scheme 1 false)
                     (procedure->cps-js-fun 1 false)
                     (procedure->void-js-fun 1 false)
                     (js-=== 2 false)
                     (js-get-named-object 1 false)
                     (js-get-field 2 true)
                     ;(get-js-array-field 2 false)
                     (js-set-field! 3 false)
                     ;(js-set-array-field! 3 false)
                     (js-typeof 1 false)
                     (js-instanceof 2 false)
                     (js-call 2 true)
                     (js-new 1 true)
                     (js-make-hash 0 true)
                     
                     (make-world-config 2 true)
                     (make-bb-info 2 false)
                     (bb-info? 1 false)
                     (bb-info-change-world 1 false)
                     (bb-info-toplevel-node 1 false)
                     
                     (make-effect-type 4 true)
                     (effect? 1 false)
                     (world-with-effects 2 false)
                     ;(coerce-world-handler 1 false)
		     (make-render-effect-type 4 true)
		     (render-effect-type? 1)
		     (render-effect? 1)
                     
                     ;(make-effect:do-nothing 0 false)
                     ;(effect:do-nothing? 1 false)
                     
                     (make-render-effect-type 4 true)
                     ;(render-effect-name 1 false)
                     ;(render-effect-dom-node 1 false)
                     ;(render-effect-effects 1 false)
                     ;(render-effect? 1 false)
                     
                     (values 0 true)
                     (sleep 0 true)
                     (current-inexact-milliseconds 0 false)
                     
                     (make-exn 2 false)
                     (exn-message 1 false)
                     (exn-continuation-marks 1 false)
                     
                     
                     
                     )))]
    core-env))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(provide/contract
 [get-toplevel-env (symbol? . -> .  env?)])
