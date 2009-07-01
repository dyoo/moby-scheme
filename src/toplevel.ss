#lang s-exp "lang.ss"

(require "env.ss")
(require "helpers.ss")


;; toplevel-env: env
(define toplevel-env
  (local [(define top-env-1
            (foldl (lambda (id+name env)
                     (env-extend-constant env (first id+name) (second id+name)))
                   empty-env
                   '((null "plt.types.Empty.EMPTY")
                     (empty "plt.types.Empty.EMPTY")
                     (true "plt.types.Logic.TRUE")
                     (false "plt.types.Logic.FALSE")
                     (eof "plt.types.EofObject.EOF")
                     (pi "plt.Kernel.pi")
                     (e "plt.Kernel.e"))))
    
          ;; Registers a new toplevel function, munging the name
          (define (r env a-name arity vararity?)
            (env-extend-function env
                                 a-name 
                                 (resolve-module-path 'lang/htdp-beginner false)
                                 arity 
                                 vararity?
                                 (string-append 
                                  "plt.Kernel."
                                  (symbol->string (identifier->munged-java-identifier 
                                   a-name)))))

          
          ;; A special registration function that doesn't munge the kernel function name.
          (define (r* env a-name arity java-string)
            (env-extend-function env
                                 a-name 
                                 (resolve-module-path 'lang/htdp-beginner false)
                                 arity 
                                 false
                                 java-string))
          
          (define top-env-2
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
                   
                   top-env-1
                   
                   ;; Numerics
                   '((+ 0 true)
                     (- 1 true)
                     (* 0 true)
                     (/ 1 true)
                     (>= 2 true)
                     (> 2 true)
                     (<= 2 true)
                     (< 2 true)
                     (= 2 true)
                     (=~ 3)
                     (number->string 1)
                     (even? 1)
                     (odd? 1)
                     (positive? 1)
                     (negative? 1)
                     (number? 1)
                     (rational? 1)
                     (quotient 1)
                     (remainder 1)
                     (numerator 1)
                     (denominator 1)
                     (integer? 1)
                     (real? 1)
                     (abs 1)
                     (acos 1)
                     (asin 1)
                     (atan 1)
                     (random 1)
                     (max 1 true)
                     (min 1 true)
                     (sqr 1)
                     (sqrt 1)
                     (modulo 2)
                     (add1 1)
                     (sub1 1)
                     (zero? 1)
                     (exp 1)
                     (expt 2)
                     (sgn 1)
                     (log 1)
                     (gcd 2 true)
                     (lcm 2 true)
                     (round 1)
                     (floor 1)
                     (ceiling 1)
                     (sin 1)
                     (cos 1)
                     (tan 1)
                     (sinh 1)
                     (cosh 1)
                     (angle 1)
                     (conjugate 1)
                     (magnitude 1)
                     
                     (exact->inexact 1)
                     (inexact->exact 1)
                     (inexact? 1)
                     (complex? 1)
                     (real-part 1)
                     (imag-part 1)
                     
                     ;; Logic
                     (not 1)
                     (false? 1)
                     (boolean? 1)
                     (boolean=? 2)
                     
                     ;; Characters
                     (char? 1)
                     (char=? 2 true)
                     (char<? 2 true)
                     (char<=? 2 true)
                     (char>? 2 true)
                     (char>=? 2 true)
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
                     (char-alphabetic? 1)
                     (char->integer 1)
                     (integer->char 1)
                     
                     ;; Symbols
                     (symbol=? 2)
                     (symbol->string 1)
                     
                     ;; Strings
                     (string=? 2 true)
                     (symbol? 1)
                     (string? 1)
                     (string>? 2 true)
                     (string>=? 2 true)
                     (string<? 2 true)
                     (string<=? 2 true)
                     (string-ci<=? 2 true)
                     (string-ci<? 2 true)
                     (string-ci=? 2 true)
                     (string-ci>=? 2 true)
                     (string-ci>? 2 true)
                     (substring 3 )
                     (string-length 1)
                     (string-ref 2)
                     (string-copy 1)
                     (string->number 1)
                     (string->list 1)
                     (string->symbol  1)
                     (string-append 0 true)
                     (list->string 1)
                     (make-string 2)
                     (string 0 true)
                     
                     ;; Pairs
                     (empty? 1)
                     (first 1)
                     (second 1)
                     (third 1)
                     (fourth 1)
                     (fifth 1)
                     (sixth 1)
                     (seventh 1)
                     (eighth 1)
                     (rest 1)
                     (cons 2)
                     (pair? 1)
                     (cons? 1)
                     (null? 1)
                     (length 1)
                     (list 0 true)
                     (list* 1 true)
                     (list-ref 2)
                     (append 1 true)
                     (member 2)
                     (memq 2)
                     (memv 2)
                     (reverse 1)
                     (caaar 1)
                     (caadr 1)
                     (caar 1)
                     (cadar 1)
                     (cadddr 1)
                     (caddr 1)
                     (cadr 1)
                     (car 1)
                     (cdaar 1)
                     (cdadr 1)
                     (cdar 1)
                     (cddar 1)
                     (cdddr 1)
                     (cddr 1)
                     (cdr 1)
                     
                     ;; Posn
                     (make-posn 2)
                     (posn-x 1)
                     (posn-y 1)
                     (posn? 1)
                     
                     ;; Eof
                     (eof-object? 1)
                     
                     ;; Misc
                     (equal? 2)
                     (eq? 2)
                     (eqv? 2)
                     (equal~? 3)
                     (error 2)
                     (struct? 1)
                     (identity 1)
                     (current-seconds 0)
                     
                     (andmap 2)
                     (foldl 2 true)
                     (build-list 2)
                     (map 1 true)
                     (format 1 true)
                     )))
          
          
          ;; These are identifiers we'll need to do the bootstrapping.
          (define top-env-3 
            (foldl (lambda (id+arity+name env)
                     (r* env (first id+arity+name) (second id+arity+name) (third id+arity+name)))
                   top-env-2
                   '((hash-set 3 "plt.Kernel._kernelHashSet")
                     (hash-ref 3 "plt.Kernel._kernelHashRef")
                     (make-immutable-hasheq 1 "plt.Kernel._kernelMakeImmutableHashEq")
                     (hash-map 2 "plt.Kernel._kernelHashMap")
                     (hash? 1 "plt.Kernel._isHash")
                     (path->string 1 "plt.Kernel._pathToString")
                     (normalize-path 1 "plt.Kernel._normalizePath")
                     (resolve-module-path 2 "plt.Kernel._resolveModulePath")
                     (build-path 2 "plt.Kernel._buildPath")

                     )))]
    top-env-3))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(provide/contract
 [toplevel-env env?])