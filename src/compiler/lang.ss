#lang scheme/base

;; The restricted language we're writing our compiler in.
;; Part of the self-hosting.  The Javascript compiler will be written
;; in a small superset of intermediate-level scheme, so we can then
;; bootstrap things.

(require (prefix-in base: scheme/base)
         (prefix-in contract: scheme/contract)
         lang/htdp-advanced
         (for-syntax scheme/base)
         scheme/class
         scheme/gui/base
         "../collects/moby/runtime/stx.ss"
         "../collects/moby/runtime/error-struct.ss"
         "../collects/moby/runtime/error-struct-to-dom.ss"
         "../collects/moby/runtime/dom-helpers.ss")





(define-syntax (my-define-struct stx)
  (syntax-case stx ()
    [(_ id (fields ...))
     (syntax/loc stx 
       (base:define-struct id (fields ...)
                           #:prefab 
                           #:mutable))]))

(base:define-struct (exn:fail:moby-syntax-error exn:fail) (stxs))


#;(base:define (syntax-error msg . stx)
               (raise (make-exn:fail:moby-syntax-error 
                       (format "~a: ~s" msg (map stx->datum stx))
                       (current-continuation-marks)
                       stx)))








;; BIG HACK.
;; open-input-stx: string -> (listof stx)
(define (open-input-stx a-path-string)
  (local [;; open-beginner-program: path-string -> text%
          ;; Opens up the beginner-level program.
          (define (open-beginner-program path)
            (local [(define text (new text%))]
              (begin (send text insert-file path)
                     text)))
          
          ;; syntax->stx: syntax -> stx
          ;; Go from Scheme's syntax objects to our own.
          (define (syntax->stx a-syntax)
            (cond
              [(pair? (syntax-e a-syntax))
               (let ([elts
                      (map syntax->stx (syntax->list a-syntax))])
                 (datum->stx elts
                             (make-Loc (syntax-position a-syntax)
                                       (syntax-line a-syntax)
                                       (syntax-span a-syntax)
                                       (format "~a" (syntax-source a-syntax)))))]
              [else
               (datum->stx (syntax-e a-syntax)
                           (make-Loc (syntax-position a-syntax)
                                     (syntax-line a-syntax)
                                     (syntax-span a-syntax)
                                     (format "~a" (syntax-source a-syntax))))]))
          
          ; parse-text-as-program: text -> program
          ;; Given a text, returns a program as well.
          (define (parse-text-as-program a-text source-name)
            (let* ([ip (open-input-text-editor a-text)])
              (begin
              (port-count-lines! ip)
              (parameterize ([read-accept-reader #t]
                             [read-decimal-as-inexact #f])
                (let loop ()
                  (let ([stx (read-syntax source-name ip)])
                    (cond [(not (eof-object? stx))
                           (cons (syntax->stx stx) (loop))]
                          [else
                           empty])))))))]
    (parse-text-as-program (open-beginner-program a-path-string)
                           a-path-string)))


(base:define-struct (moby-failure exn:fail) (val))




(define (Loc->string a-loc)
  (format "Location: line ~a, column ~a, span ~a, offset ~a, id ~s" 
          (Loc-line a-loc)
          (Loc-column a-loc)
          (Loc-span a-loc)
          (Loc-offset a-loc)
          (Loc-id a-loc)))


(define-syntax (my-raise stx)
  (syntax-case stx ()
    [(_ val)
     (syntax/loc stx
       (base:let ([msg 
                   (if (moby-error? val)
                       (base:with-handlers 
                        ([void
                          (lambda (exn)
                            (format "Bad thing happened: ~s~n" exn))])
                        (string-append (dom-string-content (error-struct->dom-sexp val false))
                                       "\n"
                                       (Loc->string (moby-error-location val))))
                       (base:format "not a moby error: ~s" val))])
                 (base:raise (make-moby-failure (base:format "~a" 
                                                             msg)
                                                (base:current-continuation-marks) 
                                                val))))]))



(define (my-hash-ref a-hash key default-val)
  (base:hash-ref a-hash key default-val))



;; Bindings from advanced student that we want to expose:
(provide 
 ;; special forms:
 #%app
 #%module-begin
 #%datum
 require
 cond else
 if
 lambda
 begin
 begin0
 case
 when
 unless     
 local
 let
 let*
 letrec
 and
 or
 
 
 ;; primitives        
 <
 <=
 =
 >
 >=
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
 integer-sqrt
 integer?
 lcm
 log
 magnitude
 make-polar
 make-rectangular
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
 true
 false
 false?
 ;; not
 symbol->string
 symbol=?
 symbol?
 append
 ;; assoc
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
 empty
 empty?
 fifth
 first
 fourth
 length
 list
 list*
 list-ref
 list?
 ;; make-list
 member
 memq
 memv
 null
 null?
 pair?
 remove
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
 ;;         set-posn-x!
 ;;         set-posn-y!
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
 explode
 format
 implode
 int->string
 list->string
 make-string
 replicate
 string
 ;; string->int
 string->list
 string->number
 string->symbol
 string-alphabetic?
 string-append
 string-ci<=?
 string-ci<?
 string-ci=?
 string-ci>=?
 string-ci>?
 string-copy
 string-ith
 string-length
 string-lower-case?
 string-numeric?
 string-ref
 string-upper-case?
 string-whitespace?
 string<=?
 string<?
 string=?
 string>=?
 string>?
 string?
 substring
 ;; image=?
 ;; image?
 =~
 ;; current-milliseconds
 eof
 eof-object?
 eq?
 equal?
 equal~?
 eqv?
 ;; error
 ;; exit
 ;; force
 ;; gensym
 identity
 ;; promise?
 ;; sleep
 struct?
 void
 ;; void?
 *
 +
 -
 /
 ;; andmap
 apply
 argmax
 argmin
 build-list
 build-string
 compose
 filter
 ;; foldl
 ;; foldr
 for-each
 map
 memf
 ;; ormap
 procedure?
 quicksort
 sort
 ;; display
 ;; newline
 ;; pretty-print
 ;; print
 printf
 ;; read
 ;; with-input-from-file
 ;; with-input-from-string
 ;; with-output-to-file
 ;; with-output-to-string
 ;; write
 build-vector
 make-vector
 vector
 vector-length
 vector-ref
 vector-set!
 vector?
 box
 box?
 set-box!
 unbox
 )



;; The following primitives will need support in the runtime,
;; or be handled specially by the preprocessor.
(provide (rename-out (base:provide provide)
                     (base:quote quote)
                     (base:quasiquote quasiquote)
                     (base:unquote unquote)
                     (base:unquote-splicing unquote-splicing)
                     (my-define-struct define-struct)
                     (base:define define)
                     (base:set! set!)
                     (base:not not)
                     (base:procedure-arity procedure-arity)
                     (base:andmap andmap)
                     (base:ormap ormap)
                     (base:foldl foldl)
                     (base:foldr foldr)
                     (base:map map)
                     (base:for-each for-each)
                     (base:error error)
                     (my-raise raise)

                     ;; The rest of these primitives will be implemented for the kernel.
                     ;; Hash stuff
                     ;; FIXME: the hash in javascript only accepts strings as keys.
                     ;; We should use contracts here.
                     ;; disabled: using internal rbtree in the compiler now.
                     ;;hash-set hash-ref hash-remove make-immutable-hasheq hash-map
                     (base:make-hash make-hash)
                     (base:make-hasheq make-hasheq)
                     (base:hash? hash?)
                     (base:hash-set! hash-set!)
                     (base:hash-remove! hash-remove!)
                     (base:hash-map hash-map)
                     (base:hash-for-each hash-for-each)

                     (my-hash-ref hash-ref)

                     
                     
         
                     ;; Contract-related stuff: the following will be erased on 
                     ;; javascript bootstrapping time.
                     (contract:list/c list/c)
                     (contract:or/c or/c)
                     (contract:false/c false/c)
                     (contract:natural-number/c natural-number/c)
                     (contract:provide/contract provide/contract)
                     (contract:any/c any/c)
                     (contract:listof listof)
                     (contract:-> ->)
                     )


         
         ;; To support include
         open-input-stx
     

         ;path->string normalize-path path? resolve-module-path build-path
         )