#lang s-exp "private/restricted-runtime-scheme.ss"


(define-struct moby-error (reason location error-type))


(define-struct moby-error-type:unclosed-lexical-token ())
(define-struct moby-error-type:unrecognized-lexical-token ())
(define-struct moby-error-type:unsupported-lexical-token ())
(define-struct moby-error-type:unclosed-parentheses ())
(define-struct moby-error-type:missing-expression ())
(define-struct moby-error-type:duplicate-identifier (id other-location))
(define-struct moby-error-type:undefined-identifier ())
(define-struct moby-error-type:application-arity (expected observed))
(define-struct moby-error-type:type-mismatch (who position expected observed))
(define-struct moby-error-type:index-out-of-bounds (minimum maximum observed))
(define-struct moby-error-type:conditional-exhausted ())
(define-struct moby-error-type:generic-runtime-error ())
(define-struct moby-error-type:generic-syntactic-error (other-locations))



;; moby-error-type: any -> boolean
;; Produces true if x is a moby-error-type.
(define (moby-error-type? x)
  (or (moby-error-type:unclosed-lexical-token? x)
      (moby-error-type:unrecognized-lexical-token? x)
      (moby-error-type:unsupported-lexical-token? x)
      (moby-error-type:unclosed-parentheses? x)
      (moby-error-type:missing-expression? x)
      (moby-error-type:duplicate-identifier? x)
      (moby-error-type:undefined-identifier? x)
      (moby-error-type:application-arity? x)
      (moby-error-type:type-mismatch? x)
      (moby-error-type:index-out-of-bounds? x)
      (moby-error-type:conditional-exhausted? x)
      (moby-error-type:generic-runtime-error? x)
      (moby-error-type:generic-syntactic-error? x)))




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-struct moby-expected:string ())
(define-struct moby-expected:integer ())
(define-struct moby-expected:natural ())
(define-struct moby-expected:rational ())
(define-struct moby-expected:real ())
(define-struct moby-expected:complex ())
(define-struct moby-expected:number ())
(define-struct moby-expected:boolean ())
(define-struct moby-expected:char ())
(define-struct moby-expected:symbol ())
(define-struct moby-expected:list ())
(define-struct moby-expected:vector ())
(define-struct moby-expected:struct ())
(define-struct moby-expected:box ())
(define-struct moby-expected:hash ())
(define-struct moby-expected:function ())
(define-struct moby-expected:something ())


;; moby-expected?: any -> boolean
;; Produces true if x is an expected value.
(define (moby-expected? x)
  (ormap (lambda (pred?)
           (pred? x))
         (list 
          moby-expected:string?
          moby-expected:integer?
          moby-expected:natural?
          moby-expected:rational?
          moby-expected:real?
          moby-expected:complex?
          moby-expected:number?
          moby-expected:boolean?
          moby-expected:char?
          moby-expected:symbol?
          moby-expected:list?
          moby-expected:vector?
          moby-expected:struct?
          moby-expected:box?
          moby-expected:hash?
          moby-expected:function?
          moby-expected:something?)))
      




(provide/contract
 [struct moby-error ([reason string?]
                     [location any/c]
                     [error-type moby-error-type?])]

 [moby-error-type? (any/c . -> . boolean?)]
 [struct moby-error-type:unclosed-lexical-token ()]
 [struct moby-error-type:unrecognized-lexical-token ()]
 [struct moby-error-type:unsupported-lexical-token ()]
 [struct moby-error-type:unclosed-parentheses ()]
 [struct moby-error-type:missing-expression ()]
 [struct moby-error-type:duplicate-identifier ([id symbol?]
                                               [other-location any/c])]
 [struct moby-error-type:undefined-identifier ()]
 [struct moby-error-type:application-arity ([expected any/c]
                                            [observed any/c])]
 [struct moby-error-type:type-mismatch ([who any/c]
                                        [position number?]
                                        [expected any/c]
                                        [observed any/c])]
 [struct moby-error-type:index-out-of-bounds ([minimum number?]
                                              [maximum number?]
                                              [observed number?])]
 [struct moby-error-type:conditional-exhausted ()]
 [struct moby-error-type:generic-runtime-error ()]
 [struct moby-error-type:generic-syntactic-error ([other-locations (listof any/c)])]
 
 
 [moby-expected? (any/c . -> . boolean?)]
 [struct moby-expected:string ()]
 [struct moby-expected:integer ()]
 [struct moby-expected:natural ()]
 [struct moby-expected:rational ()]
 [struct moby-expected:real ()]
 [struct moby-expected:complex ()]
 [struct moby-expected:number ()]
 [struct moby-expected:boolean ()]
 [struct moby-expected:char ()]
 [struct moby-expected:symbol ()]
 [struct moby-expected:list ()]
 [struct moby-expected:vector ()]
 [struct moby-expected:struct ()]
 [struct moby-expected:box ()]
 [struct moby-expected:hash ()]
 [struct moby-expected:function ()]
 [struct moby-expected:something ()])