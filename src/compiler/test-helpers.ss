#lang scheme/base

(require test-engine/scheme-tests
         scheme/bool
         (only-in scheme/list first rest)
         "helpers.ss"
         "desugar.ss"
         "../collects/moby/runtime/stx.ss"
         "../stx-helpers.ss"
         "pinfo.ss")

(require (for-syntax "../stx-helpers.ss")
         (for-syntax scheme/base))

;; e<: syntax
;; Calls expression<?, handling the boilerplate for converting from regular scheme syntax
;; objects to the ones that Moby uses.
(define-syntax (e< stx)
  (syntax-case stx ()
    [(_ x y)
     (with-syntax  ([e1 (syntax->stx #'x)]
                    [e2 (syntax->stx #'y)])
       (syntax/loc stx
         (expression<? e1 e2)))]))
     
     

(check-expect (e< 3 4) true)
(check-expect (e< 4 3) false)
(check-expect (e< "3" 4) false)
(check-expect (e< 4 "3") true)
(check-expect (e< +inf.0 17) false)
(check-expect (e< 17 +inf.0) true)
(check-expect (e< +inf.0 +inf.0) false)
(check-expect (e< +inf.0 -inf.0) false)
(check-expect (e< -inf.0 +inf.0) true)
(check-expect (e< -inf.0 -inf.0) false)







#;(stx->datum (first (first (desugar-program (list (syntax->stx #'(define (f x) 
                                                                    (local [(define g 16)
                                                                            (define-struct k (z b))]
                                                                      (* x x)))))
                                             (get-base-pinfo 'moby)))))
                 






(test)