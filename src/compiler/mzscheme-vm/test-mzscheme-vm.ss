#lang scheme/base

(require test-engine/scheme-tests
         "mzscheme-vm.ss"
         "env.ss"
         "../pinfo.ss"
         "../../stx-helpers.ss"
         "../../../support/externals/mzscheme-vm/src/bytecode-structs.ss")

(require (for-syntax scheme/base))


(define-syntax (c stx)
  (syntax-case stx ()
    [(_ e)
     (syntax/loc stx 
       (let-values ([(bytecode pinfo)
                     (compile-expression (syntax->stx #'e)
                                         empty-env
                                         empty-pinfo)])
         bytecode))]))



(check-expect (c 42) 42)
(check-expect (c "hello") "hello")
(check-expect (c #t) #t)
(check-expect (c #f) #f)
(check-expect (c #\z) #\z)
(check-expect (c (if #t 3 4))
              (make-branch #t 3 4))
(check-expect (c (begin "hello" "world"))
              (make-seq (list "hello" "world")))


;; identifiers and lambdas
(check-error 
 (c x)
 "compile-identifier-expression: Couldn't find x in the environment")

#;(check-expect (c (lambda (x y) x))
                (make-lam ...
                 ... (make-localref 0)))



(test)