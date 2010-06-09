#lang scheme/base

(require test-engine/scheme-tests
         "mzscheme-vm.ss"
         "../env.ss"
         "../pinfo.ss"
         "../../stx-helpers.ss")

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



(test)