#lang scheme/base

(require test-engine/scheme-tests
         "mzscheme-vm.ss"
         "env.ss"
         "../pinfo.ss"
         "../../stx-helpers.ss"
         "../../../support/externals/mzscheme-vm/src/bytecode-structs.ss")

(require (for-syntax scheme/base))


(define-syntax (s stx)
  (syntax-case stx ()
    [(_ e)
     (syntax/loc stx 
       (syntax->stx #'e))]))

(define-syntax (c stx)
  (syntax-case stx ()
    [(_ e)
     (syntax/loc stx 
       (let-values ([(bytecode pinfo)
                     (compile-expression (s e)
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


(check-expect (c (lambda (x y) x))
              (make-lam '() 
                        '() 
                        2 
                        '(val val) 
                        #f 
                        #() 
                        '() 
                        0
                        (make-localref #f 0 #f #f #f)))


(check-expect (c (lambda (x y z) z))
              (make-lam '() 
                        '() 
                        3 
                        '(val val val) 
                        #f 
                        #() 
                        '() 
                        0
                        (make-localref #f 2 #f #f #f)))


(check-expect (c (lambda (x y) 
                   (lambda (z) x)))
              (make-lam '() 
                        '() 
                        2 
                        '(val val) 
                        #f 
                        #() 
                        '() 
                        0
                        (make-lam '() 
                                  '() 
                                  1 
                                  '(val) 
                                  #f 
                                  #(0) 
                                  '(val/ref) 
                                  0
                                  (make-localref #f 0 #f #f #f))))



(check-expect (free-variables (s x) empty-env) '(x))
(check-expect (free-variables (s (if x y z)) empty-env) '(x y z))
(check-expect (free-variables (s 42) empty-env) '())


(check-expect (c (lambda (x y) 
                   (lambda (z) z)))
              (make-lam '() 
                        '() 
                        2 
                        '(val val) 
                        #f 
                        #() 
                        '() 
                        0
                        (make-lam '() 
                                  '() 
                                  1 
                                  '(val) 
                                  #f 
                                  #() 
                                  '() 
                                  0
                                  (make-localref #f 0 #f #f #f))))



(check-expect (c (lambda (x y) 
                   (lambda (z) y)))
              (make-lam '() 
                        '() 
                        2 
                        '(val val) 
                        #f 
                        #() 
                        '() 
                        0
                        (make-lam '() 
                                  '() 
                                  1 
                                  '(val) 
                                  #f 
                                  #(1) 
                                  '(val/ref) 
                                  0
                                  (make-localref #f 0 #f #f #f))))








(test)