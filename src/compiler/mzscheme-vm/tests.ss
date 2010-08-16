#lang scheme/base

(require "../../serve.ss"
         "../../stx-helpers.ss")

(require (for-syntax scheme/base))


;; I should be able to use the compiler on these expressions and test the result of evaluating
;; them.

(define test-program/rev '())


;; compile-and-check: expr expected -> void
(define (run-tests)
  (compile-and-serve
   (reverse test-program/rev)
   "test"))

(define (add-test* expr expected)
  (with-syntax ([e1 expr]
                [e2 expected])
    (set! test-program/rev (cons (syntax->stx #'(check-expect e1 e2))
                                 test-program/rev))))

(define-syntax (add-test stx)
  (syntax-case stx ()
    [(_ e1 e2)
     #'(add-test* #'e1 #'e2)]))


(add-test (let ([x ((lambda ()
                      (case-lambda 
                        [(x) (list x)]
                        [(x y) (list y x)]
                        [(x y z) (list z x y)])))])
            (list (x 3)
                  (x 3 4)
                  (x 1 2 3)))
          
          '((3) (4 3) (3 1 2)))


(add-test (let ([x (case-lambda 
                     [(x) (list x)]
                     [(x y) (list y x)]
                     [(x y z) (list z x y)])])
            (list (x 3)
                  (x 3 4)
                  (x 1 2 3)))
          
          '((3) (4 3) (3 1 2)))


(add-test (let ([x (case-lambda 
                     [() (list 'empty-accepted)])])
            (x))
          
          '(empty-accepted))



(run-tests)