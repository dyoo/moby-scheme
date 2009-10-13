#lang moby
;; This is a set of basic tests.  A lot of this is copy-and-pasted from PLT-Scheme's
;; test suite.


(define number-of-tests 0)

(define (test expect fun args)
  (set! number-of-tests (add1 number-of-tests))
  (printf "~s ==> " (cons fun args))
  (let ([res (if (procedure? fun)
		 (apply fun args)
		 (car args))])
    (let ([ok? (equal? expect res)])
      (unless ok?
	      (error (format "~s" (list res expect (cons fun args)))))
      ok?)))





;; test that all symbol characters are supported.
'(+ - ... !.. $.+ %.- &.! *.: /:. :+. <-. =. >. ?. ~. _. ^.)


(define disjoint-type-functions
  (list boolean? char? null? number? pair? procedure? string? symbol? vector?))


(test #f not (list #t))
(test #f not (list 3))
(test #f not (list (list 3)))
(test #t not (list #f))
(test #f not (list '()))
(test #f not (list (list)))
(test #f not (list 'nil))


(test #t boolean? (list #f))
(test #t boolean? (list #t))
(test #f boolean? (list 0))
(test #f boolean? (list '()))


(test #t eqv? (list 'a 'a))
(test #f eqv? (list 'a 'b))
(test #t eqv? (list 2 2))
(test #f eqv? (list 2 2.0))
(test #t eqv? (list '() '()))
(test #t eqv? (list '10000 '10000))
(test #t eqv? (list 10000000000000000000 10000000000000000000))
(test #f eqv? (list 10000000000000000000 10000000000000000001))
(test #f eqv? (list 10000000000000000000 20000000000000000000))
(test #f eqv? (list (cons 1 2) (cons 1 2)))
(test #f eqv? (list (lambda () 1) (lambda () 2)))
(test #f eqv? (list #f 'nil))
(let ((p (lambda (x) x)))
  (test #t eqv? (list p p)))
(define gen-counter
 (lambda ()
   (let ((n 0))
      (lambda () (set! n (+ n 1)) n))))
(let ((g (gen-counter))) (test #t eqv? (list g g)))
(test #f eqv? (list (gen-counter) (gen-counter)))
(letrec ((f (lambda () (if (eqv? f g) 'f 'both)))
	 (g (lambda () (if (eqv? f g) 'g 'both))))
  (test #f eqv? (list f g)))
