#lang moby
;; This is a set of basic tests.  A lot of this is copy-and-pasted from PLT-Scheme's
;; test suite.


(define number-of-tests 0)

(define (test expect fun args)
  (begin
    (set! number-of-tests (add1 number-of-tests))
    (let ([res (if (procedure? fun)
		   (apply fun args)
		   (car args))])
      (let ([ok? (equal? expect res)])
	(cond [(not ok?)
	       (error 'test (format "~s" (list res expect (cons fun args))))]
	      [else
	       ok?])))))




;; test that all symbol characters are supported.
'(+ - ... !.. $.+ %.- &.! *.: /:. :+. <-. =. >. ?. ~. _. ^.)


(define disjoint-type-functions
  (list boolean? char? null? number? pair? procedure? string? symbol? vector?))






;; Let tests, from syntax.ss
;;
;; FIXME:
;; The tests involving internal defines have been commented out
;; for now.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(test 6 'let (list (let ((x 2) (y 3)) (* x y))))
(test 'second 'let (list (let ((x 2) (y 3)) (begin (* x y) 'second))))
(test 35 'let (list (let ((x 2) (y 3)) (let ((x 7) (z (+ x y))) (* z x)))))
(test 70 'let* (list (let ((x 2) (y 3)) (let* ((x 7) (z (+ x y))) (* z x)))))

(test #t 'letrec (list (letrec ((-even?
                           (lambda (n) (if (zero? n) #t (-odd? (- n 1)))))
                          (-odd?
                           (lambda (n) (if (zero? n) #f (-even? (- n 1))))))
                   (-even? 88))))
(define x 34)
;(test 5 'let (let ((x 3)) (define x 5) x))
;(test 5 'let (let ((x 3)) (define-values (x w) (values 5 8)) x))
(test 34 'let (list x))
;(test 6 'let (let () (define x 6) x))
(test 34 'let (list x))
;(test 7 'let* (let* ((x 3)) (define x 7) x))
(test 34 'let* (list x))
;(test 8 'let* (let* () (define x 8) x))
(test 34 'let* (list x))
;(test 9 'letrec (letrec () (define x 9) x))
(test 34 'letrec (list x))
;(test 10 'letrec (letrec ((x 3)) (define x 10) x))
(test 34 'letrec (list x))
(test 3 'let (list (let ((y 'apple) (x 3) (z 'banana)) x)))
(test 3 'let* (list (let* ((y 'apple) (x 3) (z 'banana)) x)))
(test 3 'letrec (list (letrec ((y 'apple) (x 3) (z 'banana)) x)))
(test 3 'let* (list (let* ((x 7) (y 'apple) (z (set! x 3))) x)))
(test 3 'let* (list (let* ((x 7) (y 'apple) (z (if (not #f) (set! x 3) #f))) x)))
(test 3 'let* (list (let* ((x 7) (y 'apple) (z (if (not #t) #t (set! x 3)))) x)))

(let ([val 0])
  (begin
    (let ([g (lambda ()
	       (letrec ([f (lambda (z x)
			     (if (let ([w (even? 81)])
				   (if w
				       w
				       (let ([y x])
					 (begin
					   (set! x 7)
					   (set! val (+ y 5))))))
				 'yes
				 'no))])
		 (f 0 11)))])
      (g))
    (test 16 identity (list val))))


(let ([val 0])
  (begin
    (let ([g (lambda ()
	       (letrec ([f (lambda (z x)
			     (if (let ([w (even? 81)])
				   (if w
				       w
				       (let ([y x])
					 (set! val (+ y 5)))))
				 'yes
				 'no))])
		 (f 0 11)))])
      (g))
    (test 16 identity (list val))))





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
(test #f eqv? (list 2 (exact->inexact 2.0)))
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
