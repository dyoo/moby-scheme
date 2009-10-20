#lang moby
;; This is a set of basic tests.  A lot of this is copy-and-pasted from PLT-Scheme's
;; test suite.


(define number-of-tests 0)
(define number-of-skipped-tests 0)


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


;; Just a note to myself about which tests need to be fixed.
(define (skip f)
  (begin
    (set! number-of-skipped-tests (add1 number-of-skipped-tests))))



;; test that all symbol characters are supported.
'(+ - ... !.. $.+ %.- &.! *.: /:. :+. <-. =. >. ?. ~. _. ^.)


(define disjoint-type-functions
  (list boolean? char? null? number? pair? procedure? string? symbol? vector?))





;; Let tests, from syntax.ss
;;
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

;; FIXME:
;; The tests involving internal defines have been changed to use local
;; for now.
(test 5 'let (list (let ((x 3)) (local [(define x 5)] x))))
(test 5 'let (list (let ((x 3)) (local [(define x 5) (define w 8)] x))))
(test 34 'let (list x))
(test 6 'let (list (let () (local [(define x 6)] x))))
(test 34 'let (list x))
(test 7 'let* (list (let* ((x 3)) (local [(define x 7)] x))))
(test 34 'let* (list x))
(test 8 'let* (list (let* () (local [(define x 8)] x))))
(test 34 'let* (list x))
(test 9 'letrec (list (letrec () (local [(define x 9)] x))))
(test 34 'letrec (list x))
(test 10 'letrec (list (letrec ((x 3)) (local [(define x 10)] x))))

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

;; These tests are commented out because Moby doesn't have bignums
;; yet.
(skip (lambda () (test #t eqv? (list 10000000000000000000 10000000000000000000))))
(skip (lambda () (test #f eqv? (list 10000000000000000000 10000000000000000001))))
(skip (lambda () (test #f eqv? (list 10000000000000000000 20000000000000000000))))

;; This test is commented out because cons only allows lists as a
;; second argument.
(skip (lambda () (test #f eqv? (list (cons 1 2) (cons 1 2)))))

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




(js-big-bang #f
	     (on-draw 
	      (lambda (w)
		(list (js-text 
		       (format "~a tests run.  ~a tests skipped"
			       number-of-tests
			       number-of-skipped-tests))))
	      (lambda (w)
		'())))





;; Some tests with structures, from struct.ss

(define-struct a (b c))
(define-struct aa ())
(define ai (make-a 1 2))
(define aai (make-aa))
;;(test #t struct-type? struct:a)
;;(test #f struct-type? 5)
(test #t procedure? (list a?))
(test #t a? (list ai))
(test #f a? (list 1))
(test #f aa? (list ai))
;; We don't have struct inspectors, so this test should fail.
;;(test #f struct? (list ai))
(test 1 a-b (list ai))
(test 2 a-c (list ai))
(define ai2 (make-a 1 2))
(set-a-b! ai2 3)
(set-a-c! ai2 4)
(test 1 a-b (list ai))
(test 2 a-c (list ai))
(test 3 a-b (list ai2))
(test 4 a-c (list ai2))
(define-struct a (b c))
;; Commented out: we don't yet properly support redefinition of structures.
#;(test #f a? (list ai))









;; Quasiquotation tests from syntax.ss.
(test '(list 3 4) 'quasiquote (list `(list ,(+ 1 2) 4)))
(test '(list a (quote a)) 'quasiquote (list (let ((name 'a)) `(list ,name ',name))))
(test '(a 3 4 5 6 b) 'quasiquote (list `(a ,(+ 1 2) ,@(map abs '(4 -5 6)) b)))

;; Test commented out: we shouldn't support dotted pairs.
;(test '((foo 7) . cons)
;	'quasiquote
;	`((foo ,(- 10 3)) ,@(cdr '(c)) . ,(car '(cons))))

;; Test commented out for now: we don't yet support this syntax for vectors.
;(test '#(10 5 2 4 3 8) 'quasiquote (list `#(10 5 ,(sqrt 4) ,@(map sqrt '(16 9)) 8)))

(test 5 'quasiquote (list `,(+ 2 3)))

;; Test with foo commented out: we don't yet support the required let/cc form
;; needed to exercise this 
;(test '(a `(b ,(+ 1 2) ,(foo 4 d) e) f)
;      'quasiquote (list `(a `(b ,(+ 1 2) ,(foo ,(+ 1 3) d) e) f)))

(test '(a `(b ,x ,'y d) e) 'quasiquote
	(list (let ((name1 'x) (name2 'y)) `(a `(b ,,name1 ,',name2 d) e))))
(test '(list 3 4) 'quasiquote (list (quasiquote (list (unquote (+ 1 2)) 4))))
(test '`(list ,(+ 1 2) 4) 'quasiquote (list '(quasiquote (list (unquote (+ 1 2)) 4))))
(test '(()) 'qq (list `((,@'()))))
(define x 5)
(test '(quasiquote (unquote x)) 'qq (list ``,x))
(test '(quasiquote (unquote 5)) 'qq (list ``,,x))
;(test '(quasiquote (unquote-splicing x)) 'qq (list ``,@x))
;(test '(quasiquote (unquote-splicing 5)) 'qq (list ``,@,x))
;(test '(quasiquote (quasiquote (quasiquote (unquote (unquote (unquote x)))))) 'qq (list ````,,,x))
;(test '(quasiquote (quasiquote (quasiquote (unquote (unquote (unquote 5)))))) 'qq (list ````,,,,x))
