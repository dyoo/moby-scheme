#lang s-exp "../moby-lang.ss"
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
	       (error 'test (format "expected ~s, got ~s, on ~s" 
				    expect
				    res
				    (cons fun args)))]
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
					   (set! val (+ y 5))
                                           #t))))
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
                                         (begin
                                           (set! val (+ y 5))
                                           #t))))
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
      (lambda ()
        (begin (set! n (+ n 1)) n)))))
(let ((g (gen-counter))) (test #t eqv? (list g g)))
(test #f eqv? (list (gen-counter) (gen-counter)))
(letrec ((f (lambda () (if (eqv? f g) 'f 'both)))
           (g (lambda () (if (eqv? f g) 'g 'both))))
    (test #f eqv? (list f g)))








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
;; Commented out: we don't yet properly support redefinition of structures.
#;(define-struct a (b c))
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
(set! x 5)
(test '(quasiquote (unquote x)) 'qq (list ``,x))
(test '(quasiquote (unquote 5)) 'qq (list ``,,x))
(test '(quasiquote (unquote-splicing x)) 'qq (list ``,@x))
(test '(quasiquote (unquote-splicing 5)) 'qq (list ``,@,x))
(test '(quasiquote (quasiquote (quasiquote (unquote (unquote (unquote x)))))) 'qq (list ````,,,x))
(test '(quasiquote (quasiquote (quasiquote (unquote (unquote (unquote 5)))))) 'qq (list ````,,,,x))











;; Numbers.ss



(test #f number? (list 'a))
(test #f complex? (list 'a))
(test #f real? (list 'a))
(test #f rational? (list 'a))
(test #f integer? (list 'a))

(test #t number? (list 3))
(test #t complex? (list 3))
(test #t real? (list 3))
(test #t rational? (list 3))
(test #t integer? (list 3))

(test #t number? (list 3.0))
(test #t complex? (list 3.0))
(test #t real? (list 3.0))
(test #t rational? (list 3.0))
(test #t integer? (list 3.0))

(test #t number? (list 3.1))
(test #t complex? (list 3.1))
(test #t real? (list 3.1))
(test #t rational? (list 3.1))
(test #f integer? (list 3.1))

(test #t number? (list 3/2))
(test #t complex? (list 3/2))
(test #t real? (list 3/2))
(test #t rational? (list 3/2))
(test #f integer? (list 3/2))

;; Skipping the complex nubmers for now

(test #t exact? (list 3))
(test #t exact? (list 3/4))
(test #f exact? (list (exact->inexact 3.0)))

;; Breaking: we don't have bignums!
;(test #t exact? (list (expt 2 100)))

;(test #t exact? 3+4i)
;(test #f exact? 3.0+4i)

(test #f inexact? (list 3))
(test #f inexact? (list 3/4))
(test #t inexact? (list (exact->inexact 3.0)))

;; Breaking: we don't have bignums!
;(test #f inexact? (list (expt 2 100)))

;(test #f inexact? 3+4i)
;(test #t inexact? 3.0+4i)
;(test #t inexact? 0+4.0i)
;(test #t inexact? 4+0.i)

;(test #t complex? (list -4.242154731064108e-5-6.865001427422244e-5i))
;(test #f exact? (list -4.242154731064108e-5-6.865001427422244e-5i))
;(test #t inexact? (list -4.242154731064108e-5-6.865001427422244e-5i))

;(test #t complex? (list -4.242154731064108f-5-6.865001427422244f-5i))
;(test #f exact? (list -4.242154731064108f-5-6.865001427422244f-5i))
;(test #t inexact? (list -4.242154731064108f-5-6.865001427422244f-5i))


(test #t number? (list +inf.0))
(test #t complex? (list +inf.0))
(test #t real? (list +inf.0))
(test #f rational? (list +inf.0))
(test #f integer? (list +inf.0))

(test #t number? (list -inf.0))
(test #t complex? (list -inf.0))
(test #t real? (list -inf.0))
(test #f rational? (list -inf.0))
(test #f integer? (list -inf.0))


(test "+inf.0" number->string (list +inf.0))
(test "-inf.0" number->string (list -inf.0))

;; Currently broken: negative zero isn't properly supported yet.
(skip (lambda () (test #t = (list 0.0 -0.0))))
(skip (lambda () (test #f eqv? (list 0.0 -0.0))))
(skip (lambda () (test #f equal? (list 0.0 -0.0))))
(skip (lambda () (test #f eqv? (list -0.0 0.0))))
(skip (lambda () (test #t eqv? (list 0.0 0.0))))
(skip (lambda () (test #t eqv? (list -0.0 -0.0))))


(test #t = (list 22 22 22))
(test #t = (list 22 22))
(test #f = (list 34 34 35))
(test #f = (list 34  35))
(test #t > (list 3 -6246))
(test #f > (list 9 9 -2424))
(test #t >= (list 3 -4  -6246))
(test #t >= (list 9 9))
(test #f >= (list 8 9))
(test #t < (list -1 2 3 4 5 6 7 8))
(test #f < (list -1 2 3 4 4 5 6 7))
(test #t <= (list -1 2 3 4 5 6 7 8))
(test #t <= (list -1 2 3 4 4 5 6 7))
(test #f < (list 1 3 2))
(test #f >= (list 1 3 2))


(define (test-compare lo m hi) ; all positive!
  (local [(define -lo (- lo))
	  (define -m (- m))
	  (define -hi (- hi))
	  
	  (define (test-lh l h)
	    (begin
	      (test #f > (list l h))
	      (test #t < (list l h))
	      (test #f = (list l h))
	      (test #f >= (list l h))
	      (test #t <= (list l h))))
	  
	  (define (test-hl h l)
	    (begin
	      (test #t > (list h l))
	      (test #f < (list h l))
	      (test #f = (list h l))
	      (test #t >= (list h l))
	      (test #f <= (list h l))))
	  
	  (define (test-zero z)
	    (begin
	      (test-hl m z)
	      (test-lh -m z)
	      (test-hl z -m)
	      (test-lh z m)))]
	  
   (begin
     (test-lh m hi)
     (test-hl -m -hi)
     
     (test #f > (list m m))
     (test #f < (list m m))
     (test #t = (list m m))
     (test #t >= (list m m))
     (test #t <= (list m m))
     
     (test-hl m -m)
     (test-lh -m m)
     
     (test-hl m lo)
     (test-lh -m -lo)
     
     (test-zero 0)
     (test-zero 0.0)


     (test-compare 0.5 1.2 2.3)
     (test-compare 2/5 1/2 2/3)
     (test-compare 1/4 1/3 1/2) ; same numerator
     (test-compare 3/10 7/10 9/10) ; same denominator
					;(test-compare 2/500000000000000000000000000 1/200000000000000000000000000 2/300000000000000000000000000) ; bignums
     (test #t = (list 1/2 2/4))
     (test #f = (list 2/3 2/5))
					;(test #f = 2/3 2/500000000000000000000000000)

     (test-compare 0.5 6/5 2.3)
     (test-compare 1 11922615739/10210200 3000)
     (test-compare 1.0 11922615739/10210200 3000.0)

     (test #t < (list 0.5 2/3))
     (test #f < (list 2/3 0.5))
     (test #t = (list 0.5 1/2))
     

     )))









(test #t zero? (list 0))
(test #t zero? (list 0.0))
(test #t zero? (list 0/1))
(test #f zero? (list 1))
(test #f zero? (list -1))
(test #f zero? (list -100))
(test #f zero? (list 1.0))
(test #f zero? (list -1.0))
(test #f zero? (list 1/2))
(test #f zero? (list -1/2))
(test #f zero? (list -1/2+2i))
(test #f zero? (list +inf.0))
(test #f zero? (list -inf.0))
(test #f zero? (list (expt 2 37)))
(test #f zero? (list (expt -2 37)))
(test #t positive? (list 4))
(test #f positive? (list -4))
(test #f positive? (list 0))
(test #t positive? (list 4.0))
(test #f positive? (list -4.0))
(test #f positive? (list 0.0))
(test #t positive? (list 2/4))
(test #f positive? (list -2/4))
(test #f positive? (list 0/2))
(test #t positive? (list +inf.0))
(test #f positive? (list -inf.0))
(test #t positive? (list (expt 2 37)))
(test #f positive? (list (expt -2 37)))
(test #f negative? (list 4))
(test #t negative? (list -4))
(test #f negative? (list 0))
(test #f negative? (list 4.0))
(test #t negative? (list -4.0))
(test #f negative? (list 0.0))
(test #f negative? (list 2/4))
(test #t negative? (list -2/4))
(test #f negative? (list 0/4))
(test #f negative? (list (expt 2 37)))
(test #t negative? (list (expt -2 37)))
(test #f negative? (list +inf.0))
(test #t negative? (list -inf.0))
(test #t odd? (list 3))
(test #f odd? (list 2))
(test #f odd? (list -4))
(test #t odd? (list -1))
(test #f odd? (list (expt 2 37)))
(test #f odd? (list (expt -2 37)))
(test #t odd? (list (add1 (expt 2 37))))
(test #t odd? (list (sub1 (expt -2 37))))
(test #f even? (list 3))
(test #t even? (list 2))
(test #t even? (list -4))
(test #f even? (list -1))
(test #t even? (list (expt 2 37)))
(test #t even? (list (expt -2 37)))
(test #f even? (list (add1 (expt 2 37))))
(test #f even? (list (sub1 (expt -2 37))))


(test 5 max (list 5))
(test 5 min (list 5))
(test 38 max (list 34 5 7 38 6))
(test -24 min (list 3  5 5 330 4 -24))
(test 38.0 max (list 34 5.0 7 38 6))
(test -24.0 min (list 3  5 5 330 4 -24.0))
(test 2/3 max (list 1/2 2/3))
(test 2/3 max (list 2/3 1/2))
(test 2/3 max (list 2/3 -4/5))
(test 1/2 min (list 1/2 2/3))
(test 1/2 min (list 2/3 1/2))
(test -4/5 min (list 2/3 -4/5))
(test +inf.0 max (list +inf.0 0 -inf.0))
(test -inf.0 min (list +inf.0 0 -inf.0))



(test (expt 5 27) max (list 9 (expt 5 27)))
(test (expt 5 29) max (list (expt 5 29) (expt 5 27)))
(test (expt 5 29) max (list (expt 5 27) (expt 5 29)))
(test (expt 5 27) max (list (expt 5 27) 9))
(test (expt 5 27) max (list (expt 5 27) (- (expt 5 29))))
(test (expt 5 27) max (list (- (expt 5 29)) (expt 5 27)))
(test (- (expt 5 27)) max (list (- (expt 5 27)) (- (expt 5 29))))
(test (- (expt 5 27)) max (list (- (expt 5 29)) (- (expt 5 27))))
(test 9 min (list 9 (expt 5 27)))
(test (expt 5 27) min (list (expt 5 29) (expt 5 27)))
(test (expt 5 27) min (list (expt 5 27) (expt 5 29)))
(test 9 min (list (expt 5 27) 9))
(test (- (expt 5 29)) min (list (expt 5 27) (- (expt 5 29))))
(test (- (expt 5 29)) min (list (- (expt 5 29)) (expt 5 27)))
(test (- (expt 5 29)) min (list (- (expt 5 27)) (- (expt 5 29))))
(test (- (expt 5 29)) min (list (- (expt 5 29)) (- (expt 5 27))))


(test 0 + (list))
(test 7 + (list 3 4))
(test 6 + (list 1 2 3))
(test 7.0 + (list 3 4.0))
(test 6.0 + (list 1 2.0 3))
(test 19/12 + (list 1/4 1/3 1))
;(test +i + (list +i))
;(test 3/2+1i + (list 1 2+2i -i -3/2))
(test 3 + (list 3))
(test 0 + (list))
(test 4 * (list 4))
(test 16.0 * (list 4 4.0))
(test 1 * (list))
(test 6/25 * (list 3/5 1/5 2))
;(test #i+6/25 * (list 3/5 1/5 2.0)
;(test +6/25i * (list 3/5 1/5 2 +i))
;(test (make-rectangular 0 #i+6/25) * (list 3/5 1/5 2.0 +i))
;(test 1073741874 + (list (- (expt 2 30) 50) 100)) ; fixnum -> bignum for 32 bits
;(test -1073741874 - (list (- 50 (expt 2 30)) 100)) ; fixnum -> bignum for 32 bits
;(test 10.0+0.0i + (list 9.0+0.0i 1))
;(test 10.0+0.0i + (list 9.0+0.0i 1-0.0i))
;(test 9.0+0.0i * (list 9.0+0.0i 1))
;(test 10.0-1.0i + (list 9.0+0.0i 1-1.0i))
(test 0 * (list 0 10.0))
(test 0 * (list 0 +inf.0))
;(test 0 * (list 0 +nan.0))
;(test 0 / (list 0 0.0))
(test 0 / (list 0 +inf.0))
(test 0 / (list 0 -inf.0))
;(test 0 / (list 0 +nan.0))
(test -0.0 + (list 0 -0.0))
(test -0.0 + (list -0.0 0))
(test -0.0 - (list -0.0 0))

(test -0.0 - (list 0.0))
(test 0.0 - (list -0.0))
(test -0.0 - (list 0 0.0))
(test 0.0 - (list 0 -0.0))


(test 2 add1 (list 1))
(test 0 add1 (list -1))
(test 2.0 add1 (list 1.0))
(test 0.0 add1 (list -1.0))
(test 3/2 add1 (list 1/2))
(test 1/2 add1 (list -1/2))


(test 1 sub1 (list 2))
(test -2 sub1 (list -1))
(test 1.0 sub1 (list 2.0))
(test -2.0 sub1 (list -1.0))
(test -1/2 sub1 (list 1/2))
(test -3/2 sub1 (list -1/2))


(test 1024 expt (list 2 10))
(test 1/1024 expt (list 2 -10))
(test 1/1024 expt (list 1/2 10))
(test (/ 1 (expt 2 10000)) expt (list 1/2 10000))
(test 2 expt (list 4 1/2))
(test 2.0 expt (list 4 0.5))
(test (sqrt 5) expt (list 5 1/2))


(test 31525197391593472 inexact->exact (list 31525197391593473.0))
(test 31525197391593476 inexact->exact (list 31525197391593476.0))
(test 31525197391593476 inexact->exact (list 31525197391593476.0))


(test #t positive? (list (inexact->exact 0.1)))
(test #t negative? (list (inexact->exact -0.1)))
(test 0 + (list (inexact->exact -0.1) (inexact->exact 0.1)))








(define (test-inf-plus-times v)
  (local [(define (test+ +)
	    (begin
	      (test +inf.0 + (list v (+ +inf.0)))
	      (test -inf.0 + (list v (+ -inf.0)))
	      (test +inf.0 + (list (- v) (+ +inf.0)))
	      (test -inf.0 + (list (- v) (+ -inf.0)))
	      
	      (test +inf.0 + (list +inf.0 v))
	      (test -inf.0 + (list -inf.0 v))
	      (test +inf.0 + (list +inf.0 (- v)))
	      (test -inf.0 + (list -inf.0 (- v)))
	      
	      #;(test-nan.0 + +nan.0 v)
	      #;(test-nan.0 + v +nan.0)))]
  (begin
    (test+ +)
    (test+ -)
    
    (test +inf.0 * (list +inf.0 v))
    (test -inf.0 * (list -inf.0 v))
    (test -inf.0 * (list +inf.0 (- v)))
    (test +inf.0 * (list -inf.0 (- v)))

    (test +inf.0 * (list v +inf.0))
    (test -inf.0 * (list v -inf.0))
    (test -inf.0 * (list (- v) +inf.0))
    (test +inf.0 * (list (- v) -inf.0)))))

    ;(test-nan.0 * +nan.0 v)
    ;(test-nan.0 * v +nan.0)

(test-inf-plus-times 1)
(test-inf-plus-times 1.0)
(test-inf-plus-times (expt 2 100))


(test -inf.0 - (list +inf.0))
(test +inf.0 - (list -inf.0))
(test +inf.0 + (list +inf.0 +inf.0))
(test -inf.0 + (list -inf.0 -inf.0))
(test +inf.0 * (list +inf.0 +inf.0))
(test -inf.0 * (list +inf.0 -inf.0))
(test 0 * (list +inf.0 0))






(test 1/2 / (list 1 2))
(test -1/3 / (list -1 3))
(test -1/3 / (list 1 -3))
(test 1/2 / (list 1/4 1/2))
(test 0.5 / (list 1 2.0))
(test 0.5 / (list 1.0 2))
;(test 1/2+3/2i / 1+3i 2)
;(test 1/5-3/5i / 2 1+3i)
;(test 0.5+0.0i / 1+0.0i 2)
;(test 0.25-0.0i / 1 4+0.0i)
;(test 0.25+0.0i / 1+0.0i 4+0.0i)
;(test 0 / 0 4+3i)
;(test 0.25+0.0i / 1e300+1e300i (* 4 1e300+1e300i))
;(test 0.25+0.0i / 1e-300+1e-300i (* 4 1e-300+1e-300i))
;(test 1/2-1/2i / 1+1i)
;(test 1/2+1/2i / 1-1i)
;(test 1/5-2/5i / 1+2i)
;(test 1/5+2/5i / 1-2i)
;(test 2/5-1/5i / 2+1i)
;(test 2/5+1/5i / 2-1i)
;(test 0.5-0.5i / 1.0+1.0i)
;(test 0.5+0.5i / 1.0-1.0i)
;(test 0.2-0.4i / 1.0+2.0i)
;(test 0.2+0.4i / 1.0-2.0i)
;(test 0.4-0.2i / 2.0+1.0i)
;(test 0.4+0.2i / 2.0-1.0i)


(test 3 / (list 1 1/3))
(test -3 / (list 1 -1/3))
(test -3 / (list -1 1/3))
(test 3 / (list -1 -1/3))
(test 1/3 / (list 1 3))
(test -1/3 / (list 1 -3))
(test -1/3 / (list -1 3))
(test 1/3 / (list -1 -3))
(test 3/2 / (list 1 2/3))
(test -3/2 / (list 1 -2/3))
(test -3/2 / (list -1 2/3))
(test 3/2 / (list -1 -2/3))




(test (expt 3 50) / (list 1 (/ 1 (expt 3 50))))
(test (- (expt 3 50)) / (list 1 (- (/ 1 (expt 3 50)))))
(test (- (expt 3 50)) / (list -1 (/ 1 (expt 3 50))))
(test (expt 3 50) / (list -1 (- (/ 1 (expt 3 50)))))
(test (/ 1 (expt 3 50)) / (list 1 (expt 3 50)))
(test (- (/ 1 (expt 3 50))) / (list 1 (- (expt 3 50))))
(test (- (/ 1 (expt 3 50))) / (list -1 (expt 3 50)))
(test (/ 1 (expt 3 50)) / (list -1 (- (expt 3 50))))
(test (/ (expt 3 50) (expt 2 70)) / (list 1 (/ (expt 2 70) (expt 3 50))))
(test (- (/ (expt 3 50) (expt 2 70))) / (list 1 (- (/ (expt 2 70) (expt 3 50)))))
(test (- (/ (expt 3 50) (expt 2 70))) / (list -1 (/ (expt 2 70) (expt 3 50))))
(test (/ (expt 3 50) (expt 2 70)) / (list -1 (/ (- (expt 2 70)) (expt 3 50))))



(test (- (expt 2 30)) / (list (- (expt 2 30)) 1))
(test (expt 2 30) / (list (- (expt 2 30)) -1))
(test (expt 2 29) / (list (- (expt 2 30)) -2))
(test -1/1073741824 / (list (- (expt 2 30))))

(test +inf.0 / (list 1.0 0.0))
(test -inf.0 / (list -1.0 0.0))
(test +inf.0 / (list -1.0 -0.0))
(test -inf.0 / (list 1.0 -0.0))






(js-big-bang #f
	     (on-draw 
	      (lambda (w)
		(list (js-text 
		       (format "~a tests run.  ~a tests skipped"
			       number-of-tests
			       number-of-skipped-tests))))
	      (lambda (w)
		'())))

