#lang s-exp "../moby-lang.ss"
;; This is a set of basic tests.  A lot of this is copy-and-pasted from PLT-Scheme's
;; test suite.

#;(require "test-harness.ss")


(define number-of-tests 0)
(define number-of-skipped-tests 0)
(define number-of-errors 0)
 
(define error-messages empty)
 
(define (add-error-message! msg)
  (set! error-messages (append error-messages (list msg))))
 
 
(define (test expect fun args)
  (begin
    (set! number-of-tests (add1 number-of-tests))
    (let ([res (if (procedure? fun)
		      (apply fun args)
		         (car args))])
      (let ([ok? (equal? expect res)])
	(cond [(not ok?)
               (begin
                 (add-error-message! (format "expected ~s, got ~s, on ~s" 
                                             expect
                                             res
                                             (cons fun args)))
                 (set! number-of-errors (add1 number-of-errors))
                 (list false expect fun args))]
	            [else
		            (list ok? expect fun args)])))))


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


(define (test-nan.0 f args)
  (test +nan.0 f args))

(define (test-i-nan.0 f args)
  (test (make-rectangular +nan.0 +nan.0) f args))

(define (test-nan c)
  (begin
    (test #f < (list +nan.0 c))
    (test #f > (list +nan.0 c))
    (test #f = (list +nan.0 c))
    (test #f <= (list +nan.0 c))
    (test #f >= (list +nan.0 c))))





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

(skip (lambda () (test +inf.0 / (list 1.0 0.0))))
(skip (lambda () (test -inf.0 / (list -1.0 0.0))))
(skip (lambda () (test +inf.0 / (list -1.0 -0.0))))
(skip (lambda () (test -inf.0 / (list 1.0 -0.0))))


(skip (lambda () (test 0.0 identity (list (exact->inexact (/ (expt 2 5000) (add1 (expt 2 5000000))))))))
(skip (lambda () (test -0.0 identity (list (exact->inexact (/ (- (expt 2 5000)) (add1 (expt 2 5000000))))))))
(skip (lambda () (test #t positive? (list (exact->inexact (* 5 (expt 10 -324)))))))
(skip (lambda () (test #t negative? (list (exact->inexact (* -5 (expt 10 -324)))))))
(test #t zero? (list (exact->inexact (* 5 (expt 10 -325)))))
(skip (lambda () (test #t positive? (list (exact->inexact (* 45 (expt 10 -325)))))))


(test -1 - (list 3 4))
(test -3 - (list 3))
(test -1.0 - (list 3.0 4))
(test -3.0 - (list 3.0))
(test 7 abs (list -7))
(test (expt 7 100) abs (list (- (expt 7 100))))
(test (expt 7 100) abs (list (expt 7 100)))
(test 7.0 abs (list -7.0))
(test 7 abs (list 7))
(test 0 abs (list 0))
(test 1/2 abs (list 1/2))
(test 1/2 abs (list -1/2))
(test +inf.0 abs (list +inf.0))
(test +inf.0 abs (list -inf.0))


(test 1073741823 abs (list -1073741823))
(test 1073741823 abs (list 1073741823))
(test 1073741824 abs (list -1073741824))
(test 1073741824 abs (list 1073741824))
(test 1073741825 abs (list -1073741825))
(test 1073741825 abs (list 1073741825))



(test 5 quotient (list 35 7))
(test 5.0 quotient (list 35 7.0))
(test 5.0 quotient (list 36 7.0))
(test 5.0 quotient (list 36.0 7))
(test -5 quotient (list -35 7))
(test -5.0 quotient (list -35 7.0))
(test -5 quotient (list 35 -7))
(test -5.0 quotient (list 35 -7.0))
(test 5 quotient (list -35 -7))
(test 5.0 quotient (list -35 -7.0))
(test -5.0 quotient (list -36 7.0))
(test -5.0 quotient (list 36.0 -7))
(test 0 quotient (list 0 5.0))
(test 0 quotient (list 0 -5.0))
(test 1 modulo (list 13 4))
(test 1 remainder (list 13 4))
(test 1.0 modulo (list 13 4.0))
(test 1.0 remainder (list 13 4.0))
(test 3 modulo (list -13 4))
(test -1 remainder (list -13 4))
(test 3.0 modulo (list -13 4.0))
(test -1.0 remainder (list -13 4.0))
(test -3 modulo (list 13 -4))
(test 1 remainder (list 13 -4))
(test -3.0 modulo (list 13.0 -4))
(test 1.0 remainder (list 13.0 -4))
(test -1 modulo (list -13 -4))
(test -1 remainder (list -13 -4))
(test -1.0 modulo (list -13 -4.0))
(test -1.0 remainder (list -13 -4.0))
(test -2 remainder (list -3333333332 -3))
(test -2 modulo (list -3333333332 -3))
(test 2 remainder (list 3333333332 -3))
(test -1 modulo (list 3333333332 -3))
(test 0 modulo (list 4 2))
(test 0 modulo (list -4 2))
(test 0 modulo (list 4 -2))
(test 0 modulo (list -4 -2))
(test 0.0 modulo (list 4.0 2))
(test 0.0 modulo (list -4.0 2))
(test 0.0 modulo (list 4.0 -2))
(test 0.0 modulo (list -4.0 -2))
(test 0 remainder (list 4 2))
(test 0 remainder (list -4 2))
(test 0 remainder (list 4 -2))
(test 0 remainder (list -4 -2))
(test 0.0 remainder (list 4.0 2))
(test 0.0 remainder (list -4.0 2))
(test 0.0 remainder (list 4.0 -2))
(test 0.0 remainder (list -4.0 -2))
(test 0 modulo (list 0 5.0))
(test 0 modulo (list 0 -5.0))
(test 0 remainder (list 0 5.0))
(test 0 remainder (list 0 -5.0))



(define (divtest n1 n2)
  (= n1 (+ (* n2 (quotient n1 n2))
	   (remainder n1 n2))))
(test #t divtest (list 238 9))
(test #t divtest (list -238 9))
(test #t divtest (list 238 -9))
(test #t divtest (list -238 -9))

(test 13.0 quotient (list 1324.0 100))


;; Check 0.0 combinations
(test -0.0 quotient (list -0.0 2.0))
(test 0.0 quotient (list -0.0 -2.0))
(test 0.0 quotient (list 0.0 2.0))
(test -0.0 quotient (list 0.0 -2.0))
(test 0.0 modulo (list -0.0 2.0))
(test 0.0 modulo (list -0.0 -2.0))
(test 0.0 modulo (list 0.0 2.0))
(test 0.0 modulo (list 0.0 -2.0))
(test 0.0 remainder (list -0.0 2.0))
(test 0.0 remainder (list -0.0 -2.0))
(test 0.0 remainder (list 0.0 2.0))
(test 0.0 remainder (list 0.0 -2.0))






(test 4 gcd (list 0 4))
(test 4 gcd (list -4 0))
#;(test 4 gcd (list 4))
#;(test 4 gcd (list -4))
(test 4 gcd (list 32 -36))
(test 2 gcd (list 6 10 14))
#;(test 0 gcd (list))
(test 5 gcd (list 5))
(test 5.0 gcd (list 5.0 10))
(test 5.0 gcd (list -5.0 10))
(test 5.0 gcd (list 5.0 -10))
(test 5.0 gcd (list 5.0))
(test 5.0 gcd (list -5.0))
(test 3 gcd (list 0 0 3 0))
(test 3.0 gcd (list 0.0 0 3 0))
(test 0 gcd (list 0 0 0))


(test (expt 3 37) gcd (list (expt 9 35) (expt 6 37)))
(test (expt 3 37) gcd (list (- (expt 9 35)) (expt 6 37)))
(test (expt 3 37) gcd (list (expt 9 35) (- (expt 6 37))))
(test (expt 3 75) gcd (list (expt 3 75)))
(test (expt 3 75) gcd (list (- (expt 3 75))))

(test 201 gcd (list (* 67 (expt 3 20)) (* 67 3)))
(test 201 gcd (list (* 67 3) (* 67 (expt 3 20))))
(test 6 gcd (list (* 3 (expt 2 100)) 66))
(test 6 gcd (list 66 (* 3 (expt 2 100))))
(test 201.0 gcd (list (* 67 (expt 3 20)) (* 67. 3)))
(test 201.0 gcd (list (* 67. 3) (* 67 (expt 3 20))))
(test (expt 9 35) gcd (list (expt 9 35) 0))
(test (expt 9 35) gcd (list 0 (expt 9 35)))
(test 288 lcm (list 32 -36))
(test 12 lcm (list 2 3 4))

#;(test 1 lcm (list))
(test 5 lcm (list 5))
(test 5 lcm (list -5))
(test 0 lcm (list 123 0))

(test 0 lcm (list 0 0))
(test 0.0 lcm (list 0 0.0))
(test 0.0 lcm (list 0.0 0))


(test 30.0 lcm (list 5 6.0))
(test 6.0 lcm (list 6.0))
(test 6.0 lcm (list -6.0))
(test 0.0 lcm (list 123 0.0))
(test 0.0 lcm (list 123 -0.0))
(test (* (expt 2 37) (expt 9 35)) lcm (list (expt 9 35) (expt 6 37)))
(test (* (expt 2 37) (expt 9 35)) lcm (list (- (expt 9 35)) (expt 6 37)))
(test (* (expt 2 37) (expt 9 35)) lcm (list (expt 9 35) (- (expt 6 37))))







(test 2 floor (list 5/2))
(test 3 ceiling (list 5/2))
(test 2 round (list 5/2))
#;(test 2 truncate (list 5/2))
(test -3 floor (list -5/2))
(test -2 ceiling (list -5/2))
(test -2 round (list -5/2))
#;(test -2 truncate (list -5/2))

(test 1 floor (list 4/3))
(test 2 ceiling (list 4/3))
(test 1 round (list 4/3))
#;(test 1 truncate (list 4/3))
(test -2 floor (list -4/3))
(test -1 ceiling (list -4/3))
(test -1 round (list -4/3))
#;(test -1 truncate (list -4/3))

(test 1 floor (list 5/3))
(test 2 ceiling (list 5/3))
(test 2 round (list 5/3))
#;(test 1 truncate (list 5/3))
(test -2 floor (list -5/3))
(test -1 ceiling (list -5/3))
(test -2 round (list -5/3))
#;(test -1 truncate (list -5/3))

(test 2 floor (list 11/4))
(test 3 ceiling (list 11/4))
(test 3 round (list 11/4))
#;(test 2 truncate (list 11/4))
(test -3 floor (list -11/4))
(test -2 ceiling (list -11/4))
(test -3 round (list -11/4))
#;(test -2 truncate (list -11/4))

(test 2 floor (list 9/4))
(test 3 ceiling (list 9/4))
(test 2 round (list 9/4))
#;(test 2 truncate (list 9/4))
(test -3 floor (list -9/4))
(test -2 ceiling (list -9/4))
(test -2 round (list -9/4))
#;(test -2 truncate (list -9/4))

(test 2.0 floor (list 2.4))
(test 3.0 ceiling (list 2.4))
(test 2.0 round (list 2.4))
#;(test 2.0 truncate (list 2.4))
(test -3.0 floor (list -2.4))
(test -2.0 ceiling (list -2.4))
(test -2.0 round (list -2.4))
#;(test -2.0 truncate (list -2.4))

(test 2.0 floor (list 2.6))
(test 3.0 ceiling (list 2.6))
(test 3.0 round (list 2.6))
#;(test 2.0 truncate (list 2.6))
(test -3.0 floor (list -2.6))
(test -2.0 ceiling (list -2.6))
(test -3.0 round (list -2.6))
#;(test -2.0 truncate (list -2.6))

(test 2.0 round (list 2.5))
(test -2.0 round (list -2.5))
(test 4.0 round (list 3.5))
(test -4.0 round (list -3.5))




(define (test-zero-ident f)
  (begin
    (test 0.0 f (list 0.0))
    (test -0.0 f (list -0.0))))
(test-zero-ident round)
(test-zero-ident floor)
(test-zero-ident ceiling)
#;(test-zero-ident truncate)




(test +inf.0 floor (list +inf.0))
(test +inf.0 ceiling (list +inf.0))
(test +inf.0 round (list +inf.0))
#;(test +inf.0 truncate (list +inf.0))
(test -inf.0 floor (list -inf.0))
(test -inf.0 ceiling (list -inf.0))
(test -inf.0 round (list -inf.0))
#;(test -inf.0 truncate (list -inf.0))
#;(test +nan.0 floor +nan.0)
#;(test +nan.0 ceiling +nan.0)
#;(test +nan.0 round +nan.0)
#;(test +nan.0 truncate +nan.0)



(define (test-fcrt-int v)
  (begin
    (test v floor (list v))
    (test v ceiling (list v))
    (test v round (list v))
    #;(test v truncate (list v))))

(test-fcrt-int 2)
(test-fcrt-int 2.0)
(test-fcrt-int (expt 2 100))




(test 5 numerator (list 5))
(test 5000000000000 numerator (list 5000000000000))
(test 5.0 numerator (list 5.0))

(test 1 denominator (list 5))
(test 1 denominator (list 5000000000000))
(test 1.0 denominator (list 5.0))

(test 2 numerator (list 2/3))
(test 3 denominator (list 2/3))
(test 1000.0 round (list (* 10000.0 (/ (numerator 0.1) (denominator 0.1)))))




(define (test-on-reals f filter)
  (begin
    (test (filter 5) f (list 5))
    (test (filter 5.0) f (list 5.0))
    (test (filter 1/5) f (list 1/5))
    (test (filter (expt 2 100)) f (list (expt 2 100)))))

(test 1+2i make-rectangular (list 1 2))
(test 1.0+2.0i make-rectangular (list 1.0 2))
#;(test-nan.0 real-part (make-rectangular +nan.0 1))
#;(test 1.0 imag-part (list (make-rectangular +nan.0 1)))
#;(test-nan.0 imag-part (make-rectangular 1 +nan.0))
#;(test 1.0 real-part (make-rectangular 1 +nan.0))
(test +inf.0 real-part (list (make-rectangular +inf.0 -inf.0)))
(test -inf.0 imag-part (list (make-rectangular +inf.0 -inf.0)))

(test (make-rectangular +inf.0 -inf.0) * (list 1. (make-rectangular +inf.0 -inf.0)))
(test (make-rectangular +inf.0 +inf.0) * (list +1.0i (make-rectangular +inf.0 -inf.0)))
(test (make-rectangular -inf.0 +inf.0) * (list -3. (make-rectangular +inf.0 -inf.0)))
(test (make-rectangular +inf.0 -inf.0) * (list (make-rectangular +inf.0 -inf.0) 1.))
(test (make-rectangular +inf.0 +inf.0) * (list (make-rectangular +inf.0 -inf.0) +1.0i))
(test (make-rectangular -inf.0 +inf.0) * (list (make-rectangular +inf.0 -inf.0) -3.))
(test (make-rectangular +inf.0 -inf.0) / (list (make-rectangular +inf.0 -inf.0) 1.))
(test (make-rectangular -inf.0 -inf.0) / (list (make-rectangular +inf.0 -inf.0) +1.0i))
(test (make-rectangular -inf.0 +inf.0) / (list (make-rectangular +inf.0 -inf.0) -3.))

;; Test division with exact zeros in demoniator where
;;  the exact zero gets polluted to an inexact zero unless
;;  it's special-cased
(test 0-0.0i / (list 0+1.0i -inf.0))
(test -0.0-0.0i / (list 1.0+1.0i -inf.0))
(test -0.0 / (list 0+1.0i 0-inf.0i))
(test -0.0+0.0i / (list 1.0+1.0i 0-inf.0i))




#;(test-i-nan.0 * 1.+0.i (make-rectangular +inf.0 -inf.0))
#;(test-i-nan.0 * 0.+1.0i (make-rectangular +inf.0 -inf.0))
#;(test-i-nan.0 * -3.+0.i (make-rectangular +inf.0 -inf.0))
#;(test-i-nan.0 * (make-rectangular +inf.0 -inf.0) 1.+0.i)
#;(test-i-nan.0 * (make-rectangular +inf.0 -inf.0) 0.+1.0i)
#;(test-i-nan.0 * (make-rectangular +inf.0 -inf.0) -3.+0.i)
#;(test-i-nan.0 / (make-rectangular +inf.0 -inf.0) 1.+0.i)
#;(test-i-nan.0 / (make-rectangular +inf.0 -inf.0) 0.+1.0i)
#;(test-i-nan.0 / (make-rectangular +inf.0 -inf.0) -3.+0.i)




(test 1 magnitude (list 1))
(test 1 magnitude (list -1))
(test 1.0 magnitude (list 1.0))
(test 1.0 magnitude (list -1.0))
#;(test big-num magnitude (list big-num))
#;(test big-num magnitude (list (- big-num)))
(test 3/4 magnitude (list 3/4))
(test 3/4 magnitude (list -3/4))
(test 10.0 magnitude (list 10.0+0.0i))
(test 10.0 magnitude (list -10.0+0.0i))
(test 10.0 magnitude (list 0+10.0i))
(test 10 magnitude (list 0+10i))
#;(test 141421.0 round (list (* 1e-295 (magnitude 1e300+1e300i))))
#;(test 141421.0 round (list (* 1e+305 (magnitude 1e-300+1e-300i))))
#;(test +inf.0 magnitude (list +inf.0+inf.0i))
#;(test +inf.0 magnitude (list -inf.0-inf.0i))
#;(test +inf.0 magnitude (list 1+inf.0i))
#;(test +inf.0 magnitude (list +inf.0+1i))
#;(test +inf.0 magnitude (list +inf.0+0.0i))
#;(test +inf.0 magnitude (list 0.0+inf.0i))
#;(test +nan.0 magnitude (list +nan.0+inf.0i))
#;(test +nan.0 magnitude (list +inf.0+nan.0i))



(test 0 angle (list 1))
(test 0 angle (list 1.0))
(test 0 angle (list 0.0))
#;(test 0 angle (list big-num))
(test 0 angle (list 3/4))
(test 0.0 angle (list 3+0.0i))
(let ([pi (atan 0 -1)])
  (begin
    (test pi angle (list -1))
    (test pi angle (list -1.0))
    (test pi angle (list -0.0))
    #;(test pi angle (list (- big-num)))
    (test pi angle (list -3/4))
    (test pi angle (list -3+0.0i))))
(test -inf.0 atan (list 0+i))
(test -inf.0 atan (list 0-i))



(test 1 real-part (list 1+2i))
(test 1.0 real-part (list 1+2.0i))
(test 1.0 real-part (list 1+0.0i))
(test 1/5 real-part (list 1/5+2i))
(test-on-reals real-part (lambda (x) x))
(test 2.0 imag-part (list 1+2.0i))
(test 0.0 imag-part (list 1+0.0i))
(test -0.0 imag-part (list 1-0.0i))
(test 1/5 imag-part (list 1+1/5i))
(test-on-reals imag-part (lambda (x) 0))
#;(test-nan.0 real-part +nan.0 (list))
#;(test 6@1 (lambda (x) x) 6.0@1.0)
#;(test 324.0 floor (* 100 (real-part 6@1)))
#;(test 50488.0 floor (* 10000 (imag-part 6@1)))

(test 1 make-polar (list 1 0))
(test 1.0+0.0i make-polar (list 1 0.0))
(test 1.0 make-polar (list 1.0 0))
(test 1.0+0.0i make-polar (list 1.0 0.0))
#;(err/rt-test (make-polar 1.0 0.0+0.0i))
#;(err/rt-test (make-polar 1.0+0.0i 0.0))
(let ([v (make-polar 1 1)])
  (begin
  (test 5403.0 floor (list (* 10000 (real-part v))))
  (test 84147.0 floor (list (* 100000 (imag-part v))))
  (test 10000.0 round (list (* 10000.0 (magnitude v))))))
(let ([v (make-polar 1 2)])
  (begin 
    (test -416.0 ceiling (list (* 1000 (real-part v))))
    (test 909.0 floor (list (* 1000 (imag-part v))))
    (test 1.0 magnitude (list v))
    (test 2.0 angle (list v))))
#;(test-nan.0 make-polar +nan.0 0)
#;(test-i-nan.0 make-polar +nan.0 1)
#;(test-i-nan.0 make-polar 1 +nan.0)
#;(test-i-nan.0 make-polar 1 +inf.0)
#;(test-i-nan.0 make-polar 1 -inf.0)
(test +inf.0 make-polar (list +inf.0 0))
(test -inf.0 make-polar (list -inf.0 0))
(test (make-rectangular +inf.0 +inf.0) make-polar (list +inf.0 (atan 1 1)))
(test (make-rectangular -inf.0 +inf.0) make-polar (list +inf.0 (atan 1 -1)))
(test (make-rectangular +inf.0 -inf.0) make-polar (list +inf.0 (atan -1 1)))
(test 785.0 floor (list (* 1000 (angle (make-rectangular 1 1)))))
(test 14142.0 floor (list (* 10000 (magnitude (make-rectangular 1 1)))))





(define (z-round c) (make-rectangular (round (real-part c)) (round (imag-part c))))

(test -1 * (list +i +i))
(test 1 * (list +i -i))
(test 2 * (list 1+i 1-i))
(test +2i * (list 1+i 1+i))
(test -3+4i - (list 3-4i))
(test 0.5+0.0i - (list (+ 0.5 +i) +i))
(test 1/2 - (list (+ 1/2 +i) +i))
(test 1.0+0.0i - (list (+ 1 +0.5i) +1/2i))

(test 1 sqrt (list 1))
(test 1.0 sqrt (list 1.0))
(test 25 sqrt (list 625))
(test 3/7 sqrt (list 9/49))
(test 0.5 sqrt (list 0.25))
(test +1i sqrt (list -1))
(test +2/3i sqrt (list -4/9))
(test +1.0i sqrt (list -1.0))
(test 1+1i sqrt (list +2i))
(test 2+1i sqrt (list 3+4i))
(test 2.0+0.0i sqrt (list 4+0.0i))
(test +inf.0 sqrt (list +inf.0))
(test (make-rectangular 0 +inf.0) sqrt (list -inf.0))
#;(test-nan.0 sqrt +nan.0 (list))



;; Complex `sqrt' cases where both z and (magnitude z) are exact:
(test 1414.0 round (list (* 1000 (real-part (sqrt +4i)))))
(test +1414.0 round (list (* 1000 (imag-part (sqrt +4i)))))
(test 1414.0 round (list (* 1000 (real-part (sqrt -4i)))))
(test -1414.0 round (list (* 1000 (imag-part (sqrt -4i)))))
(test 1155.0 round (list (* 1000 (real-part (sqrt 1+4/3i)))))
(test +577.0 round (list (* 1000 (imag-part (sqrt 1+4/3i)))))
(test 1155.0 round (list (* 1000 (real-part (sqrt 1-4/3i)))))
(test -577.0 round (list (* 1000 (imag-part (sqrt 1-4/3i)))))


(test (expt 5 13) sqrt (list (expt 5 26)))
(test 545915034.0 round (list (sqrt (expt 5 25))))
(test (make-rectangular 0 (expt 5 13)) sqrt (list (- (expt 5 26))))
(test (make-rectangular 0 545915034.0) z-round (list (sqrt (- (expt 5 25)))))


#;(err/rt-test (sqrt "a"))
#;(arity-test sqrt 1 1)

(test 3 integer-sqrt (list 10))
(test 420 integer-sqrt (list (expt 3 11)))
(test 97184015999 integer-sqrt (list (expt 2 73)))
(skip (lambda () (test 0+3i integer-sqrt (list -10))))
(skip (lambda () (test 0+420i integer-sqrt (list (expt -3 11)))))
(skip (lambda () (test 0+97184015999i integer-sqrt (list (expt -2 73)))))

#;(test 2.0 integer-sqrt (list 5.0))
#;(test 0+2.0i integer-sqrt (list -5.0))
#;(err/rt-test (integer-sqrt 5.0+0.0i))
#;(err/rt-test (integer-sqrt -5.0+0.0i))

#;(err/rt-test (integer-sqrt "a"))
#;(err/rt-test (integer-sqrt 1.1))
#;(err/rt-test (integer-sqrt 1+1i))
#;(arity-test integer-sqrt 1 1)




(test -13/64-21/16i expt (list -3/4+7/8i 2))
(let ([v (expt -3/4+7/8i 2+3i)])
  (begin
    (printf "I see v as ~s~n" v)
    (test 3826.0 floor (list (* 10000000 (real-part v))))
    (test -137.0 ceiling (list (* 100000 (imag-part v))))))
(test 49.0+0.0i expt (list 7 2+0.0i))
(test 49.0 floor (list (* 10 (expt 2 2.3))))
(test 189.0 floor (list (* 1000 (expt 2.3 -2))))
(test 1/4 expt (list 2 -2))
(test 1/1125899906842624 expt (list 2 -50))
(test 1/1024 expt (list 1/2 10))
(test 1024 expt (list 1/2 -10))
(test 707.0 floor (list (* 1000 (expt 1/2 1/2))))
(test 707.0 floor (list (* 1000 (expt 1/2 0.5))))
(test 707.0 floor (list (* 1000 (expt 0.5 1/2))))
(test 100.0+173.0i z-round (list (* 100 (expt -8 1/3))))
(test 100.0+173.0i z-round (list (* 100 (expt -8.0 1/3))))
(test 101.0+171.0i z-round (list (* 100 (expt -8 0.33))))
(test 101.0+171.0i z-round (list (* 100 (expt -8.0 0.33))))
(test 108.0+29.0i z-round (list (* 100 (expt 1+i 1/3))))
(test 25.0-43.0i z-round (list (* 100 (expt -8 -1/3))))


;; This choice doesn't make sense to me, but it fits
;;   with other standards and implementations:
(define INF-POWER-OF_NEGATIVE +inf.0)

(test +inf.0 expt (list 2 +inf.0))
(test +inf.0 expt (list +inf.0 10))
(test 0.0 expt (list +inf.0 -2))
(test 1 expt (list +inf.0 0))
(test 1.0 expt (list +inf.0 0.))
(test +inf.0 expt (list +inf.0 +inf.0))
(test INF-POWER-OF_NEGATIVE expt (list -2 +inf.0))
(test INF-POWER-OF_NEGATIVE expt (list -inf.0 +inf.0))
(test 0.0 expt (list 2 -inf.0))
(test -inf.0 expt (list -inf.0 11))
(test +inf.0 expt (list -inf.0 10))
(test 0.0 expt (list -inf.0 -2))
(test -0.0 expt (list -inf.0 -3))
(test 1 expt (list -inf.0 0))
(test 1.0 expt (list -inf.0 0.0))
(test 0.0 expt (list +inf.0 -inf.0))
(test 0.0 expt (list -2 -inf.0))
(test 0.0 expt (list -inf.0 -inf.0))
(test 1 expt (list +nan.0 0))
(test 0 expt (list 0 10))
(test 0 expt (list 0 10.0))
(test 0 expt (list 0 +inf.0))
#;(test-nan.0 expt 0 (list +nan.0))
(test 1 expt (list 1 +inf.0))
(test 1 expt (list 1 -inf.0))
(test 1 expt (list 1 -nan.0))
(test 0.0 expt (list 0.0 10))
(test 0.0 expt (list 0.0 +inf.0))
(test +inf.0 expt (list 0.0 -5))
(test -inf.0 expt (list -0.0 -5))
(test +inf.0 expt (list 0.0 -4))
(test +inf.0 expt (list -0.0 -4))
(test +inf.0 expt (list 0.0 -4.3))
(test +inf.0 expt (list -0.0 -4.3))
(test +inf.0 expt (list 0.0 -inf.0))
#;(test-nan.0 expt 0.0 (list +nan.0))
(test 1 expt (list 0 0))
(test 1.0 expt (list 0 0.0)) ; to match (expt 0 0)
(test 1.0 expt (list 0 -0.0))
(test 1.0 expt (list 0.0 0.0))
(test 1.0 expt (list 0.0 0.0))
(test 1 expt (list 0.0 0))
(test 1 expt (list -0.0 0))
(test -0.0 expt (list -0.0 1))
#;(test-nan.0 expt +nan.0 (list 10))
#;(test-nan.0 expt 2 (list +nan.0))

(test 0 expt (list 0 1+i))
(test 0 expt (list 0 1-i))


#;(test-nan.0 expt 1.0 +inf.0)
#;(test-nan.0 expt 1.0 -inf.0)
#;(test-nan.0 expt 1.0 +nan.0)

(test 0.0 expt (list 0.0 5))
(test -0.0 expt (list -0.0 5))
(test 0.0 expt (list 0.0 4))
(test 0.0 expt (list -0.0 4))
(test 0.0 expt (list 0.0 4.3))
(test 0.0 expt (list -0.0 4.3))

(test 0.0 expt (list 0.5 +inf.0))
(test +inf.0 expt (list 0.5 -inf.0))
(test INF-POWER-OF_NEGATIVE expt (list -0.5 -inf.0))
(test +inf.0 expt (list 1.5 +inf.0))
(test 0.0 expt (list 1.5 -inf.0))
(test 0.0 expt (list -0.5 +inf.0))
(test +inf.0 expt (list -0.5 -inf.0))
(test INF-POWER-OF_NEGATIVE expt (list -1.5 +inf.0))
(test 0.0 expt (list -1.5 -inf.0))




;;;;From: fred@sce.carleton.ca (Fred J Kaudel)
;;; Modified by jaffer.
(define f3.9 (string->number "3.9"))
(define f4.0 (string->number "4.0"))
(define f-3.25 (string->number "-3.25"))
(define f.25 (string->number ".25"))
(define f4.5 (string->number "4.5"))
(define f3.5 (string->number "3.5"))
(define f0.0 (string->number "0.0"))
(define f0.8 (string->number "0.8"))
(define f1.0 (string->number "1.0"))
#;(newline)
#;(display ";testing inexact numbers; ")
#;(newline)
(test #t inexact? (list f3.9))
(test #f exact? (list f3.9))
(test #t 'inexact? (list (inexact? (max f3.9 4))))
(test f4.0 'max (list (max f3.9 4)))
(test f4.0 'exact->inexact (list (exact->inexact 4)))




; Should at least be close...
(test 4.0 round (list (log (exp 4.0))))
(test 125.0 round (list (* 1000 (asin (sin 0.125)))))
(test 125.0d0 round (list (* 1000 (magnitude (asin (sin 0.125+0.0d0i))))))
(test 125.0 round (list (* 1000 (asin (sin 1/8)))))
(test 125.0 round (list (* 1000 (acos (cos 0.125)))))
(test 125.0d0-0.0i z-round (list (* 1000 (acos (cos 0.125+0.0d0i)))))
(test 125.0 round (list (* 1000 (acos (cos 1/8)))))


(test 785.0 round (list (* 1000 (atan 1 1))))
(test 785.0 round (list (* 1000 (atan 1.0 1.0))))
#;(err/rt-test (atan 1.0 1.0+0.0i))
#;(err/rt-test (atan 1.0+0.0i 1.0))
(test 2356.0 round (list (* 1000 (atan 1 -1))))
(test -785.0 round (list (* 1000 (atan -1 1))))
(test 785.0 round (list (* 1000 (atan 1))))
(test 100.0 round (list (* 100 (tan (atan 1)))))
(test 100.0-0.0i z-round (list (* 100 (tan (+ +0.0i (atan 1))))))
(test 0.0 atan (list 0.0 0))
#;(err/rt-test (atan 0 0) exn:fail:contract:divide-by-zero?)
(test 1024.0 round (list (expt 2.0 10.0)))
(test 1024.0 round (list (expt -2.0 10.0)))
(test -512.0 round (list (expt -2.0 9.0)))
(test 32.0 round (list (sqrt 1024.0)))
(test 32.0+0.0i z-round (list (sqrt 1024.0+0.0i)))
(test 1.0+1.5e-10i sqrt (list 1+3e-10i))


(test 1 exp (list 0))
(test 1.0 exp (list 0.0))
(test 1.0 exp (list -0.0))
(test 272.0 round (list (* 100 (exp 1))))


(test 0 log (list 1))
(test 0.0 log (list 1.0))
(test -inf.0 log (list 0.0))
(test -inf.0 log (list -0.0))
(test +inf.0 log (list +inf.0))
(test +inf.0 real-part (list (log -inf.0)))
(test +3142.0 round (list (* 1000 (imag-part (log -inf.0)))))
(test +nan.0 log (list +nan.0))
#;(err/rt-test (log 0) exn:fail:contract:divide-by-zero?)

(test 1 cos (list 0))
(test 1.0 cos (list 0.0))
(test 0 sin (list 0))
(test 0.0 sin (list 0.0))
(test -0.0 sin (list -0.0))
(test 0 tan (list 0))
(test 0.0 tan (list 0.0))
(test -0.0 tan (list -0.0))

(test #t >= (list 1 (sin 12345678901234567890123)))
(test #t >= (list 1 (cos 12345678901234567890123)))
(test #t <= (list -inf.0 (tan 12345678901234567890123) +inf.0))

(test 0 atan (list 0))
(test 0.0 atan (list 0.0))
(test -0.0 atan (list -0.0))
(test 314.0 round (list (* 400 (atan 1))))
(test 314.0 round (list (* 400 (atan 1.0))))
(test 0 asin (list 0))
(test 0.0 asin (list 0.0))
(test -0.0 asin (list -0.0))
(test 314.0 round (list (* 200 (asin 1))))
(test 314.0 round (list (* 200 (asin 1.0))))
(test 0 acos (list 1))
(test 0.0 acos (list 1.0))
(test 314.0 round (list (* 200 (acos 0))))
(test 314.0 round (list (* 200 (acos 0.0))))
(test 314.0 round (list (* 200 (acos -0.0))))

(test (/ 314.0 2) round (list (* 100 (atan +inf.0))))
(test (/ -314.0 2) round (list (* 100 (atan -inf.0))))



(skip (lambda () (test 71034.0 round (list (* 100 (log 312918491891666147403524564598095080760332972643192197862041633988540637438735086398143104076897116667450730097183397289314559387355872839339937813881411504027225774279272518360586167057501686099965513263132778526566297754301647311975918380842568054630540214544682491386730004162058539391336047825248736472519))))))
(test 71117.0 round (list (* 100 (log (expt 2 1026)))))
(test 71048.0 round (list (* 100 (log (expt 2 1025)))))
(test 70978.0 round (list (* 100 (log (expt 2 1024)))))
(test 70909.0 round (list (* 100 (log (expt 2 1023)))))
(test 35420.0 round (list (* 100 (log (expt 2 511)))))
(test 35489.0 round (list (* 100 (log (expt 2 512)))))
(test 35558.0 round (list (* 100 (log (expt 2 513)))))
(test 141887.0 round (list (* 100 (log (expt 2 2047)))))
(test 141957.0 round (list (* 100 (log (expt 2 2048)))))
(test 142026.0 round (list (* 100 (log (expt 2 2049)))))
(test 23026.0 round (list (log (expt 10 10000))))
(test 23026.0 round (list (real-part (log (- (expt 10 10000))))))
(test 3.0 round (list (imag-part (log (- (expt 10 10000))))))





(define (test-inf-bad f)
  (begin
    (test-nan.0 f (list +inf.0))
    (test-nan.0 f (list -inf.0))
    (test-nan.0 f (list +nan.0))))

(test-inf-bad tan)
(test-inf-bad sin)
(test-inf-bad cos)
(test-inf-bad asin)
(test-inf-bad acos)

#;(test 11/7 rationalize (list (inexact->exact (atan +inf.0 1)) 1/100))
#;(skip (lambda () (test -11/7 rationalize (list (inexact->exact (atan -inf.0 1)) 1/100))))
(test 0.0 atan (list 1 +inf.0))
#;(skip (lambda () (test 22/7 rationalize (list (inexact->exact (atan 1 -inf.0)) 1/100))))




; Note on the following tests with atan and inf.0:
;  The IEEE standard makes this decision. I think it's a bad one,
;  since (limit (atan (g x) (f x))) as x -> +inf.0 is not necessarily
;  (atan 1 1) when (limit (f x)) and (limit (g x)) are +inf.0.
;  Perhaps IEEE makes this choice because it's easiest to compute.
#;(test 7/9 rationalize (list (inexact->exact (atan +inf.0 +inf.0)) 1/100))
#;(test 26/11 rationalize (list (inexact->exact (atan +inf.0 -inf.0)) 1/100))
#;(test -7/9 rationalize (list (inexact->exact (atan -inf.0 +inf.0)) 1/100))

(test-nan.0 atan (list +nan.0))
(test-nan.0 atan (list 1 +nan.0))
(test-nan.0 atan (list +nan.0 1))

(test -1178.+173.i  z-round (list (* 1000 (atan -2+1i))))





#;(map (lambda (f) 
       (err/rt-test (f "a"))
       (arity-test f 1 1))
     (list log exp asin acos tan))
#;(err/rt-test (atan "a" 1))
#;(err/rt-test (atan 2+i 1))
#;(err/rt-test (atan "a"))
#;(err/rt-test (atan 1 "a"))
#;(err/rt-test (atan 1 2+i))
#;(arity-test atan 1 2)

(test 3166.+1960.i  z-round (list (* 1000 (sin 1+2i))))
(test -3166.-1960.i  z-round (list (* 1000 (sin -1-2i))))
(test 0+1175.i z-round (list (* 1000 (sin 0+i))))
(test -642.-1069.i z-round (list (* 1000 (cos 2+i))))
(test -642.-1069.i z-round (list (* 1000 (cos -2-i))))
(test 1543. z-round (list (* 1000 (cos 0+i))))
(test 272-1084.i z-round (list (* 1000 (tan 1-i))))
(test -272+1084.i z-round (list (* 1000 (tan -1+i))))

(test 693.+3142.i z-round (list (* 1000 (log -2))))
(test 1571.-1317.i z-round (list (* 1000 (asin 2))))
(test -1571.+1317.i z-round (list (* 1000 (asin -2))))
(test 0+3688.i z-round (list (* 1000 (acos 20))))
(test 3142.-3688.i z-round (list (* 1000 (acos -20))))

(define (cs2 c) (+ (* (cos c) (cos c)) (* (sin c) (sin c))))
(test 0.0 imag-part (list (cs2 2+3i)))
(test 1000.0 round (list (* 1000 (real-part (cs2 2+3i)))))
(test 0.0 imag-part (list (cs2 -2+3i)))
(test 1000.0 round (list (* 1000 (real-part (cs2 -2+3i)))))
(test 0.0 imag-part (list (cs2 2-3i)))
(test 1000.0 round (list (* 1000 (real-part (cs2 2-3i)))))

(test #t positive? (list (real-part (sqrt (- 1 (* 2+3i 2+3i))))))

(test (- f4.0) round (list (- f4.5)))
(test (- f4.0) round (list (- f3.5)))
(test (- f4.0) round (list (- f3.9)))
(test f0.0 round (list f0.0))
(test f0.0 round (list f.25))
(test f1.0 round (list f0.8))
(test f4.0 round (list f3.5))
(test f4.0 round (list f4.5))

(let ((x (string->number "4195835.0"))
      (y (string->number "3145727.0")))
  (test #t 'pentium-fdiv-bug (list (> f1.0 (- x (* (/ x y) y))))))

#;(test (exact->inexact 1/3) rationalize (list .3 1/10))
#;(test 1/3 rationalize (list 3/10 1/10))
#;(test (exact->inexact 1/3) rationalize (list .3 -1/10))
#;(test 1/3 rationalize (list 3/10 -1/10))
#;(test 0 rationalize (list 3/10 4/10))
#;(test 0.0 rationalize (list .3 4/10))
#;(err/rt-test (rationalize .3+0.0i 4/10))
#;(err/rt-test (rationalize .3+0.0i 1/10))


#;(define (test-rat-inf v)
  (local [(define zero (if (exact? v) 0 0.0))]

  (test +inf.0 rationalize +inf.0 v)
  (test -inf.0 rationalize -inf.0 v)
  (test-nan.0 rationalize +nan.0 v)

  (test zero rationalize v +inf.0)
  (test zero rationalize v -inf.0)
  (test-nan.0 rationalize v +nan.0)))

#;(let loop ([i 100])
  (unless (= i -100)
	  (test (/ i 100) rationalize (inexact->exact (/ i 100.0)) 1/100000)
	  (loop (sub1 i))))


#;(arity-test rationalize 2 2)

(define tb
  (lambda (n1 n2)
    (= n1 (+ (* n2 (quotient n1 n2))
	     (remainder n1 n2)))))




(test -2147483648 - (list 2147483648))
(test 2147483648 - (list -2147483648))
(test #f = (list -2147483648 2147483648))
(test #t = (list -2147483648 -2147483648))
(test #t = (list 2147483648 2147483648))
(test 2147483647 sub1 (list 2147483648))
(test 2147483648 add1 (list 2147483647))
(test 2147483648 * (list 1 2147483648))

(test 437893890380859375 expt (list 15 15))

(test 0 modulo (list -2177452800 86400))
(test 0 modulo (list 2177452800 -86400))
(test 0 modulo (list 2177452800 86400))
(test 0 modulo (list -2177452800 -86400))

(test 86399 modulo (list -2177452801 86400))
(test -1 modulo (list 2177452799 -86400))
(test 1 modulo (list 2177452801 86400))
(test -86399 modulo (list -2177452799 -86400))

(test #t 'remainder (list (tb 281474976710655 65535)))
(test #t 'remainder (list (tb 281474976710654 65535)))
(test 281474976710655 string->number (list "281474976710655"))
(test "281474976710655" number->string (list 281474976710655))
(skip (lambda () (test "-4" number->string (list -4 16))))
(skip (lambda () (test "-e" number->string (list -14 16))))
(skip (lambda () (test "0" number->string (list 0 16))))
;;(test "30000000" number->string (list #x30000000 16))


(test "0" number->string (list 0))
(test "100" number->string (list 100))
(skip (lambda () (test "100" number->string (list 256 16))))
(skip (lambda () (test 256 string->number (list "100" 16))))
(skip (lambda () (test 15 string->number (list "#o17"))))
(skip (lambda () (test 15 string->number (list "#o17" 10))))




(test #t = (list 0 0))
(test #f = (list 0 (expt 2 32)))
(test #f = (list (expt 2 32) 0))
(test #f = (list (- (expt 2 32)) (expt 2 32)))
(test #f = (list (expt 2 32) (- (expt 2 32))))
(test #t = (list 1234567890987654321 1234567890987654321))

(test #f < (list 0 0))
(test #t < (list 0 (expt 2 32)))
(test #f < (list (expt 2 32) 0))
(test #t < (list (- (expt 2 32)) 0))
(test #f < (list 0 (- (expt 2 32))))
(test #f < (list 1234567890987654321 1234567890987654321))
(test #t < (list (- (expt 3 64)) (- (expt 2 13))))
(test #f < (list (- (expt 2 13)) (- (expt 3 64))))
(test #t < (list (- 123456789876543200) 123456789876543200))
(test #f < (list 123456789876543200 (- 123456789876543200)))

(test 1234567890987654321 + (list 1234567890987654321 0))
(test 1234567890987654321 + (list 0 1234567890987654321))
(test 1234567890987654321 - (list 1234567890987654321 0))
(test -1234567890987654321 - (list 0 1234567890987654321))
(test (expt 2 33) + (list (expt 2 32) (expt 2 32)))
(test 0 - (list (expt 2 32) (expt 2 32)))
(test (expt 2 31) - (list (expt 2 32) (expt 2 31)))
(test (- (expt 2 31)) - (list (expt 2 31) (expt 2 32)))
(test 18446744073709551621 + (list 18446744073709551615 6))
(test 18446744073709551621 + (list 6 18446744073709551615))
;(test 0 - (list #xfffffffffffffffff #xfffffffffffffffff))
;(test -1 - (list #xffffffffffffffffe #xfffffffffffffffff))
;(test 1 - (list #xfffffffffffffffff #xffffffffffffffffe))
;(test #x1000000000000000000000000 + (list #xffffffffffffffffffffffff 1))


(test 0 * (list 1234567890987654321 0))
(test 0 * (list 0 1234567890987654321))
;(test #x100000000000000000000 * #x100000000000 #x1000000000)
;(test #x-100000000000000000000 * #x100000000000 #x-1000000000)
;(test #x100000000000000000000 * #x-100000000000 #x-1000000000)
;(test #x-100000000000000000000 * #x-100000000000 #x1000000000)
;(test #x100000000000000000000 * #x1000000000 #x100000000000)
;(test #x-100000000000000000000 * #x1000000000 #x-100000000000)
;(test #x100000000000000000000 * #x-1000000000 #x-100000000000)
;(test #x-100000000000000000000 * #x-1000000000 #x100000000000)
;(test 4521191813415169 * #x100000000001 #x101)
;(test 4521191813415169 * #x101 #x100000000001)

(test (expt 2 35) * (list (expt 2 32) 8))
(test (- (expt 2 35)) * (list (- (expt 2 32)) 8))
(test (- (expt 2 35)) * (list (expt 2 32) -8))
(test (expt 2 35) * (list (- (expt 2 32)) -8))
(test (- (add1 (expt 2 128)) (expt 2 65)) * (list (sub1 (expt 2 64)) (sub1 (expt 2 64))))

(test 4294967296 expt (list 2 32))
(test 3433683820292512484657849089281 expt (list 3 64))
(test 8192 expt (list 2 13))
(test 8589934592 expt (list 2 33))
(test 2147483648 expt (list 2 31))
(test 34359738368 expt (list 2 35))
(test 36893488147419103232 expt (list 2 65))
(test 18446744073709551616 expt (list 2 64))
(test 340282366920938463463374607431768211456 expt (list 2 128))
(test 340282366920938463463374607431768211456 expt (list -2 128))
(test 174449211009120179071170507 expt (list 3 55))
(test -174449211009120179071170507 expt (list -3 55))
(test 59768263894155949306790119265585619217025149412430681649 expt (list 7 66))
(test 1 expt (list 1234567890987654321 0))
(test 0 expt (list 0 1234567890987654321))
(test 1 expt (list 1 1234567890987654321))
(test 1234567890987654321 expt (list 1234567890987654321 1))
(test 828179745220145502584084235957368498016122811853894435464201864103254919330121223037770283296858019385573376 expt (list 953962166440690129601298432 4))


(test "0" number->string (list 0))
(test "1" number->string (list 1))
(test "-1" number->string (list -1))
(test "7284132478923046920834523467890234589203467590267382904573942345703" number->string (list 7284132478923046920834523467890234589203467590267382904573942345703))
(test "-7284132478923046920834523467890234589203467590267382904573942345703" number->string (list -7284132478923046920834523467890234589203467590267382904573942345703))
;(test "1000101001010101011111011000101001011011100111110001111111010101000100010110001001011011101111011001111101000100110100010101111001111001001011111001000100111000011111110010101010110110001011011111101110000001010111111100111" number->string 7284132478923046920834523467890234589203467590267382904573942345703 2)
;(test "-1000101001010101011111011000101001011011100111110001111111010101000100010110001001011011101111011001111101000100110100010101111001111001001011111001000100111000011111110010101010110110001011011111101110000001010111111100111" number->string -7284132478923046920834523467890234589203467590267382904573942345703 2)
;(test "105125373051334761772504261133573175046425717113710470376252661337560127747" number->string 7284132478923046920834523467890234589203467590267382904573942345703 8)
;(test "-105125373051334761772504261133573175046425717113710470376252661337560127747" number->string -7284132478923046920834523467890234589203467590267382904573942345703 8)
;(test "452abec52dcf8fea88b12ddecfa268af3c97c89c3f955b16fdc0afe7" number->string 7284132478923046920834523467890234589203467590267382904573942345703 16)
;(test "-452abec52dcf8fea88b12ddecfa268af3c97c89c3f955b16fdc0afe7" number->string -7284132478923046920834523467890234589203467590267382904573942345703 16)
;(test "115792089237316195423570985008687907853269984665640564039457584007913129639936" number->string (expt 2 256))
;(test "115792089237316195423570985008687907853269984665640564039457584007913129639935" number->string (sub1 (expt 2 256)))
;(test "10000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000" number->string (expt 2 256) 2)
;(test "1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111" number->string (sub1 (expt 2 256)) 2)
;(test "20000000000000000000000000000000000000000000000000000000000000000000000000000000000000" number->string (expt 2 256) 8)
;(test "17777777777777777777777777777777777777777777777777777777777777777777777777777777777777" number->string (sub1 (expt 2 256)) 8)
;(test "10000000000000000000000000000000000000000000000000000000000000000" number->string (expt 2 256) 16)
;(test "ffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff" number->string (sub1 (expt 2 256)) 16)
;(test "-115792089237316195423570985008687907853269984665640564039457584007913129639936" number->string (- (expt 2 256)))
;(test "-115792089237316195423570985008687907853269984665640564039457584007913129639935" number->string (- (sub1 (expt 2 256))))
;(test "-10000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000" number->string (- (expt 2 256)) 2)
;(test "-1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111" number->string (- (sub1 (expt 2 256))) 2)
;(test "-20000000000000000000000000000000000000000000000000000000000000000000000000000000000000" number->string (- (expt 2 256)) 8)
;(test "-17777777777777777777777777777777777777777777777777777777777777777777777777777777777777" number->string (- (sub1 (expt 2 256))) 8)
;(test "-10000000000000000000000000000000000000000000000000000000000000000" number->string (- (expt 2 256)) 16)
;(test "-ffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff" number->string (- (sub1 (expt 2 256))) 16)





(test 0 string->number (list "0"))
(test 1 string->number (list "1"))
(test -1 string->number (list "-1"))
(test 7284132478923046920834523467890234589203467590267382904573942345703 string->number (list "7284132478923046920834523467890234589203467590267382904573942345703"))
(test -7284132478923046920834523467890234589203467590267382904573942345703 string->number (list "-7284132478923046920834523467890234589203467590267382904573942345703"))
;(test 7284132478923046920834523467890234589203467590267382904573942345703 string->number "1000101001010101011111011000101001011011100111110001111111010101000100010110001001011011101111011001111101000100110100010101111001111001001011111001000100111000011111110010101010110110001011011111101110000001010111111100111" 2)
;(test -7284132478923046920834523467890234589203467590267382904573942345703 string->number "-1000101001010101011111011000101001011011100111110001111111010101000100010110001001011011101111011001111101000100110100010101111001111001001011111001000100111000011111110010101010110110001011011111101110000001010111111100111" 2)
;(test  7284132478923046920834523467890234589203467590267382904573942345703 string->number "105125373051334761772504261133573175046425717113710470376252661337560127747" 8)
;(test -7284132478923046920834523467890234589203467590267382904573942345703 string->number "-105125373051334761772504261133573175046425717113710470376252661337560127747" 8)
;(test 7284132478923046920834523467890234589203467590267382904573942345703 string->number "452abec52dcf8fea88b12ddecfa268af3c97c89c3f955b16fdc0afe7" 16)
;(test -7284132478923046920834523467890234589203467590267382904573942345703 string->number "-452abec52dcf8fea88b12ddecfa268af3c97c89c3f955b16fdc0afe7" 16)
;(test (expt 2 256) string->number "115792089237316195423570985008687907853269984665640564039457584007913129639936")
;(test (sub1 (expt 2 256)) string->number "115792089237316195423570985008687907853269984665640564039457584007913129639935")
;(test (expt 2 256) string->number "10000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000" 2)
;(test (sub1 (expt 2 256)) string->number "1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111" 2)
;(test (expt 2 256) string->number "20000000000000000000000000000000000000000000000000000000000000000000000000000000000000" 8)
;(test (sub1 (expt 2 256)) string->number "17777777777777777777777777777777777777777777777777777777777777777777777777777777777777" 8)
;(test (expt 2 256) string->number "10000000000000000000000000000000000000000000000000000000000000000" 16)
;(test (sub1 (expt 2 256)) string->number "ffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff" 16)
;(test (- (expt 2 256)) string->number "-115792089237316195423570985008687907853269984665640564039457584007913129639936")
;(test (- (sub1 (expt 2 256))) string->number "-115792089237316195423570985008687907853269984665640564039457584007913129639935")
;(test (- (expt 2 256)) string->number "-10000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000" 2)
;(test (- (sub1 (expt 2 256))) string->number "-1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111" 2)
;(test (- (expt 2 256)) string->number "-20000000000000000000000000000000000000000000000000000000000000000000000000000000000000" 8)
;(test (- (sub1 (expt 2 256))) string->number "-17777777777777777777777777777777777777777777777777777777777777777777777777777777777777" 8)
;(test (- (expt 2 256)) string->number "-10000000000000000000000000000000000000000000000000000000000000000" 16)
;(test (- (sub1 (expt 2 256))) string->number "-ffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff" 16)
;(test #f string->number "144r" 10)
;(err/rt-test (string->number "10" 30))


(define (q-test quotient)
  (begin
  (test 0 quotient (list 0 12345678909876532341))
  (test 0 quotient (list 0 -1235782934075829307))
  (test 2374865902374859023745 quotient (list 2374865902374859023745 1))
  (test -2374865902374859023745 quotient (list -2374865902374859023745 1))
  (test 0 quotient (list 1 13748910785903278450))
  (test 1 quotient (list 13748910785903278450 13748910785903278449))
  (test 0 quotient (list 13748910785903278450 13748910785903278451))
  (test -1 quotient (list -13748910785903278450 13748910785903278449))
  (test 0 quotient (list -13748910785903278450 13748910785903278451))
  (test -1 quotient (list 13748910785903278450 -13748910785903278449))
  (test 0 quotient (list 13748910785903278450 -13748910785903278451))
  (test 1 quotient (list -13748910785903278450 -13748910785903278449))
  (test 0 quotient (list -13748910785903278450 -13748910785903278451))
  (test 1 quotient (list 13748910785903278450 13748910785903278450))
  (test -1 quotient (list -13748910785903278450 13748910785903278450))
  (test -1 quotient (list 13748910785903278450 -13748910785903278450))
  (test 1 quotient (list -13748910785903278450 -13748910785903278450))
  (test (expt 5 64) quotient (list (expt 5 256) (expt 5 192)))
  (test 0 quotient (list (expt 5 192) (expt 5 256)))
  (test 8636168555094444625386351862800399571116000364436281385023703470168591803162427057971507503472288226560547293946149 quotient (list (expt 5 192) (expt 2 64)))))
(q-test quotient)
#;(q-test (lambda (n1 n2) (let-values ([(q r) (quotient/remainder n1 n2)]) q)))

(define (r-test remainder)
  (begin
  (test 0 remainder (list 0 12345678909876532341))
  (test 0 remainder (list 0 -1235782934075829307))
  (test 0 remainder (list 2374865902374859023745 1))
  (test 0 remainder (list -2374865902374859023745 1))
  (test 1 remainder (list 1 13748910785903278450))
  (test 1 remainder (list 13748910785903278450 13748910785903278449))
  (test 13748910785903278450 remainder (list 13748910785903278450 13748910785903278451))
  (test -1 remainder (list -13748910785903278450 13748910785903278449))
  (test -13748910785903278450 remainder (list -13748910785903278450 13748910785903278451))
  (test 1 remainder (list 13748910785903278450 -13748910785903278449))
  (test 13748910785903278450 remainder (list 13748910785903278450 -13748910785903278451))
  (test -1 remainder (list -13748910785903278450 -13748910785903278449))
  (test -13748910785903278450 remainder (list -13748910785903278450 -13748910785903278451))
  (test 0 remainder (list 13748910785903278450 13748910785903278450))
  (test 0 remainder (list -13748910785903278450 13748910785903278450))
  (test 0 remainder (list 13748910785903278450 -13748910785903278450))
  (test 0 remainder (list -13748910785903278450 -13748910785903278450))
  (test 0 remainder (list (expt 5 256) (expt 5 192)))
  (test (expt 5 192) remainder (list (expt 5 192) (expt 5 256)))
  (test 12241203936672963841 remainder (list (expt 5 192) (expt 2 64)))))
(r-test remainder)
;(r-test (lambda (n1 n2) (let-values ([(q r) (quotient/remainder n1 n2)]) r)))


(define (s-test sqrt)
  (begin
  (test 0 sqrt (list 0))
  (test 1 sqrt (list 1))
  (test 2 sqrt (list 4))
  (test 3 sqrt (list 9))
  (test (expt 2 64) sqrt (list (* (expt 2 64) (expt 2 64))))
  (test (expt 13 70) sqrt (list (* (expt 13 70) (expt 13 70))))
  (test (sub1 (expt 2 200)) sqrt (list (* (sub1 (expt 2 200)) (sub1 (expt 2 200)))))
  (test (expt 2 25) sqrt (list (expt 2 50)))
  (test 1 sqrt (list 3))
  (test #xffffffff sqrt (list (sub1 (expt 2 64))))
  (test 2876265888493261300027370452880859375 sqrt (list (expt 15 62)))
  (test #x8f0767e50d4d0c07563bd81f530d36 sqrt (list (expt 15 61)))))
(s-test integer-sqrt)
;(s-test (lambda (a) (let-values ([(root rem) (integer-sqrt/remainder a)]) root)))

(define (sr-test sqrt)
  (begin
  (test 0 sqrt (list 0))
  (test 0 sqrt (list 1))
  (test 0 sqrt (list 4))
  (test 0 sqrt (list 9))
  (test 0 sqrt (list (* (expt 2 64) (expt 2 64))))
  (test 0 sqrt (list (* (expt 13 70) (expt 13 70))))
  (test 0 sqrt (list (* (sub1 (expt 2 200)) (sub1 (expt 2 200)))))
  (test 0 sqrt (list (expt 2 50)))
  (test 2 sqrt (list 3))
  (test 8589934590 sqrt (list (sub1 (expt 2 64))))
  (test 0 sqrt (list (expt 15 62)))
  (test 1306106749204831357295958563982718571 sqrt (list (expt 15 61)))))
;(sr-test (lambda (a) (let-values ([(root rem) (integer-sqrt/remainder a)]) rem)))


(test 1.7320508075688772 sqrt (list 3))
(test 4294967296.0 sqrt (list (sub1 (expt 2 64))))
(test 2876265888493261300027370452880859375 sqrt (list (expt 15 62)))
(test 7.426486590265921e+35 sqrt (list (expt 15 61)))

(test 5.515270307539953e+71 exact->inexact (list (expt 15 61)))
(test -5.515270307539953e+71 exact->inexact (list (- (expt 15 61))))
(test 1.8446744073709552e+19 exact->inexact (list (expt 2 64)))
(test 1.157920892373162e+77 exact->inexact (list (expt 2 256)))
(test 1.157920892373162e+77 exact->inexact (list (sub1 (expt 2 256))))


(test 551527030753995340375346347667240734743269800540264151034260072897183744 inexact->exact (list 5.515270307539953d+71))
(test (expt 2 64) inexact->exact (list 1.8446744073709552e+19))
(test (- (expt 2 64)) inexact->exact (list -1.8446744073709552e+19))
(test (expt 2 256) inexact->exact (list 1.157920892373162d+77))
(test 115792089237316195423570985008687907853269984665640564039457584007913129639936 inexact->exact (list 1.157920892373162d+77))


;(test (integer-bytes->integer #"\1\2" #f) integer-bytes->integer #"\1\2" #f (system-big-endian?))









(js-big-bang #f
	     (on-draw 
	      (lambda (w)
                `(,(js-div)
                  
                  ,(list (js-text 
                          (format "~a tests run.  ~a tests broke.  ~a tests skipped."
                                  number-of-tests
                                  number-of-errors
                                  number-of-skipped-tests)))
                  ,@(map (lambda (msg)
                           (list (js-div)
                                 (list (js-text (format "~a" msg)))))
                         error-messages)))
	      (lambda (w)
		'())))

