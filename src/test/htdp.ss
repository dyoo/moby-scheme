#lang s-exp "../moby-lang.ss"
(include "test-harness.ss")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 2.1.1
(test 169 sqr (list 13))
(test 0 sin (list 0))
(test 1/3 max (list -1 1/3))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 2.1.2
(test 2 sqrt (list 4))
(test 1.4142135623730951 sqrt (list 2))
(test 0+1i sqrt (list -1))
(test 0 tan (list 0))
(test -1.2246467991473532e-16 tan (list pi))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 2.2.1
(define (fahrenheit->celsius t)
  (* 5/9 (- t 32)))

(test 0 fahrenheit->celsius (list 32))
(test 100 fahrenheit->celsius (list 212))
(test -40 fahrenheit->celsius (list -40))




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 2.2.2
(define (dollar->euro d)
  (* 1.17 d))
(test 1.17 dollar->euro (list 1))
(test 2.34 dollar->euro (list 2))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 2.2.3
(define (triangle base height)
  (* 1/2 base height))
(test 25 triangle (list 10 5))
(test 5 triangle (list 2 5))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 2.2.4
(define (convert3 ones tens hundreds)
  (+ ones
     (* 10 tens)
     (* 100 hundreds)))
(test 321 convert3 (list 1 2 3))
(test 935 convert3 (list 5 3 9))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
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
