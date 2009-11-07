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
;; 2.2.5
(local [(define (f n)
  (+ (/ n 3) 2))]
(begin
(test 8/3 f (list 2))
(test 11/3 f (list 5))
(test 5 f (list 9))))

(local [(define (f n)
  (+ (* n n) 10))]
(begin
(test 14 f (list 2))
(test 91 f (list 9))))

(local [(define (f n)
  (+ (* 1/2 n n) 20))]
(begin (test 22 f (list 2))
(test 60.5 f (list 9))))

(local [(define (f n)
  (- 2 (/ 1 n)))]
(begin (test 1.5 f (list 2))
(test 17/9 f (list 9))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 2.3.1
(define (tax w)
  (* 0.15 w))
(define (netpay h)
  (- (wage h)
     (tax (wage h))))
(define (wage h)
  (* h 12))
(test 15 tax (list 100))
(test 24 wage (list 2))
(test 408 netpay (list 40))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 2.3.2

(define (sum-coins p n d q)
  (+ (* p 1)
     (* n 5)
     (* d 10)
     (* q 25)))
(test 1 sum-coins (list 1 0 0 0))
(test 5 sum-coins (list 0 1 0 0))
(test 10 sum-coins (list 0 0 1 0))
(test 25 sum-coins (list 0 0 0 1))
(test 41 sum-coins (list 1 1 1 1))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 2.3.3
(define (total-profit customers)
  (- (* 5 customers) (+ 20 (* .50 customers))))
(test 2.50 total-profit (list 5))
(test -11.00 total-profit (list 2))
(test 430.00 total-profit (list 100))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 2.4.1

;; SKIPPED: I don't have with-handlers form to automatically check
;; the exceptions.  Do this by hand.

;; 2.4.2

;; SKIPPED as well.  Doing it by hand.

;; 2.4.3
;; SKIPPED as well.  Doing it by hand.



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 3.1.1
(define (attendance price)
  (+ (* (/ 15 .10) (- 5.00 price))
     120))
(test 120 attendance (list 5.0))
(test 135 attendance (list 4.9))
(test 270 attendance (list 4))
(test 420 attendance (list 3))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 3.1.3
(local [
(define (profit-left ticket-price) 
  (- (revenue ticket-price) 
     (cost ticket-price))) 

(define (revenue ticket-price) 
  (*  (attendees ticket-price) ticket-price)) 

(define (cost ticket-price) 
  (+ 180  
     (* .04 (attendees ticket-price)))) 

(define (attendees ticket-price) 
  (+ 120 
     (* (/ 15 .10) (- 5.00 ticket-price)))) 


;; How not to design a program  
(define (profit-right price) 
  (- (* (+ 120 
           (* (/ 15 .10) 
              (- 5.00 price))) 
        price) 
     (+ 180  
        (* .04 
           (+ 120 
              (* (/ 15 .10) 
                 (- 5.00 price))))))) ]
(begin (test (profit-right 3.00) profit-left (list 3.00))
(test (profit-right 4.00) profit-left (list 4.00))
(test (profit-right 5.00) profit-left (list 5.00))))




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 3.1.4
(local [
(define (profit-left ticket-price)
  (- (revenue ticket-price)
     (cost ticket-price)))

;; revenue : number -> number
;; determines the revenue for a particular ticket price.
(define (revenue ticket-price)
  (*  (attendees ticket-price) ticket-price))

;; cost : number -> number
;; determines the cost for a particular ticket price.
(define (cost ticket-price)
  (* 1.50 (attendees ticket-price)))

;; attendees : number -> number
;; determines the number of attendees for a particular ticket price.
(define (attendees ticket-price)
  (+ 120
     (* (/ 15 .10) (- 5.00 ticket-price))))


;; profit-right : number -> number
;; determines the profit of the program
(define (profit-right ticket-price)
  (- (* (+ 120
           (* (/ 15 .10)
              (- 5.00 ticket-price)))
        ticket-price)
     (* 1.50 (+ 120
                (* (/ 15 .10)
                   (- 5.00 ticket-price))))))]
(begin (test (profit-right 3.00) profit-left (list 3.00))
(test (profit-right 4.00) profit-left (list 4.00))
(test (profit-right 5.00) profit-left (list 5.00))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 3.2.1
(local [
(define COST-BASE 180)
(define COST-FACTOR 0.04)
(define ATTENDANCE-BASE 120)
(define ATTENDANCE-FACTOR (/ 15 .10))
(define ATTENDANCE-CAP 5.00)

;; profit : number[price] -> number[profit]
;; to compute how much money the movie makes if it charges price/ticket
;; Examples: 
;; if the price is 5.0, the movie makes 415.20
;; if the price is 4.9, the movie makes 476.10
(define (profit ticket-price)
  (- (revenue ticket-price)
     (cost ticket-price)))

;; revenue : number[price] -> number[revenue]
;; to compute how much revenue are generated by attendees at some given price
;; Examples:
;; at 5.00, we get 120 attendees, and produce a revenue of 5.0 * 120 = 600.0
;; at 4.90, we get 135 attendees, and produce a revenue of 4.9 * 135 = 661.5
(define (revenue ticket-price)
  (* (attendees ticket-price) ticket-price))

;; cost : number[price] -> number[cost]
;; to compute how much it costs to accommodate attendees
;; Examples:
;; if the price is 5.00, the cost is $184.80
;; if the price is 4.90, the cost is $185.40
(define (cost ticket-price)
  (+ COST-BASE
     (* COST-FACTOR (attendees ticket-price))))

;; attendees : number[price] -> number[people]
;; to compute how many attendees we get at some price 
;; Examples:
;; at 5.0, we get 120
;; at 4.9, we get 135
(define (attendees ticket-price)
  (+ ATTENDANCE-BASE
     (* ATTENDANCE-FACTOR (- ATTENDANCE-CAP ticket-price))))]
(begin 
  (test 120 attendees (list 5.00))
  (test 135 attendees (list 4.90))
  
  (test 184.80 cost (list 5.00))
  (test 185.40 cost (list 4.90))
  
  (test 600 revenue (list 5.00))
  (test 661.50 revenue (list 4.90))
  
  (test 415.20 profit (list 5.00))
  (test 476.10 profit (list 4.90))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 3.3.1
;; CONSTANTS
(local[
(define cm-per-inch 2.54)
(define inches-per-foot 12)
(define feet-per-yard 3)
(define yards-per-rod (+ 5 1/2))
(define rods-per-furlong 40)
(define furlongs-per-mile 8)
  
;; inches->cm : number -> number
;; to determine the number of cm in some number of inches
(define (inches->cm inches)
  (* inches cm-per-inch))

;; feet->inches : number -> number
;; to determine the number of inches in some number of feet
(define (feet->inches feet)
  (* feet inches-per-foot))

;; yards->feet : number -> number
;; to determine the number of feet in some number of yards
(define (yards->feet yards)
  (* yards feet-per-yard))

;; rods->yards : number -> number
;; to determine the number of yards in some number of rods
(define (rods->yards rods)
  (* rods yards-per-rod))

;; furlongs->rods : number -> number
;; to determine the number of rods in some number of furlongs
(define (furlongs->rods furlongs)
  (* furlongs rods-per-furlong))

;; miles->furlongs : number -> number
;; to determine the number of furlongs in some number of miles
(define (miles->furlongs miles)
  (* miles furlongs-per-mile))

;; feet->cm : number -> number
;; to determine the number of cm in some number of feet
(define (feet->cm feet)
  (inches->cm (feet->inches feet)))

;; yards->cm : number -> number
;; to determine the number of cm in some number of yards
(define (yards->cm yards)
  (feet->cm (yards->feet yards)))

;; rods->inches : number -> number
;; to determine the number of inches in some number of rods
(define (rods->inches rods)
  (feet->inches (yards->feet (rods->yards rods))))

;; miles->feet : number -> number
;; to determine the number of feet in some number of miles
(define (miles->feet miles)
  (yards->feet 
   (rods->yards
    (furlongs->rods
     (miles->furlongs 
      miles)))))]

(begin
(test 2.54 inches->cm (list 1))
(test 7.62 inches->cm (list 3))

(test 12 feet->inches (list 1))
(test 18 feet->inches (list 3/2))

(test 3 yards->feet (list 1))
(test 36 yards->feet (list 12))

(test 198 rods->inches (list 1))
(test 297 rods->inches (list 3/2))

(test 40 furlongs->rods (list 1))
(test 280 furlongs->rods (list 7))

(test 8 miles->furlongs (list 1))
(test 80 miles->furlongs (list 10))

(test 30.48 feet->cm (list 1))
(test 121.92 feet->cm (list 4))

(test 91.44 yards->cm (list 1))
(test 182.88 yards->cm (list 2))

(test 198 rods->inches (list 1))
(test 693 rods->inches (list 3.5))

(test 5280 miles->feet (list 1))
(test 21120 miles->feet (list 4))))












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


