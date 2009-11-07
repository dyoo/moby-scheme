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
;; 3.3.2
(local [(define (volume-cylinder radius height)
  (* (area-circle radius) height))
(define (area-circle radius)
  (* radius radius pi))]
(begin
(test pi area-circle (list 1))
(test (* 18 pi) volume-cylinder (list 3 2))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; 3.3.3
(local[
(define (area-cylinder radius height)
  (+ (* (circumference-circle radius) height)
     (area-circle radius)
     (area-circle radius)))

;; area-circle : number -> number
;; to determine the area of a circle
(define (area-circle radius)
  (* pi radius radius))

;; circumference-circle : number -> number
;; to deteremine the circumference of a circle
(define (circumference-circle radius)
  (* radius 2 pi))]

;; EXAMPLES AS TESTS
(begin
(test (* 4 pi) circumference-circle (list 2))
(test (* pi 9) area-circle (list 3))
(test (* 20 pi) area-cylinder (list 2 3))
(test (* 42 pi) area-cylinder (list 3 4))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 3.3.4

(local [
;; area-pipe-one-def : number number number -> number
;; to determine the area of a pipe with given inner radius, length, and thickness
;; this version does not use any helper functions
(define (area-pipe-one-def inner len thickness)
  (+ (* 2 (- (* pi (+ inner thickness) (+ inner thickness))
             (* pi inner inner)))
     (* len (* 2 pi (+ inner thickness)))
     (* len (* 2 pi inner))))

;; area-pipe : number number number -> number
;; to determine the area of a pipe with given inner radius, length, and thickness
(define (area-pipe inner len thickness)
  (+ (* 2 (area-donut inner (+ inner thickness)))  ; s.a. of rings on ends
     (* len (circumference (+ inner thickness)))   ; s.a. of outer cylinder
     (* len (circumference inner))))               ; s.a. of inner cylinder

;; area-donut : number number -> number
;; finds the area of a circle with a chunk missing
;; the entire circle has radius outer and the missing
;; middle portion has radius inner.
(define (area-donut inner outer)
  (- (area-circle outer)
     (area-circle inner)))

;; area-circle : number -> number
;; determines the area of a circle with given radius
(define (area-circle r)
  (* pi r r))

;; circumference : number -> number
;; determines the circumference of a circle with given radius
(define (circumference r)
  (* 2 pi r))]

;; EXAMPLES AS TESTS
(begin (test (* 9 pi) area-circle (list 3))
(test (* 6 pi) circumference (list 3))
(test (* 16 pi) area-donut (list 3 5))
(test (* 112 pi) area-pipe (list 2 3 4))
(test (* 112 pi) area-pipe-one-def (list 2 3 4))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 3.3.5
(local[
;; height : number number -> number
;; to determine the height a rocket reaches in a time t.
(define (height g t)
  (height-from-speed t (speed-from-time g t)))

;; height-from-speed : number number -> number
;; to determine the height a rocket reaches from its speed and time
(define (height-from-speed t v)
  (* 1/2 v t))

;; speed-from-time : number number -> number
;; to determine the speed a rocket reaches in time t
(define (speed-from-time g t)
  (* g t))]

;; EXAMPLES AS TESTS
(begin 
(test 10 speed-from-time (list 10 1))
(test 25 height-from-speed (list 10 5))
(test 500 height (list 10 10))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 3.3.6

(local[
(define (fahrenheit->celsius t)
  (* 5/9 (- t 32)))

;; celsius->fahrenheit : number -> number
;; computes the farenheit equivalent of t
(define (celsius->fahrenheit t)
  (+ (* 9/5 t) 32))

;; I : number -> number
;; to convert a Fahrenheight temperature to Celsuis and back
(define (I f)
  (celsius->fahrenheit (fahrenheit->celsius f)))]

;; EXAMPLES AS TESTS
(test 32 I (list 32)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 4.1.1
(test true (lambda (x y) (and x y)) (list (> 4 3) (<= 10 100)))
(test true (lambda (x y) (or x y)) (list (> 4 3) (= 10 100)))
(test true not (list (= 2 3)))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 4.1.2
(test true > (list 4 3))
(test false > (list 2 3))
(test true > (list 7/2 3))
(test false (lambda (x y) (and x y)) (list (> 4 3) (> 3 3)))
(test false (lambda (x y) (and x y)) (list (> 4 2) (> 2 3)))
(test true (lambda (x y) (and x y)) (list (> 4 7/2) (> 7/2 3)))
(test false = (list (* 4 4) 4))
(test false = (list (* 2 2) 2))
(test false = (list (* 7/2 7/2) 7/2))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 4.2.1

(local [(define (interval-1 n)
  (and (> n 3)
       (<= n 7)))]

(begin
(test false interval-1 (list -1))
(test false interval-1 (list 3))
(test true interval-1 (list 5))
(test true interval-1 (list 7))
(test false interval-1 (list 10))))


(local [(define (interval-2 n)
  (and (>= n 3)
       (<= n 7)))]
(begin
(test false interval-2 (list -1))
(test true interval-2 (list 3))
(test true interval-2 (list 5))
(test true interval-2 (list 7))
(test false interval-2 (list 10))))


(local [(define (interval-3 n)
  (and (>= n 3)
       (< n 9)))]
(begin
(test false interval-3 (list -1))
(test true interval-3 (list 3))
(test true interval-3 (list 5))
(test false interval-3 (list 9))
(test false interval-3 (list 10))))


(local [(define (interval-4 n)
  (or (and (> n 1)
           (< n 3))
      (and (> n 9)
           (< n 11))))]
(begin
(test false interval-4 (list 1))
(test true interval-4 (list 2))
(test false interval-4 (list 3))
(test false interval-4 (list 5))
(test false interval-4 (list 9))
(test true interval-4 (list 10))
(test false interval-4 (list 11))
(test false interval-4 (list 50))))

(local [(define (interval-5 n)
  (or (< n 1)
      (> n 3)))]
(begin
(test true interval-5 (list -100))
(test false interval-5 (list 1))
(test false interval-5 (list 3))
(test true interval-5 (list 100))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 4.2.2

(local [

(define (in-interval1? x)
  (and (< -3 x) (< x 0)))

(define (in-interval2? x)
  (or (< x 1) (> x 2)))

(define (in-interval3? x)
  (not (and (<= 1 x) (<= x 5))))]
(begin

(test false in-interval1? (list -4))
(test false in-interval1? (list -3))
(test false in-interval1? (list 0))
(test false in-interval1? (list 1))
(test true in-interval1? (list -2))
(test true in-interval1? (list -1))

(test true in-interval2? (list 0))
(test true in-interval2? (list 3))
(test false in-interval2? (list 1))
(test false in-interval2? (list 2))
(test false in-interval2? (list 3/2))

(test false in-interval3? (list 1))
(test false in-interval3? (list 3))
(test false in-interval3? (list 5))
(test true in-interval3? (list 0))
(test true in-interval3? (list 6))
))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 4.2.3
(local [(define (equation1 n)
  (= (+ (* 4 n) 2)
     62))

;; equation2 : number -> boolean
;; determines if n satisfies equation #2
(define (equation2 n)
  (= (* 2 (* n n))
     102))

;; equation3 : number -> boolean
;; determines if n satisfies equation #3
(define (equation3 n)
  (= (+ (* 4 (* n n)) (* 6 n) 2)
     462))]
(begin
(test false equation1 (list 10))
(test false equation1 (list 12))
(test false equation1 (list 14))
(test false equation2 (list 10))
(test false equation2 (list 12))
(test false equation2 (list 14))
(test true equation3 (list 10))
(test false equation3 (list 12))
(test false equation3 (list 14))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 4.2.4
(local[
(define (f n)
  (= (+ (* 4 n) 2) 62))]
(begin
(test false f (list 10))
(test false f (list 12))
(test false f (list 14))))

(local [
(define (g n)
  (= (* 2 n n) 102))]
(begin
(test false g (list 10))
(test false g (list 12))
(test false g (list 14))))

(local [
(define (h n)
  (= (+ (* 4 n n) (* 6 n) 2) 462))]
(begin
(test true h (list 10))
(test false h (list 12))
(test false h (list 14))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 4.3.3
(local[(define n1 500)]

(test 40 'cond (list (cond
  [(<= n1 1000) (* .040 1000)]
  [(<= n1 5000) (+ (* 1000 .040) 
                  (* (- n1 1000) .045))]
  [else (+ (* 1000 .040) 
           (* 4000 .045)
           (* (- n1 10000) .055))]))))


(local[(define n2 2800)]
(test 121 'cond (list (cond
  [(<= n2 1000) (* .040 1000)]
  [(<= n2 5000) (+ (* 1000 .040) 
                  (* (- n2 1000) .045))]
  [else (+ (* 1000 .040) 
           (* 4000 .045)
           (* (- n2 10000) .055))]))))

(local [
(define n3 15000)]
(test 495 'cond (list
(cond
  [(<= n3 1000) (* .040 1000)]
  [(<= n3 5000) (+ (* 1000 .040) 
                  (* (- n3 1000) .045))]
  [else (+ (* 1000 .040) 
           (* 4000 .045)
           (* (- n3 10000) .055))]))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 4.4.1
(local[
(define (interest d)
  (cond
    [(<= d 1000) (* d 4/100)]
    [(<= d 5000) (* d 45/1000)]
    [(> d 5000) (* d 5/100)]))]
(begin

(test 20 interest (list 500))
(test 90 interest (list 2000))
(test 500 interest (list 10000))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 4.4.2
(local [
(define (tax gross-pay)
  (cond
    [(<= gross-pay 240) 0]
    [(<= gross-pay 480) (* gross-pay .15)]
    [else (* gross-pay .28)]))

;; netpay : number -> number
;; to determine the amount of income, after taxes.
(define (netpay hours-worked)
  (- (* hours-worked 12) 
     (tax (* hours-worked 12))))
]

(begin
(test 0 tax (list 10))
(test 0 tax (list 240))
(test 45 tax (list 300))
(test 72 tax (list 480))
(test 140 tax (list 500))

(test 12 netpay (list 1))
(test 408 netpay (list 40))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 4.4.3

(local [
(define (pay-back a)
  (cond
    [(<= a 500)
     (pay-back-0-500 a)]
    [(and (> a 500) (<= a 1500))
     (pay-back-500-1500 a)]
    [(and (> a 1500) (<= a 2500))
     (pay-back-1500-2500 a)]
    [else
     (pay-back-2500+ a)]))

;; pay-back-0-500 : number -> number
;; computes the pay back for amounts between 0 and 500
(define (pay-back-0-500 a)
  (* a (* .25 1/100)))

;; pay-back-500-1500 : number -> number
;; computes the pay back for amounts between 500 and 1500
(define (pay-back-500-1500 a)
  (+ (pay-back-0-500 500)
     (* (- a 500) (* .50 1/100))))

;; pay-back-1500-2500 : number -> number
;; computes the pay back for amounts between 1500 and 2500
(define (pay-back-1500-2500 a)
  (+ (pay-back-500-1500 1500)
     (* (- a 1500) (* .75 1/100))))

;; pay-back-2500+ : number -> number
;; computes the pay back for amounts between 2500 and higher
(define (pay-back-2500+ a)
  (+ (pay-back-1500-2500 2500)
     (* (- a 2500) (* 1 1/100))))
]
(begin

(test 1 pay-back-0-500 (list 400))
(test 1 pay-back (list 400))

(test 5.75 pay-back-500-1500 (list 1400))
(test 5.75 pay-back (list 1400))

(test 10.00 pay-back-1500-2500 (list 2000))
(test 10.00 pay-back (list 2000))

(test 14.75 pay-back-2500+ (list 2600))
(test 14.75 pay-back (list 2600))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 4.4.4

(local [
(define (how-many a b c)
  (cond
    [(> (discriminant a b c) 0) 2]
    [(= (discriminant a b c) 0) 1]
    [(< (discriminant a b c) 0) 0]))

(define (discriminant a b c)
  (- (* b b) (* 4 a c)))]
(begin

(test 0 discriminant (list 1 2 1))
(test 8 discriminant (list 2 4 1))
(test -8 discriminant (list 2 4 3))

(test 1 how-many (list 1 2 1))
(test 2 how-many (list 2 4 1))
(test 0 how-many (list 2 4 3))
(test 2 how-many (list 1 0 -1))
(test 1 how-many (list 2 4 2))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 5.1.1


(local [(define (reply s)
  (cond
    [(symbol=? s 'GoodMorning) 'Hi]
    [(symbol=? s 'HowAreYou?) 'Fine]
    [(symbol=? s 'GoodAfternoon) 'INeedANap]
    [(symbol=? s 'GoodEvening) 'BoyAmITired]))]

(begin

(test 'Fine reply (list 'HowAreYou?))
(test 'Hi reply (list 'GoodMorning))
(test 'INeedANap reply (list 'GoodAfternoon))
(test 'BoyAmITired reply (list 'GoodEvening))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 5.1.2
(local [
(define (check-guess guess target)
  (cond
    ((= guess target) 'Perfect)
    ((< guess target) 'TooSmall)
    ((> guess target) 'TooLarge)))
]

(begin
(test 'Perfect check-guess (list 1 1))
(test 'TooSmall check-guess (list 1 2))
(test 'TooLarge check-guess (list 1 0))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 5.1.3

(local [
(define (check-guess3 ones tens hundreds target)
  (check-guess (digits ones tens hundreds) target))

(define (digits ones tens hundreds)
  (+ ones
     (* tens 10)
     (* hundreds 100)))
(define (check-guess guess target)
  (cond
    [(= guess target) 'Perfect]
    [(< guess target) 'TooSmall]
    [(> guess target) 'TooLarge]))]
(begin


(test 'Perfect check-guess (list 1 1))
(test 'TooSmall check-guess (list 1 2))
(test 'TooLarge check-guess (list 1 0))

(test 321 digits (list 1 2 3))

(test 'TooSmall check-guess3 (list 1 2 3 500))
(test 'TooLarge check-guess3 (list 1 2 3 100))
(test 'Perfect check-guess3 (list 1 2 3 321))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 5.1.4
(local [
(define (how-many a b c)
  (cond
    [(= a 0) 'degenerate]
    [(> (discriminant a b c) 0) 'two]
    [(= (discriminant a b c) 0) 'one]
    [(< (discriminant a b c) 0) 'none]))

;; discriminant : number number number -> number
;; computes the discriminant of the quadratic equation with
;; coefficients a, b, and c.
(define (discriminant a b c)
  (- (* b b) (* 4 a c)))]
(begin

(test 0 discriminant (list 1 2 1))
(test 8 discriminant (list 2 4 1))
(test -8 discriminant (list 2 4 3))

(test 'one how-many (list 1 2 1))
(test 'two how-many (list 2 4 1))
(test 'none how-many (list 2 4 3))
(test 'two how-many (list 1 0 -1))
(test 'one how-many (list 2 4 2))
(test 'degenerate how-many (list 0 1 1))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 5.1.5
(local [
(define (check-color target-1 target-2 guess-1 guess-2)
  (cond
    [(and (symbol=? guess-1 target-1) (symbol=? guess-2 target-2))
     'perfect!]
    [(or  (symbol=? guess-1 target-1) (symbol=? guess-2 target-2)) 
     'One_Color_At_Correct_Position]
    [(or  (symbol=? guess-1 target-2) (symbol=? guess-2 target-1))
     'The_Colors_Occur]
    [else
     'Nothing_Correct]))]
(begin

(test 'perfect! check-color (list 'red 'green 'red 'green) )
(test 'One_Color_At_Correct_Position check-color (list 'red 'green 'red 'purple))
(test 'The_Colors_Occur check-color (list 'red 'green 'purple 'red))
(test 'The_Colors_Occur check-color (list 'green 'red 'red 'purple))
(test 'Nothing_Correct check-color (list 'green 'blue 'red 'purple))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 6.1.1

(local [
(define (distance-to-0 a-posn)
  (sqrt (+ (sqr (posn-x a-posn))
           (sqr (posn-y a-posn)))))]

(begin
(test 5 distance-to-0 (list (make-posn 3 4)))
(test 10 distance-to-0 (list (make-posn (* 2 3) (* 2 4))))
(test 13 distance-to-0 (list (make-posn 12 (- 6 1))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 6.2.1

;; SKIPPED: we don't have the right primitives


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 6.3.2
(local [(define-struct movie (title producer))]
(begin

(test 'ThePhantomMenace movie-title (list (make-movie 'ThePhantomMenace 'Lucas)))
(test 'Lucas movie-producer (list (make-movie 'TheEmpireStrikesBack 'Lucas)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 6.3.3
(local [
(define-struct jet-fighter (designation acceleration top-speed range))

(define (within-range jf d)
  (<= d (jet-fighter-range jf)))

;; reduce-range : jet-fighter -> jet-fighter
;; to return a jet-fighter whose range is 80% of jf's.
(define (reduce-range jf)
  (make-jet-fighter
   (jet-fighter-designation jf)
   (jet-fighter-acceleration jf)
   (jet-fighter-top-speed jf)
   (* .8 (jet-fighter-range jf))))]

(begin

(test true within-range (list (make-jet-fighter 'f22 4 1000 600) 400))
(test false within-range (list (make-jet-fighter 'mig22 10 800 300) 400))

(test (make-jet-fighter 'f22 4 1000 480) reduce-range (list (make-jet-fighter 'f22 4 1000 600)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 6.5.1
(local [
(define-struct time (hours minutes seconds))

(define (time->seconds t)
  (+ (time-seconds t)
     (* 60 (time-minutes t))
     (* 60 60 (time-hours t))))]

(begin
(test 0 time->seconds (list (make-time 0 0 0)))
(test 1 time->seconds (list (make-time 0 0 1)))
(test 60 time->seconds (list (make-time 0 1 0)))
(test 3600 time->seconds (list (make-time 1 0 0)))
(test 3782 time->seconds (list (make-time 1 3 2)))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 6.6.8 + 6.6.10
(local [
(define-struct rectangle (nw-corner width height color))

(define example-rectangle1 (make-rectangle (make-posn 20 20) 260 260 'red))
(define example-rectangle2 (make-rectangle (make-posn 60 60) 180 180 'blue))
(define (in-rectangle? a-rectangle a-posn)
  (and (<= (posn-x (rectangle-nw-corner a-rectangle))
           (posn-x a-posn)
           (+ (posn-x (rectangle-nw-corner a-rectangle))
              (rectangle-width a-rectangle)))
       (<= (posn-y (rectangle-nw-corner a-rectangle))
           (posn-y a-posn)
           (+ (posn-y (rectangle-nw-corner a-rectangle))
              (rectangle-height a-rectangle)))))
(define (translate-rectangle a-rectangle x)
  (make-rectangle (make-posn
                   (+ x (posn-x (rectangle-nw-corner a-rectangle)))
                   (posn-y (rectangle-nw-corner a-rectangle)))
                  (rectangle-width a-rectangle)
                  (rectangle-height a-rectangle)
                  (rectangle-color a-rectangle)))

]
(begin
(test false in-rectangle? (list example-rectangle1 (make-posn 0 0)))
(test false in-rectangle? (list example-rectangle1 (make-posn 25 0)))
(test false in-rectangle? (list example-rectangle1 (make-posn 0 25)))
(test true in-rectangle? (list example-rectangle1 (make-posn 25 25)))

(test (make-rectangle (make-posn 50 20) 260 260 'red) translate-rectangle (list example-rectangle1 30))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 6.7.3
(local [

(define-struct word (first second third))

(define (reveal chosen status guess)
  (make-word (reveal1 (word-first chosen) (word-first status) guess)
             (reveal1 (word-second chosen) (word-second status) guess)
             (reveal1 (word-third chosen) (word-third status) guess)))

(define (reveal1 ch st gu) 
  (cond
    ((eq? ch st) ch)
    ((eq? ch gu) gu)
    (else st)))
]
(begin
(test 'a  reveal1 (list 'a 'a 'x))
(test 'x reveal1 (list 'x '_ 'x))
(test '_ reveal1 (list 'x '_ 'y))

(test (make-word 'd'_ '_) reveal (list (make-word 'd 'e 'r) (make-word '_ '_ '_) 'd))
(test (make-word '_ '_ '_) reveal (list (make-word 'd 'e 'r) (make-word '_ '_ '_) 'f))
))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 7.1.1
(local [
(define-struct employee (last first dob ssn))
]
(begin
(test false number? (list (make-posn 2 3)))
(test true number? (list (+ 12 10)))
(test false posn? (list 23))
(test true posn? (list (make-posn 23 3)))
(test false employee? (list (make-posn 23 23)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 7.1.2
(local [
(define-struct square (nw-corner length))
(define-struct circle (center radius))

;; perimeter : shape -> number
;; to compute the perimeter of a-shape
(define (perimeter a-shape)
  (cond
    [(square? a-shape) (* (square-length a-shape) 4)]
    [(circle? a-shape) (* (* 2 (circle-radius a-shape)) pi)]))
]
(begin
(test 16 perimeter (list (make-square (make-posn 10 10) 4)))
(test (* 40 pi) perimeter (list (make-circle (make-posn 30 30) 20)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(local [

(define-struct square (nw-corner length))
(define-struct circle (center radius))

;; area : shape -> number
;; computes the area of a-shape

#|
;; Template
(define (area a-shape)
  (cond
    [(square? a-shape) ...
     (square-nw-corner a-shape) ...
     (square-length a-shape) ...]
    [(circle? a-shape) ...
     (circle-center a-shape) ...
     (circle-radius a-shape) ...]))
|#

(define (area a-shape)
  (cond
    [(square? a-shape)
     (* (square-length a-shape)
        (square-length a-shape))]
    [(circle? a-shape) 
     (* pi
        (circle-radius a-shape)
        (circle-radius a-shape))]))
]

(begin

(test 16 area (list (make-square (make-posn 10 10) 4)))
(test (* 4 pi) area (list (make-circle (make-posn 10 10) 2)))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 7.1.3 
(local [
(define-struct spider (legs space))
(define-struct elephant (space))
(define-struct monkey (intelligence space))
]
(void))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 7.3.1
(local [


(define-struct square (nw-corner length))
(define-struct circle (center radius))
(define-struct rectangle (nw-corner width height))

(define (perimeter a-shape)
  (cond
    [(square? a-shape)
     (* (square-length a-shape)
        (square-length a-shape))]
    [(circle? a-shape) 
     (* pi
        (circle-radius a-shape)
        (circle-radius a-shape))]
    [(rectangle? a-shape) 
     (* (rectangle-width a-shape)
        (rectangle-height a-shape))]))
]
(begin
(test 16 perimeter (list (make-square (make-posn 10 10) 4)))
(test (* 4 pi) perimeter (list (make-circle (make-posn 10 10) 2)))
(test 8 perimeter (list (make-rectangle (make-posn 10 10) 2 4)))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 7.4.4
(local [

(define-struct circle (color center radius))
(define-struct rectangle (color nw-corner width height))

(define (translate-shape a-shape delta)
  (cond
    [(circle? a-shape) 
     (make-circle      
      (circle-color a-shape)
      (make-posn (+ delta (posn-x (circle-center a-shape)))
                 (posn-y (circle-center a-shape)))
      (circle-radius a-shape))]
    [(rectangle? a-shape)
     (make-rectangle
      (rectangle-color a-shape)
      (make-posn (+ delta (posn-x (rectangle-nw-corner a-shape)))
                 (posn-y (rectangle-nw-corner a-shape)))
      (rectangle-width a-shape)
      (rectangle-height a-shape))]))
]
(begin
(test (make-circle 'red (make-posn 40 30) 20) translate-shape (list (make-circle 'red (make-posn 30 30) 20) 10))
(test (make-rectangle 'blue (make-posn 50 60) 20 50) translate-shape (list (make-rectangle 'blue (make-posn 30 60) 20 50) 20))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 8.3.2
(local [
(define (f x y)
  (+ (* 3 x) (* y y)))
]
(begin
(test 14 '8.3.2 (list (+ (f 1 2) (f 2 1))))
(test 39 f (list 1 (* 2 3)))
(test 478 f (list (f 1 (* 2 3)) 19))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 9.1.3
(local [

(define (add-up-3 a-lo3n)
  (+ (first a-lo3n)
     (first (rest a-lo3n))
     (first (rest (rest a-lo3n)))))


(define (distance-to-0-for-3 a-lo3n)
  (sqrt (+ (sqr (first a-lo3n))
           (sqr (first (rest a-lo3n)))
           (sqr (first (rest (rest a-lo3n)))))))]

(begin
(test 3 add-up-3 (list (cons 1 (cons 1 (cons 1 empty)))))
(test 0 add-up-3 (list (cons -1 (cons 2 (cons -1 empty)))))


(test (sqrt 3) distance-to-0-for-3 (list (cons 1 (cons 1 (cons 1 empty)))))
(test (sqrt 6) distance-to-0-for-3 (list (cons -1 (cons 2 (cons -1 empty)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 9.1.4
(local [

(define (contains-2-doll? a-lo2s)
  (or (eq? 'doll (first a-lo2s))
      (eq? 'doll (first (rest a-lo2s)))))
]

(begin
(test true contains-2-doll? (list (cons 'doll (cons 'rocket empty))))
(test true contains-2-doll? (list (cons 'candy (cons 'doll empty))))
(test false contains-2-doll? (list (cons 'candy (cons 'rocket empty))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 9.3.1
(local [
(define (contains-doll? a-list-of-symbols)
  (cond
    [(empty? a-list-of-symbols) false]
    [else
     (cond
       [(eq? (first a-list-of-symbols) 'doll) true]
       [else (contains-doll? (rest a-list-of-symbols))])]))
]
(begin
(test false contains-doll? (list empty))
(test false contains-doll? (list (cons 'ball empty)))
(test true contains-doll? (list (cons 'arrow (cons 'doll empty))))
(test false contains-doll? (list (cons 'bow (cons 'arrow (cons 'ball empty)))))
(test true contains-doll? (list (cons 'make-up-set
                      (cons 'clown
                            (cons 'arrow
                                  (cons 'doll
                                        (cons 'ball
                                              empty)))))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 9.5.1
(local [
(define (sum a-list-of-nums)
  (cond
    [(empty? a-list-of-nums) 0]
    [else (+ (first a-list-of-nums)
             (sum (rest a-list-of-nums)))]))
]
(begin
(test 0 sum (list empty))
(test 1 sum (list (cons 1.00 empty)))
(test 20.86 sum (list (cons 17.05 (cons 1.22 (cons 2.59 empty)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 9.5.2
(local [

;; how-many-symbols: list-of-symbol -> number
;; Determins how many symbols are in a-lon

;; Template
#|
(define (how-many-symbols a-los)
  (cond
   [(empty? a-los) ...]
   [else ... (first a-los) ... (how-many-symbols (rest a-lon) ...)]))
|#

;; 
(define (how-many-symbols a-los)
  (cond
   [(empty? a-los) 0]
   [else (add1 (how-many-symbols (rest a-los)))]))
]
(begin
(test 0 how-many-symbols (list empty))
(test 1 how-many-symbols (list (cons 'apple empty)))
(test 3 how-many-symbols (list (cons 'dandelion (cons 'mashed (cons 'potatoes empty)))))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 9.5.3
(local[

(define (dollar-store? a-lon)
  (cond
   [(empty? a-lon) true]
   [else (and (<= (first a-lon) 1.00)
              (dollar-store? (rest a-lon)))]))]
(begin
(test true dollar-store? (list empty))
(test false dollar-store? (list (cons .75 (cons 1.95 (cons .25 empty)))))
(test true dollar-store? (list (cons .75 (cons 0.95 (cons .25 empty)))))))

(local [
(define (N-dollar-store? a-lon an-n)
  (cond
    [(empty? a-lon) true]
    [else (and (<= (first a-lon) an-n)
               (N-dollar-store? (rest a-lon) an-n))]))
]
(begin
(test true N-dollar-store? (list empty 1.00))
(test true N-dollar-store? (list (cons 1.50 empty) 2.00))
(test false N-dollar-store? (list (cons 3.00 empty) 2.00))
(test false N-dollar-store? (list (cons .75 (cons 1.95 (cons .25 empty))) 1.50))
(test true N-dollar-store? (list (cons .75 (cons 0.95 (cons .25 empty))) 1.50))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;







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


