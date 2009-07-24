;; The first three lines of this file were inserted by DrScheme. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname marble-with-trail) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ())))
;; Marble rolling program, with a trail.

(define WIDTH 320)
(define HEIGHT 480)


;; We'll have a trail of a certain length.
(define TRAIL-LENGTH 5)


(define background (js-div '(("id" "backgroundDiv"))))


;; A velocity has an x and y component.
(define-struct vel (x y))

;; A marble consists of its div element and its posn.
(define-struct marble (div posn))

;; A world is a posn, a (listof marble), and a vel.
(define-struct world (posn trail vel))


;; make-initial-trail: number -> (listof marble)
;; Creates the initial trail of marbles.
(define (make-initial-trail n)
  (cond
    [(= n 0)
     empty]
    [else
     (cons (make-marble (js-div)
                        (make-posn (quotient WIDTH 2)
                                   (quotient HEIGHT 2)))
           (make-initial-trail (sub1 n)))]))


;; The initial world has the marbles centered.
(define initial-world 
  (make-world (make-posn (quotient WIDTH 2)
                         (quotient HEIGHT 2))
              (make-initial-trail TRAIL-LENGTH)             
              (make-vel 0 0)))


;; tick: world -> world
;; Every tick moves the ball by its velocity.
(define (tick w)
  (make-world (posn+vel (world-posn w) (world-vel w))
              (add-posn-to-trail (posn+vel (world-posn w) 
                                           (world-vel w))
                                 (world-trail w))
              (world-vel w)))


;; add-posn-to-trail: posn (listof marble) -> (listof marble)
;; Adds a posn to the end of the trail.
(define (add-posn-to-trail a-posn a-trail)
  (append (rest a-trail)
          (list (make-marble (js-div) a-posn))))


;; tilt: world number number number -> world
;; Adjusts velocity based on the tilt.
(define (tilt w azimuth pitch roll)
  (make-world (world-posn w)
              (world-trail w)
              (make-vel roll (- pitch))))


;; draw: world -> dom-sexp
;; We draw the background and the marbles.
(define (draw w)
  (cons background
        (draw-trail (world-trail w))))


;; draw-trail: trail -> (listof dom-sexp)
;; Produces the dom trees for the trail.
(define (draw-trail a-trail)
  (cond
    [(empty? a-trail)
     empty]
    [else
     (cons (list (marble-div (first a-trail)))
           (draw-trail (rest a-trail)))]))


;; draw-css: world -> css-sexp
;; Style both the trail and the background.
(define (draw-css w)
  (cons (list "backgroundDiv"
              (list "background-color" "white")
              (list "width" 
                    (number->px WIDTH))
              (list "height"
                    (number->px HEIGHT)))
        (draw-trail-css (world-trail w) 0)))


;; draw-trail-css: trail number -> (listof css-style)
;; Produces the styling for the trail of marbles.
;; Each marble is styled slightly differently based on
;; i.
(define (draw-trail-css a-trail i)
  (cond
    [(empty? a-trail)
     empty]
    [else
     (cons (cons (marble-div (first a-trail))
                 (marble-styling 
                  (marble-posn (first a-trail))
                  i))
           (draw-trail-css (rest a-trail) (add1 i)))]))

;; marble-styling: posn number -> (listof css-style)
;; Styles the ith marble.  As 'i' gets larger, the
;; marble gets larger and more red.
(define (marble-styling a-posn i)
  (list 
   (list "background-color" "red")
   (list "position" "absolute")
   (list "width" (number->px (* (add1 i) 5)))
   (list "height" (number->px (* (add1 i) 5)))
   (list "top" (number->px (posn-y a-posn)))
   (list "left" (number->px (posn-x a-posn)))))

;; number->px: number -> string
;; Turns a number into a px string for css.
(define (number->px a-num)
  (string-append (number->string a-num)
                 "px"))
                    


;; posn+vel: posn velocity -> posn
;; Adds a posn to a velocity.
(define (posn+vel a-posn a-vel)
  (make-posn (clamp (+ (posn-x a-posn) 
                       (vel-x a-vel))
                    0 WIDTH)
             (clamp (+ (posn-y a-posn) 
                       (vel-y a-vel))
                    0 HEIGHT)))

;; clamp: number number number -> number
;; Clamps a number x between a and b.
(define (clamp x a b)
  (cond [(> x b) b]
        [(< x a) a]
        [else x]))


(js-big-bang initial-world
             '()
             (on-draw draw draw-css)
             (on-tick 1/20 tick)
             (on-tilt tilt))