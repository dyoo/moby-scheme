;; The first three lines of this file were inserted by DrScheme. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname marble) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ())))
;; Marble rolling program.

(define WIDTH 320)
(define HEIGHT 480)

(define marble (js-div '(("id" "marble"))))


;; A velocity has an x and y component.
(define-struct vel (x y))

;; A world is a posn and a vel.
(define-struct world (posn vel))

;; The initial world has the marble centered
;; with no velocity.
(define initial-world 
  (make-world (make-posn (quotient WIDTH 2)
                         (quotient HEIGHT 2))
              (make-vel 0 0)))

;; tick: world -> world
;; Every tick moves the ball by its velocity.
(define (tick w)
  (make-world (posn+vel (world-posn w) (world-vel w))
              (world-vel w)))

;; tilt: world number number number -> world
;; Adjusts velocity based on the tilt.
(define (tilt w azimuth pitch roll)
  (make-world (world-posn w)
              (make-vel roll (- pitch))))


;; draw: world -> dom-sexp
;; We draw a single marble on screen.
(define (draw w)
  (list marble))

;; marble-styling: posn -> (listof css-style)
(define (marble-styling a-posn)
  (list 
   (list "background-color" "blue")
   (list "position" "absolute")
   (list "top" (string-append 
                (number->string
                 (posn-x (world-posn w)))
                "px")
         (list "left" (string-append
                       (number->string
                        (posn-y (world-posn w)))
                       "px")))))

;; draw-css: world -> css-sexp
;; The marble is styled to be at a position and
;; a certain color.
(define (draw-css w)
  (list (cons "marble" (marble-styling
                        (world-posn w)))))
              

;; posn+vel: posn velocity -> posn
(define (posn+vel a-posn a-vel)
  (make-posn (clamp (+ (posn-x a-posn) (vel-x a-vel))
                    0 WIDTH)
             (clamp (+ (posn-y a-posn) (vel-y a-vel))
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