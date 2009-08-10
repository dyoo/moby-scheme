;; The first three lines of this file were inserted by DrScheme. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname falling-ball) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ())))

;; Simple falling ball example.  Red ball falls down.

;; A world is a number representing the y position of the red ball.

(define WIDTH 320)
(define HEIGHT 480)
(define RADIUS 5)

(define (tick y)
  (+ y 5))

(define (hits-floor? y)
  (>= y (- HEIGHT RADIUS)))

(check-expect (hits-floor? 0) false)
(check-expect (hits-floor? HEIGHT) true)

(define (draw-scene y)
  (place-image (circle RADIUS "solid" "red") (/ WIDTH 2) y
               (empty-scene WIDTH HEIGHT)))

(big-bang WIDTH HEIGHT 
          10
          (on-tick 1/15 tick)
          (on-redraw draw-scene)
          (stop-when hits-floor?))