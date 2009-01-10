;; The first three lines of this file were inserted by DrScheme. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname falling-ball) (read-case-sensitive #t) (teachpacks ((lib "world.ss" "teachpack" "htdp"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "world.ss" "teachpack" "htdp")))))
;; Simple falling ball example.  Red ball falls down.

;; A world is a number representing the y position of the red ball.

(define WIDTH 100)
(define HEIGHT 100)
(define RADIUS 5)

(define (tick y)
  (+ y 5))

(define (hits-floor? y)
  (>= y HEIGHT))

(check-expect (hits-floor? 0) false)
(check-expect (hits-floor? HEIGHT) true)

(define (draw-scene y)
  (place-image (circle RADIUS "solid" "red") (/ WIDTH 2) y
               (empty-scene WIDTH HEIGHT)))

(big-bang WIDTH HEIGHT 1/15 0)
(on-tick-event tick)
(on-redraw draw-scene)
(stop-when hits-floor?)