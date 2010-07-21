#lang s-exp "../moby-lang.ss"

;; Simple falling ball example.  A red ball falls down the screen
;; until hitting the bottom.



;; The dimensions of the screen:
(define WIDTH 320)
(define HEIGHT 480)

;; The radius of the red circle.
(define RADIUS 15)

;; The world is the distance from the top of the screen.
(define INITIAL-WORLD 0)

;; tick: world -> world
;; Moves the ball down.
(define (tick w)
  (+ w 5))

;; hits-floor?: world -> boolean
;; Returns true when the distance reaches the screen height.
(define (hits-floor? w)
  (>= w HEIGHT))

;; We have some simple test cases.
(check-expect (hits-floor? 0) false)
(check-expect (hits-floor? HEIGHT) true)

;; render: world -> scene
;; Produces a scene with the circle at a height described by the world.
(define (render w)
  (place-image (circle RADIUS "solid" "red") (/ WIDTH 2) w
               (empty-scene WIDTH HEIGHT)))

;; Start up a big bang, 15 frames a second.
(js-big-bang INITIAL-WORLD
             (on-tick tick 1/15)
             (on-redraw render)
             (stop-when hits-floor?))