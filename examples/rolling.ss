#lang s-exp "../moby-lang.ss"

(require jsworld/phonegap)

;; Rolling ball. A world is a posn.

(define WIDTH 300)
(define HEIGHT 460)
(define RADIUS 50)

;; The initial world starts at the center.
(define initial-world (make-posn (quotient WIDTH 2) (quotient HEIGHT 2)))

;; tilt: world number number number -> world
;; Adjusts velocity based on the tilt.
(define (tilt w azimuth pitch roll)
  (make-posn (floor (clamp (+ roll (posn-x w)) 0 WIDTH))
	     (floor (clamp (- (posn-y w) pitch) 0 HEIGHT))))

;; clamp: number number number -> number
;; Clamps a number x between a and b.
(define (clamp x a b)
  (cond [(> x b) b]
        [(< x a) a]
        [else x]))
  
;; render: world -> scene
;; Renders the world.
(define (render w)
  (place-image (circle RADIUS "solid" "blue")
	       (posn-x w)
	       (posn-y w)
	       (empty-scene WIDTH HEIGHT)))

(js-big-bang initial-world
             (on-tilt tilt)
             (on-redraw render))