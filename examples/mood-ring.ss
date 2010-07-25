#lang s-exp "../moby-lang.ss"

;; Mood ring: change color based on orientation

(require jsworld/phonegap)

;; The world is a color.
(define initial-world (make-color 0 0 0))

;; tilt: world number number number -> world
;; Tilting the phone adjusts the color.
(define (tilt w azimuth pitch roll)
  (make-color (scale azimuth 360)
	      (scale (+ pitch 90) 180)
	      (scale (+ roll 90) 180)))

;; scale-azimuth: number -> number
;; Take a number going from 0-360 and scale it to a number between 0-255
(define (scale n domain-bound)
  (inexact->exact (floor (* (/ n domain-bound) 255))))

;; User interface.
(define view (list (js-div '((id "background")))))

(define (draw-html w) view)

(define (draw-css w)
  (list (list "background" 
	      (list "background-color" 
		    (format "rgb(~a, ~a, ~a)"
			    (color-red w)
			    (color-green w)
			    (color-blue w)))
	      (list "width" "100%")
	      (list "height" "100%"))))



(js-big-bang initial-world
	     (on-tilt tilt)
	     (on-draw draw-html draw-css))