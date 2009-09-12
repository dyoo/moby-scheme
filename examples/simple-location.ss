#lang s-exp "../moby-lang.ss"

;; The world is a latitude and longitude.
(define-struct world (lat long))

(define initial-world (make-world 0 0))


;; update: world number number -> world
;; Update the world to the latest latitude and longitude reading.
(define (update w lat long)
  (make-world lat long))


;; world->string: world -> string
;; Produces a string representation of the world.
(define (world->string w)
  (string-append "("
		 (number->string (world-lat w))
		 ", "
		 (number->string (world-long w))
		 ")"))


;; draw: world -> DOM-sexp
;; Produces the DOM tree that we display.
(define (draw w)
  (list (js-p '(("id" "aPara")))
        (list (js-text "(latitude, longitude) = "))
	(list (js-text (world->string w)))))


;; draw-css: world -> CSS-sexp
;; Style the dom so that the font size is large.
(define (draw-css w)
  '(("aPara" ("font-size" "30px"))))


;; Finally, begin a big-bang.
(js-big-bang initial-world
             '()
             (on-draw draw draw-css)
             (on-location-change update))
