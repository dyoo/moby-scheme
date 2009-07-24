;; The first three lines of this file were inserted by DrScheme. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname simple-location) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ())))
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
