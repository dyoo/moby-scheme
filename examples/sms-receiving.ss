#lang s-exp "../moby-lang.ss"

;; The world is a string or false
(define initial-world #f)


;; update: world string string -> world
;; Get a new message.
(define (update w sender message)
  (string-append sender ": " message))


;; world->string: world -> string
;; Produces a string representation of the world.
(define (world->string w)
  (cond
    [(string? w)
     (string-append "Last seen SMS says: " w)]
    [else
     "No SMS message received recently"]))


;; draw: world -> DOM-sexp
;; Produces the DOM tree that we display.
(define (draw w)
  (list (js-p '(("id" "aPara")))
	(list (js-text (world->string w)))))


;; draw-css: world -> CSS-sexp
;; Style the dom so that the font size is large.
(define (draw-css w)
  '(("aPara" ("font-size" "30px"))))


;; Finally, begin a big-bang.
(js-big-bang initial-world
             '()
             (on-draw draw draw-css)
             (on-sms-receive update))
