#lang s-exp "../moby-lang.ss"

;; Small program to test setting of ringer volume.

;; The world doesn't contain anything interesting.
(define initial-world #f)



(define user-interface
  (list (js-p '(("id" "aPara")))
	(list (js-button! (lambda (w) w)
                          (lambda (w) (make-effect:beep)))
	      (list (js-text "Beep")))
	(list (js-button! (lambda (w) w)
                          (lambda (w) (make-effect:set-beep-volume 0)))
	      (list (js-text "Mute beep volume")))
	(list (js-button! (lambda (w) w)
                          (lambda (w) (make-effect:set-beep-volume 75)))
	      (list (js-text "Set beep volume to 75%")))))


;; draw: world -> DOM-sexp
;; The interface includes a button for beeping, a button
;; for muting the ringer, and a button for setting the
;; ringer to 75%.
(define (draw w) user-interface)


;; draw-css: world -> CSS-sexp
;; Style the dom so that the font size is large.
(define (draw-css w)
  '(("aPara" ("font-size" "30px"))))


;; Finally, begin a big-bang.
(js-big-bang initial-world
             '()
             (on-draw draw draw-css))
