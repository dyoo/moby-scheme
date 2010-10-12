;; The first three lines of this file were inserted by DrScheme. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname simple-button) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ())))
(require (lib "moby.ss" "moby" "stub"))

;; button example.  Pressing the button will send an alert and adjust the button's text.

;; The world is a number.


;; on-press: world -> world
(define (on-press w)
  (add1 w))


;; ring: world -> effect
(define (ring w)
  (make-effect:beep))



;; draw: world -> (sexpof dom)
(define (draw w)
  (local [(define a-button (js-button* on-press ring
                                       (list (list "id" "aButton"))))
	  (define a-para (js-p (list (list "id" "aPara"))))
          (define a-button-text (js-text (number->string w) (list (list "id" "aText"))))]
    (list a-button (list a-para (list a-button-text)))))



;; draw-css: world -> (sexpof css)
(define (draw-css w)
  (list (list "aButton"
              (list "background-color" "lightblue"))
        (list "aPara"
              (list "font-size" "30pt"))))


(js-big-bang 0
             '()
             (on-draw draw draw-css))