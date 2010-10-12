#lang moby

;; The world consists of a number.

;; We have two images, a plus and a minus image, that we'll plop onto our buttons.
(define PLUS (js-img "http://boredzo.org/buttonicons/plus-8.png"))
(define MINUS (js-img "http://boredzo.org/buttonicons/minus-8.png"))

;; plus-press: world -> world
(define (plus-press w)
  (add1 w))

;; minus-press: world -> world
(define (minus-press w)
  (sub1 w))

;; draw: world -> dom-sexp
(define (draw w)
  (list (js-div '((id "main")))
        (list (js-text (format "World contains: ~a" w)))
        (list (js-button plus-press)
              (list PLUS))
        (list (js-button minus-press)
              (list MINUS))))

;; draw-css: world -> css-sexp
;; Let all text drawn in the main div have a font size of 30px.
(define (draw-css w)
  '(("main" ("font-size" "30px"))))

(js-big-bang 0
             (on-draw draw draw-css))