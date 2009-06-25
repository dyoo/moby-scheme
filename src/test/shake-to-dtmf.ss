;; The first three lines of this file were inserted by DrScheme. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname shake-to-dtmf) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ())))
;; DTMF ringing program for Moby.
(require (lib "moby.ss" "moby" "stub"))



(define WIDTH 320)
(define HEIGHT 480)

;; The world is a number counting how many times we've been shaken, 
;; and the current tone.
(define-struct world (shaken tone))

(define initial-world (make-world 0 4))



(define button-up (js-button ""))
(define button-down (js-button ""))



;; update: world -> world
(define (update a-world)
  (make-world (add1 (world-shaken a-world))
              (world-tone a-world)))


;; ring: world -> effect
(define (ring a-world)
  (make-effect:play-dtmf-tone (world-tone a-world) 1000))
  

;; render: world -> (sexpof dom)
(define (render w)
  (list (js-div)
        (list (js-text (number->string (world-shaken w))))
        (list (js-text "rings"))
        (list button-up)
        (list button-down)))
  

;; render-css: world -> (sexpof css-style)
(define (render-css w)
  '())


(js-big-bang initial-world
             '()
             (on-draw render render-css)
             (on-shake* update ring))
