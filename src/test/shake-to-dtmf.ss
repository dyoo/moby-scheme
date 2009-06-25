;; The first three lines of this file were inserted by DrScheme. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname shake-to-dtmf) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ())))
;; DTMF ringing program for Moby.
(require (lib "moby.ss" "moby" "stub"))



(define WIDTH 320)
(define HEIGHT 480)
(define MAX-TONE 15)
(define MIN-TONE 0)


;; The world is a number counting how many times we've been shaken, 
;; and the current tone.
(define-struct world (shaken tone))

(define initial-world (make-world 0 4))



;; update: world -> world
(define (update a-world)
  (make-world (add1 (world-shaken a-world))
              (world-tone a-world)))


;; ring: world -> effect
(define (ring a-world)
  (make-effect:play-dtmf-tone (world-tone a-world) 1000))

;; up: world -> world
(define (up a-world)
  (make-world (world-shaken a-world)
              (min (add1 (world-tone a-world))
                   MAX-TONE)))

;; down: world -> world
(define (down a-world)
  (make-world (world-shaken a-world)
              (max (sub1 (world-tone a-world))
                   MIN-TONE)))



(define button-up (js-button up))
(define button-down (js-button down))

;; render: world -> (sexpof dom)
(define (render w)
  (list (js-div)
        (list (js-div) 
              (list (js-text (number->string (world-shaken w))))
              (list (js-text " "))
              (list (js-text "rings")))
        (list (js-div) 
              (list (js-text "note is: "))
              (list (js-text (number->string (world-tone w)))))
        (list button-up (list (js-text "Up")))
        (list button-down (list (js-text "Down")))))
  

;; render-css: world -> (sexpof css-style)
(define (render-css w)
  '())


(js-big-bang initial-world
             '()
             (on-draw render render-css)
             (on-shake* update ring))