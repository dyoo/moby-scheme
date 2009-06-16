;; The first three lines of this file were inserted by DrScheme. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname shake-to-dtmf) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ())))
(require (lib "world.ss" "moby" "stub"))

(define WIDTH 320)
(define HEIGHT 480)

;; The world is a number counting how many times we've been shaken.
(define initial-world 0)

;; update: world -> world
(define (update a-world)
  (add1 a-world))


;; ring: world -> effect
(define (ring a-world)
  (make-effect:play-dtmf-tone 4 1000))
  

;; render: world -> scene
(define (render w)
  (place-image (text (string-append (number->string w)
                                    " rings")
                     10
                     "black")
               50
               50
               (empty-scene WIDTH HEIGHT)))



(big-bang WIDTH HEIGHT initial-world
          (on-redraw render)
          (on-shake* update ring))
