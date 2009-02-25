;; The first three lines of this file were inserted by DrScheme. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname location) (read-case-sensitive #t) (teachpacks ((lib "world.ss" "teachpack" "htdp"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "world.ss" "teachpack" "htdp")))))
(require (lib "location.ss" "moby"))

(define width 300)
(define height 100)

(define-struct world (latitude longitude))

(define initial-world (make-world 0 0))


(define (render-world a-world)
  (place-image
   (text (number->string (world-longitude a-world)) 20 "blue")
   0
   50
   (place-image
    (text (number->string (world-latitude a-world)) 20 "red")
    0
    0
    (empty-scene width height))))


(define (handle-location-message a-world a-message)
  (make-world (first a-message)
              (second a-message)))


(big-bang width height 1/10 initial-world)
(on-redraw render-world)
(on-message-event handle-location-message)