;; The first three lines of this file were inserted by DrScheme. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname location) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ())))

(define width 320)
(define height 480)

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


(define (tick a-world)
  (make-world (get-latitude)
              (get-longitude)))


(big-bang width height initial-world
          (on-redraw render-world)
          (on-tick 1 tick))