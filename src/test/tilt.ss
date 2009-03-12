;; The first three lines of this file were inserted by DrScheme. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname tilt) (read-case-sensitive #t) (teachpacks ((lib "world.ss" "teachpack" "htdp"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "world.ss" "teachpack" "htdp")))))
(require (lib "world.ss" "moby" "stub"))
(require (lib "tilt.ss" "moby" "stub"))


(define width 300)
(define height 100)

(define-struct world (x y z))
(define initial-world (make-world 0 0 0))


(define (render-world a-world)
  (place-image
   (text (string-append (number->string (world-x a-world))
                        " "
                        (number->string (world-y a-world))
                        " "
                        (number->string (world-z a-world)))
         20 
         "blue")
   0
   50
   (empty-scene width height)))

(define (handle-tilt-change a-world new-x new-y new-z)
  (make-world new-x new-y new-z))

(big-bang width height 1/10 initial-world)
(on-redraw render-world)
(on-tilt-change-event handle-tilt-change)