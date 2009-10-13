;; The first three lines of this file were inserted by DrScheme. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname tilt) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ())))


(define width 320)
(define height 480)

(define-struct world (a p r))
(define initial-world (make-world 0 0 0))


(define (render-world a-world)
  (place-image
   (text (string-append (number->string (world-a a-world))
                        " "
                        (number->string (world-p a-world))
                        " "
                        (number->string (world-r a-world)))
         20 
         "blue")
   0
   50
   (empty-scene width height)))

(define (handle-orientation-change a-world new-a new-p new-r)
  (make-world new-a new-p new-r))

(big-bang width height initial-world
          (on-redraw render-world)
          (on-tilt handle-orientation-change))