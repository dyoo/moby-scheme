;; The first three lines of this file were inserted by DrScheme. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname pinholes) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ())))
(require (lib "world.ss" "moby" "stub"))
(define x 50)
(define y 50)

(define (draw-world a-world)
  #;(place-image (circle x "solid" "green")
               100 
               y
               (empty-scene 100 100))
  
  (place-image (circle x "solid" "red") 
               x 
               y
               (place-image (circle x "solid" "green")
                            100 
                            y
                            (empty-scene 320 480))))


(big-bang 320 480 false
          (on-redraw draw-world))