#lang s-exp "../../moby-lang.ss"
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


(js-big-bang false
             (on-redraw draw-world))