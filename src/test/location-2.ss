;; Location, using the on-location-changed hook.
(require (lib "location.ss" "moby" "stub"))

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


(define (handle-location-change a-world a-latitude a-longitude)
  (make-world a-latitude
              a-longitude))


(big-bang width height 1/10 initial-world)
(on-redraw render-world)
(on-location-change-event handle-location-change)