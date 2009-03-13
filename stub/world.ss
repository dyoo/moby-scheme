#lang scheme
(require htdp/world
         lang/prim)

(provide (all-from-out htdp/world))


(define (on-location-change-event handler)
  (void))

(define (on-tilt-change-event handler)
  (void))

(define (on-acceleration-change-event handler)
  (void))


(provide-higher-order-primitive on-location-change-event (handler))

(provide-higher-order-primitive on-tilt-change-event (handler))
(provide-higher-order-primitive on-acceleration-change-event (handler))

;; FIXME: changes to location or tilt should reflect on the world.