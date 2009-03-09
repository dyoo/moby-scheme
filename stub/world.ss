#lang scheme
(require htdp/world
         lang/prim)

(provide (all-from-out htdp/world))


(define (on-location-change-event handler)
  (void))


(provide-higher-order-primitive on-location-change-event (handler))