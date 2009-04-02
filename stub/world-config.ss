#lang scheme/base

(define-struct world-config () #:prefab)

(define-struct (world-config:on-key world-config) (f) #:prefab)
(define-struct (world-config:on-tick world-config) (f) #:prefab)
(define-struct (world-config:on-mouse world-config) (f) #:prefab)
(define-struct (world-config:on-message world-config) (f) #:prefab)
(define-struct (world-config:on-location-change world-config) (f) #:prefab)
(define-struct (world-config:on-tilt world-config) (f) #:prefab)
(define-struct (world-config:on-acceleration world-config) (f) #:prefab)
(define-struct (world-config:on-redraw world-config) (f) #:prefab)
(define-struct (world-config:stop-when world-config) (f) #:prefab)


(provide (all-defined-out))