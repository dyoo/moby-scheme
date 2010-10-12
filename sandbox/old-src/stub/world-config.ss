#lang scheme/base

(define-struct world-config () #:prefab)

(define-struct (world-config:on-key world-config) (f ef) #:prefab)
(define-struct (world-config:on-tick world-config) (delay f ef) #:prefab)
(define-struct (world-config:initial-effect world-config) (e) #:prefab)
(define-struct (world-config:on-location-change world-config) (f ef) #:prefab)
(define-struct (world-config:on-tilt world-config) (f ef) #:prefab)
(define-struct (world-config:on-acceleration world-config) (f ef) #:prefab)
(define-struct (world-config:on-shake world-config) (f ef) #:prefab)
(define-struct (world-config:on-redraw world-config) (f) #:prefab)
(define-struct (world-config:on-draw world-config) (f css-f) #:prefab)
(define-struct (world-config:stop-when world-config) (f) #:prefab)

(define (initial-effect e)
  (make-world-config:initial-effect e))

(define (on-key f)
  (make-world-config:on-key f (lambda (w k) '())))

(define (on-key! f ef)
  (make-world-config:on-key f ef))

(define (on-tick delay f)
  (make-world-config:on-tick delay f (lambda (w) '())))

(define (on-tick! delay f ef)
  (make-world-config:on-tick delay f ef))

(define (on-location-change f)
  (make-world-config:on-location-change f (lambda (w lat long) '())))

(define (on-location-change! f ef)
  (make-world-config:on-location-change f ef))

(define (on-tilt f)
  (make-world-config:on-tilt f (lambda (w azimuth pitch roll) '())))

(define (on-tilt! f ef)
  (make-world-config:on-tilt f ef))

(define (on-acceleration f)
  (make-world-config:on-acceleration f (lambda (w x y z) '())))

(define (on-acceleration! f ef)
  (make-world-config:on-acceleration f ef))

(define (on-shake f)
  (make-world-config:on-shake f (lambda (w) '())))

(define (on-shake! f ef)
  (make-world-config:on-shake f ef))

(define (on-redraw f)
  (make-world-config:on-redraw f))

(define (on-draw f css-f)
  (make-world-config:on-draw f css-f))

(define (stop-when f)
  (make-world-config:stop-when f))


(provide (all-defined-out))