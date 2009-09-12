#lang scheme
(require lang/prim
         scheme/gui/base)

(define a-frame (new frame%
                     [label "Location Stub"]))

(define t-x (new text-field% 
               [parent a-frame]
                [label "x tilt"]
                [init-value "0"]))

(define t-y (new text-field% 
                [parent a-frame]
                [label "y tilt"]
                [init-value "0"]))

(define t-z (new text-field% 
                [parent a-frame]
                [label "z tilt"]
                [init-value "0"]))
(send a-frame show #t)


(define ((get-tilt a-text-field))
  (cond
    [(string->number (send a-text-field get-value))
     => (lambda (x) x)]
    [else
     0]))


(define get-x-tilt (get-tilt t-x))
(define get-y-tilt (get-tilt t-y))
(define get-z-tilt (get-tilt t-z))


(provide-primitives get-x-tilt
                    get-y-tilt
                    get-z-tilt)