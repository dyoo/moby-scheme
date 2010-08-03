#lang s-exp "../moby-lang.ss"

(require jsworld/phonegap)


(define (move w lat lng)
  (list lat lng))


(js-big-bang (void) (on-location-change move))