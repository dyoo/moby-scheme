#lang s-exp "../moby-lang.ss"

(require jsworld/phonegap)


(define (tilt w azimuth pitch roll)
  (list azimuth pitch roll))

(js-big-bang #f (on-tilt tilt))
