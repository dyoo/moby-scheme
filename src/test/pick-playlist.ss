#lang s-exp "../moby-lang.ss"

(define (update-playlist w playlist)
  playlist)

(js-big-bang false
             (initial-effect (make-effect:pick-playlist update-playlist)))