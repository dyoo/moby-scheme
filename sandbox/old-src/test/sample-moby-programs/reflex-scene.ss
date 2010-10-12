;; The first three lines of this file were inserted by DrScheme. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname reflex-scene) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ())))

(define react-time 50)
(define wait-time 500)
(define blink-time 10)

(define-struct world (time-remaining score))

(define init-world (make-world 0 0))

(define (key-handler world key)
  (if (key=? key 'up)
      (if (<= (world-time-remaining world) react-time)
          ; got it!
          (make-world (+ react-time (random wait-time)) (+ (world-score world) 1))
          ; too early!
          (make-world (+ react-time (random wait-time)) (- (world-score world) 1)))
      world))

(define (tick-handler world)
  (if (<= (world-time-remaining world) 0)
      ; missed it!
      (make-world (+ react-time (random wait-time)) (world-score world))
      ; counting
      (make-world (- (world-time-remaining world) 1) (world-score world))))

(define (background world)
  (if (and (<= (world-time-remaining world) react-time)
                  (> (world-time-remaining world) (- react-time blink-time)))
             (rectangle 100 100 'solid 'red)
             (rectangle 100 100 'solid 'gray)))

(define (score world)
  (text (number->string (world-score world)) 12 'black))

(define (redraw world)
  (place-image (score world) 0 0
               (place-image (background world) 0 0
                            (empty-scene 400 300))))

;; RUN PROGRAM
(big-bang 400 300 init-world
          (on-tick 0.01 tick-handler)
          (on-key key-handler)
          (on-redraw redraw))
