;; The first three lines of this file were inserted by DrScheme. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname bubble-3) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ())))


(define WIDTH 320)
(define HEIGHT 480)

(define target-radius 20)

;; A velocity has an x and y component.
(define-struct vel (x y))

(define-struct world (posn radius target-posn vel))
(define initial-world (make-world (make-posn (quotient WIDTH 2)
                                             (quotient HEIGHT 2))
                                  30
                                  (make-posn (quotient WIDTH 2)
                                             (quotient HEIGHT 2))
                                  (make-vel 0 0)))


;; randomize-target-posn: world -> world
(define (randomize-target-posn a-world)
  (rerandomize-target-posn-if-out-of-range
   (make-world (world-posn a-world)
               (world-radius a-world)
               (make-posn (random WIDTH)
                          (random HEIGHT))
               (world-vel a-world))))


;; rerandomize-target-posn-if-out-of-range: world -> world
(define (rerandomize-target-posn-if-out-of-range a-world)
  (cond
    [(<= (distance (make-posn (quotient WIDTH 2)
                              (quotient HEIGHT 2))
                   (world-target-posn a-world))
         (quotient WIDTH 3))
     a-world]
    [else
     (randomize-target-posn a-world)]))
         


;; world-deflate-bubble: world -> world
(define (world-deflate-bubble a-world)
  (cond
    [(> (world-radius a-world) 1)
     (make-world (world-posn a-world)
                 (- (world-radius a-world) 1/2)
                 (world-target-posn a-world)
                 (world-vel a-world))]
    [else
     a-world]))
         

(define (tick a-world)
  (world-reset-if-collide
   (world-deflate-bubble 
    (move a-world))))

;; move: world -> world
(define (move a-world)
  (make-world (posn+vel (world-posn a-world) (world-vel a-world))
              (world-radius a-world)
              (world-target-posn a-world)
              (world-vel a-world)))

;; render-world: world -> scene
(define (render-world a-world)
  (cond [(game-end? a-world)
         (place-image/posn (text "Game Over!" 10 "black")
                           (make-posn 20 20)
                           (place-image/posn (circle 5 "solid" "red")
                                             (world-target-posn a-world)
                                             (place-image/posn (circle (world-radius a-world) "solid" "blue")
                                                               (world-posn a-world)
                                                               (empty-scene WIDTH HEIGHT))))]
        [else
         (place-image/posn (circle target-radius "solid" "red")
                           (world-target-posn a-world)
                           (place-image/posn (circle (world-radius a-world) "solid" "blue")
                                             (world-posn a-world)
                                             (empty-scene WIDTH HEIGHT)))]))



;; world-bubble-collide?: world -> boolean
(define (world-bubble-collide? a-world)
  (< (distance (world-posn a-world)
               (world-target-posn a-world))
     (+ target-radius (world-radius a-world))))


;; distance: posn posn -> number
(define (distance posn-1 posn-2)
  (sqrt
   (+ (sqr (- (posn-x posn-1)
              (posn-x posn-2)))
      (sqr (- (posn-y posn-1)
              (posn-y posn-2))))))
        

;; place-image/posn: image posn scene -> scene
(define (place-image/posn an-image a-posn a-scene)
  (place-image an-image 
               (posn-x a-posn)
               (posn-y a-posn)
               a-scene))

;; posn+vel: posn velocity -> posn
(define (posn+vel a-posn a-vel)
  (make-posn (clamp (+ (posn-x a-posn) (vel-x a-vel))
                    0 WIDTH)
             (clamp (+ (posn-y a-posn) (vel-y a-vel))
                    0 HEIGHT)))


;; clamp: number number number -> number
;; Clamps a number x between a and b.
(define (clamp x a b)
  (cond [(> x b) b]
        [(< x a) a]
        [else x]))


;; handle-orientation-change: world number number number -> world
(define (handle-orientation-change a-world new-azimuth new-pitch new-roll)
  (make-world (world-posn a-world)
              (world-radius a-world)
              (world-target-posn a-world)
              (make-vel new-roll (- new-pitch))))



;; world-reset-if-collide: world -> world
(define (world-reset-if-collide a-world)
  (cond
    [(world-bubble-collide? a-world)
     (randomize-target-posn 
      (make-world (world-posn a-world)
                  30
                  (world-target-posn a-world)
                  (world-vel a-world)))]
    [else
     a-world]))





;; game-end?: world -> boolean
;; The game ends when the bubble gets too small.
(define (game-end? a-world)
  (<= (world-radius a-world)
     1))




(big-bang WIDTH HEIGHT (randomize-target-posn initial-world)
          (on-redraw render-world)
          (on-tick 1/10 tick)
          (on-tilt handle-orientation-change)
          (stop-when game-end?))