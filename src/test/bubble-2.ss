;; The first three lines of this file were inserted by DrScheme. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname bubble-2) (read-case-sensitive #t) (teachpacks ((lib "world.ss" "teachpack" "htdp"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "world.ss" "teachpack" "htdp")))))
(require (lib "world.ss" "moby" "stub"))

(define WIDTH 300)
(define HEIGHT 300)

(define-struct world (posn radius target-posn))
(define initial-world (make-world (make-posn (quotient WIDTH 2)
                                             (quotient HEIGHT 2))
                                  30
                                  (make-posn (quotient WIDTH 2)
                                             (quotient HEIGHT 2))))


;; randomize-target-posn: world -> world
(define (randomize-target-posn a-world)
  (rerandomize-target-posn-if-out-of-range
   (make-world (world-posn a-world)
               (world-radius a-world)
               (make-posn (random WIDTH)
                          (random HEIGHT)))))


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
                 (world-target-posn a-world))]
    [else
     a-world]))
         

;; render-world: world -> scene
(define (render-world a-world)
  (place-image/posn (circle 5 "solid" "red")
                    (world-target-posn a-world)
                    (place-image/posn (circle (world-radius a-world) "solid" "blue")
                                      (world-posn a-world)
                                      (empty-scene WIDTH HEIGHT))))


;; world-bubble-collide?: world -> boolean
(define (world-bubble-collide? a-world)
  (< (distance (world-posn a-world)
               (world-target-posn a-world))
     (world-radius a-world)))


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


;; handle-orientation-change: world number number number -> world
(define (handle-orientation-change a-world new-azimuth new-pitch new-roll)
  (cond
    [(game-end? a-world)
     a-world]
    [else
     (world-reset-if-collide
      (make-world (pitch-roll->posn new-pitch new-roll)
                  (world-radius a-world)
                  (world-target-posn a-world)))]))


;; world-reset-if-collide: world -> world
(define (world-reset-if-collide a-world)
  (cond
    [(world-bubble-collide? a-world)
     (randomize-target-posn 
      (make-world (world-posn a-world)
                  30
                  (world-target-posn a-world)))]
    [else
     a-world]))


;; pitch-roll->posn: number number -> posn
(define (pitch-roll->posn pitch roll)
  (make-posn (- (quotient WIDTH 2)
                (* (/ roll 90)
                   (quotient WIDTH 2)))
             (+ (* (/ pitch 90)
                   (quotient HEIGHT 2))
                (quotient HEIGHT 2))))



;; game-end?: world -> boolean
;; The game ends when the bubble gets too small.
(define (game-end? a-world)
  (<= (world-radius a-world)
     1))




(big-bang WIDTH HEIGHT 1/10 (randomize-target-posn initial-world))
(on-redraw render-world)
(on-tick-event world-deflate-bubble)
(on-orientation-change-event handle-orientation-change)
(stop-when game-end?)