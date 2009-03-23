;; The first three lines of this file were inserted by DrScheme. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname bubble-2) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ())))
(require (lib "world.ss" "moby" "stub"))

(define WIDTH 300)
(define HEIGHT 300)

(define-struct world (posn radius target-posn velocity-posn))
(define initial-world 
  (make-world (make-posn (quotient WIDTH 2) (quotient HEIGHT 2))
              30
              (make-posn 0 0)
              (make-posn 0 0)))

;; randomize-target-posn: world -> world
(define (randomize-target-posn a-world)
  (make-world (world-posn a-world)
              (world-radius a-world)
              (make-posn (random WIDTH) (random HEIGHT))
              (world-velocity-posn a-world)))
         
;; handle-on-tick: world -> world
(define (handle-on-tick a-world)
  (world-reset-if-collide-or-too-small
   (make-world (posn+ (world-posn a-world) 
                      (world-velocity-posn a-world))
               (- (world-radius a-world) 1/2)
               (world-target-posn a-world)
               (world-velocity-posn a-world))))
         
;; handle-orientation-change: world number number number -> world
(define (handle-orientation-change a-world new-azimuth pitch roll)
  (make-world (world-posn a-world)
              (world-radius a-world)
              (world-target-posn a-world)
              (make-posn roll (- pitch))))

;; render-world: world -> scene
(define (render-world a-world)
  (place-image/posn (circle 5 "solid" "red")
                    (world-target-posn a-world)
                    (place-image/posn (circle (world-radius a-world) "solid" "blue")
                                      (world-posn a-world)
                                      (empty-scene WIDTH HEIGHT))))

;; world-bubble-collide?: world -> boolean
(define (world-bubble-collide? a-world)
  (< (distance (world-posn a-world) (world-target-posn a-world))
     (world-radius a-world)))

;; world-reset-if-collide-or-too-small: world -> world
(define (world-reset-if-collide-or-too-small a-world)
  (cond
    [(or (<= (world-radius a-world) 1)
         (world-bubble-collide? a-world))
     (randomize-target-posn 
      (make-world (world-posn a-world)
                  30
                  (world-target-posn a-world)
                  (world-velocity-posn a-world)))]
    [else
     a-world]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; posn+: posn posn -> posn
(define (posn+ posn-1 posn-2)
  (make-posn (clamp (+ (posn-x posn-1) (posn-x posn-2))
                    0 WIDTH)
             (clamp (+ (posn-y posn-1) (posn-y posn-2))
                    0 HEIGHT)))

;; clamp: number number number -> number
(define (clamp x a b)
  (cond [(> x b)
         b]
        [(< x a)
         a]
        [else x]))

;; distance: posn posn -> number
(define (distance posn-1 posn-2)
  (sqrt 
   (+ (sqr (- (posn-x posn-1) (posn-x posn-2)))
      (sqr (- (posn-y posn-1) (posn-y posn-2))))))

;; place-image/posn: image posn scene -> scene
(define (place-image/posn an-image a-posn a-scene)
  (place-image an-image (posn-x a-posn) (posn-y a-posn) a-scene))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(big-bang WIDTH HEIGHT 1/20 (randomize-target-posn initial-world))
(on-redraw render-world)
(on-tick-event handle-on-tick)
(on-orientation-change-event handle-orientation-change)
