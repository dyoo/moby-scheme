#lang s-exp "../moby-lang.ss"

(require jsworld/phonegap)

;; Rolling out of time
;;
;; Roll the blue ball onto the red target:
;; if the blue ball shrinks down to zero,
;; then the game ends.

;; A world is a posn, a radius, a vel, a
;; target posn, and a score.
(define-struct world 
  (posn r vel target-posn score))

(define WIDTH 300)
(define HEIGHT 460)

(define TARGET-RADIUS 30)

;; A velocity has an x and y component.
(define-struct vel (x y))

;; The initial world starts at the center.
(define initial-world 
  (make-world (make-posn (quotient WIDTH 2)
                         (quotient HEIGHT 2))
              30
              (make-vel 0 0)              
              (make-posn 0 0)
              0))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; tick: world -> world
;; Moves the ball by a velocity, and shrinks
;; it.
(define (tick w)
  (cond
    [(collide? w)
     (make-world (posn+vel (world-posn w)
                           (world-vel w))
                 30
                 (world-vel w)
                 (make-random-posn)
                 (add1 (world-score w)))]
    [else
     (make-world (posn+vel (world-posn w)
                           (world-vel w))
                 (- (world-r w) 1/2)
                 (world-vel w)
                 (world-target-posn w)
                 (world-score w))]))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; tilt: world number number number -> world
;; Adjusts velocity based on the tilt.
(define (tilt w azimuth pitch roll)
  (make-world (world-posn w)
              (world-r w)
              (make-vel roll (- pitch))
              (world-target-posn w)
              (world-score w)))

  
;; render: world -> scene
;; Renders the world.
(define (render w)
  (maybe-add-game-over 
   w
   
   (place-image/posn 
    (text (format "Score: ~a" (world-score w))
          20 "black")
    (make-posn 20 20)
    
    (place-image/posn 
     (circle TARGET-RADIUS "solid" "red")
     (world-target-posn w)
     
     (place-image/posn 
      (circle (world-r w) "solid" "blue")
      (world-posn w)
      (empty-scene WIDTH HEIGHT))))))


;; maybe-add-game-over: world scene -> scene
(define (maybe-add-game-over w a-scene)
  (cond
    [(game-ends? w)
     (place-image/posn 
      (text "GAME OVER" 30 "red")
      (make-posn 20 100)
      a-scene)]
    [else
     a-scene]))
     
  
;; collide?: world -> boolean
;; Produces true if the target and the ball
;; have collided.
(define (collide? w)
  (< (distance (world-posn w)
               (world-target-posn w))
     (+ TARGET-RADIUS (world-r w))))


;; game-ends?: world -> boolean
;; Produces true if the game should finish;
;; we end when there's no more ball left.
(define (game-ends? w)
  (<= (world-r w) 1))


;; make-random-posn: -> posn
;; Produces a random position for the target.
(define (make-random-posn)
  (make-posn (random WIDTH)
             (random HEIGHT)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; posn+vel: posn velocity -> posn
;; Adds a position by a velocity.
(define (posn+vel a-posn a-vel)
  (make-posn (clamp (+ (posn-x a-posn) 
                       (vel-x a-vel))
                    0 WIDTH)
             (clamp (+ (posn-y a-posn) 
                       (vel-y a-vel))
                    0 HEIGHT)))

;; clamp: number number number -> number
;; Clamps a number x between a and b.
(define (clamp x a b)
  (cond [(> x b) b]
        [(< x a) a]
        [else x]))

;; distance: posn posn -> number
;; Produces the Euclidean distance between
;; two positions.
(define (distance posn-1 posn-2)
  (sqrt 
   (+ (sqr (- (posn-x posn-1) 
              (posn-x posn-2)))
      (sqr (- (posn-y posn-1) 
              (posn-y posn-2))))))

;; place-image/posn: image posn scene -> scene
;; Place an image at a position into the
;; scene.
(define (place-image/posn img a-posn a-scene)
  (place-image img 
               (posn-x a-posn) 
               (posn-y a-posn) 
               a-scene))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



(js-big-bang initial-world
             (on-tick tick 1/20)
             (on-tilt tilt)
             
             (on-redraw render)
             (stop-when game-ends?))