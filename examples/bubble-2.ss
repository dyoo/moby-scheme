#lang s-exp "../moby-lang.ss"
;; Running out of time: the bubble-chasing game.


(define WIDTH 320)
(define HEIGHT 480)

(define TARGET-RADIUS 30)

;; A velocity has an x and y component.
(define-struct vel (x y))

;; A target is at a random position.
(define target (make-posn (random WIDTH) (random HEIGHT)))

;; A world is a posn, a radius, and a vel.
(define-struct world (posn r vel))

;; The initial world starts at the center, with stillness.
(define initial-w 
  (make-world (make-posn (quotient WIDTH 2) (quotient HEIGHT 2))
              30
              (make-vel 0 0)))

;; tick: world -> world
;; Moves the ball by a velocity, and shrinks it.
(define (tick w)
  (make-world (posn+vel (world-posn w) (world-vel w))
              (- (world-r w) 1/3)
              (world-vel w)))

;; tilt: world number number number -> world
;; Adjusts velocity based on the tilt.
(define (tilt w azimuth pitch roll)
  (make-world (world-posn w)
              (world-r w)
              (make-vel roll (- pitch))))


;; key: world key -> world
;; Adjust velocity based on key presses.
(define (key w a-key)
  (make-world (world-posn w)
              (world-r w)
              (update-vel-with-key (world-vel w) a-key)))
  

;; update-vel-with-key: vel key -> vel
;; Adjust the velocity based on which key the user presses.
(define (update-vel-with-key v a-key)
  (cond [(key=? a-key "left")
         (make-vel (- (vel-x v) 3)
                   (vel-y v))]
        [(key=? a-key "right")
         (make-vel (+ (vel-x v) 3)
                   (vel-y v))]
        [(key=? a-key "up")
         (make-vel (vel-x v)
                   (- (vel-y v) 3))]
        [(key=? a-key "down")
         (make-vel (vel-x v)
                   (+ (vel-y v) 3))]
        [else
         v]))
        

  
;; render: world -> scene
;; Renders the world.
(define (render w)
  (place-image/posn (circle TARGET-RADIUS "solid" "red")
                    target
                    (place-image/posn (circle (world-r w) "solid" "blue")
                                      (world-posn w)
                                      (empty-scene WIDTH HEIGHT))))

;; collide?: world -> boolean
;; Produces true if the target and the ball have collided.
(define (collide? w)
  (< (distance (world-posn w) target)
     (+ TARGET-RADIUS (world-r w))))


;; game-ends?: world -> boolean
;; Produces true if the game should finish.
(define (game-ends? w)
  (or (<= (world-r w) 1) (collide? w)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
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

;; distance: posn posn -> number
(define (distance posn-1 posn-2)
  (sqrt 
   (+ (sqr (- (posn-x posn-1) (posn-x posn-2)))
      (sqr (- (posn-y posn-1) (posn-y posn-2))))))

;; place-image/posn: image posn scene -> scene
(define (place-image/posn an-image a-posn a-scene)
  (place-image an-image (posn-x a-posn) (posn-y a-posn) a-scene))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(js-big-bang initial-w
          (on-redraw render)
          (on-key key)
          (on-tick 1/10 tick)
          (stop-when game-ends?)
          (on-tilt tilt))