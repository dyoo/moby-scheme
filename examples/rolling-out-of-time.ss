#lang s-exp "../moby-lang.ss"
;; Running out of time: the bubble-chasing game.


;; A world is a posn, a radius, a vel, a target posn, and a score.
(define-struct world (posn r vel target-posn score))




(define WIDTH 320)
(define HEIGHT 480)

(define TARGET-RADIUS 30)

;; A velocity has an x and y component.
(define-struct vel (x y))


;; The initial world starts at the center, with stillness.
(define initial-world 
  (make-world (make-posn (quotient WIDTH 2) (quotient HEIGHT 2))
              30
              (make-vel 0 0)              
              (make-posn 0 0)
              0))


;; tick: world -> world
;; Moves the ball by a velocity, and shrinks it.
(define (tick w)
  (cond
    [(collide? w)
     (make-world (posn+vel (world-posn w) (world-vel w))
                 30
                 (world-vel w)
                 (world-target-posn w)
                 (add1 (world-score w)))]
    [else
     (make-world (posn+vel (world-posn w) (world-vel w))
                 (- (world-r w) 1/3)
                 (world-vel w)
                 (world-target-posn w)
                 (world-score w))]))





;; tick-effect: world -> effect
;; If we do collide, re-randomize the target posn.
(define (tick-effect w)
  (cond
    [(collide? w)
     (randomize-world-target w)]
    [else
     empty]))
    



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
  (maybe-add-game-over-message 
   w
   (place-image/posn (text (format "Score: ~a" (world-score w)) 20 "black")
                     (make-posn 20 20)
                     (place-image/posn (circle TARGET-RADIUS "solid" "red")
                                       (world-target-posn w)
                                       (place-image/posn (circle (world-r w) "solid" "blue")
                                                         (world-posn w)
                                                         (empty-scene WIDTH HEIGHT))))))
;; maybe-add-game-over-message: world scene -> scene
(define (maybe-add-game-over-message w a-scene)
  (cond
    [(game-ends? w)
     (place-image/posn (text "GAME OVER" 30 "red")
                       (make-posn 20 100)
                       a-scene)]
    [else
     a-scene]))
     
  
;; collide?: world -> boolean
;; Produces true if the target and the ball have collided.
(define (collide? w)
  (< (distance (world-posn w) (world-target-posn w))
     (+ TARGET-RADIUS (world-r w))))


;; game-ends?: world -> boolean
;; Produces true if the game should finish; we end when there's
;; no more ball left.
(define (game-ends? w)
  (<= (world-r w) 1))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; key: world key -> world
;; Adjust velocity based on key presses.
(define (key w a-key)
  (make-world (world-posn w)
              (world-r w)
              (update-vel-with-key (world-vel w) a-key)
              (world-target-posn w)
              (world-score w)))
  

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

;; randomize-world-target: world -> effect
;; Constructs an effect that, when triggered, will randomize the world.
(define (randomize-world-target w)
  (list (make-effect:pick-random
         WIDTH
         (lambda (w n) 
           (update-world-target-posn 
            w 
            (make-posn n (posn-y (world-posn w))))))
        (make-effect:pick-random 
         HEIGHT
         (lambda (w n) 
           (update-world-target-posn
            w 
            (make-posn (posn-x (world-posn w)) n))))))

;; update-world-target-posn: world posn -> world
;; Updates the target posn
(define (update-world-target-posn w posn)
  (make-world 
   (world-posn w)
   (world-r w)
   (world-vel w)
   posn
   (world-score w)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



(js-big-bang initial-world
             (initial-effect (randomize-world-target initial-world))

             (on-tick* 1/20 tick tick-effect)
             (on-tilt tilt)

             (on-redraw render)
             (on-key key)
             (stop-when game-ends?))