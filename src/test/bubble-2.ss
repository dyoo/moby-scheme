;; The first three lines of this file were inserted by DrScheme. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname bubble-2) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ())))
(require (lib "world.ss" "moby" "stub"))

(define WIDTH 300)
(define HEIGHT 300)

;; A world is a posn, a radius, a target posn, and a velocity posn.
(define-struct world (posn r target vel))

(define initial-world 
  (make-world (make-posn (quotient WIDTH 2) (quotient HEIGHT 2))
              30
              (make-posn (random WIDTH) (random HEIGHT))
              (make-posn 0 0)))

;; tick: world -> world
(define (tick w)
  (make-world (posn+ (world-posn w) 
                     (world-vel w))
              (- (world-r w) 1/3)
              (world-target w)
              (world-vel w)))

;; tilt: world number number number -> world
(define (tilt w azimuth pitch roll)
  (make-world (world-posn w)
              (world-r w)
              (world-target w)
              (make-posn roll (- pitch))))

;; render-world: world -> scene
(define (render-world w)
  (place-image/posn (circle 5 "solid" "red")
                    (world-target w)
                    (place-image/posn (circle (world-r w) "solid" "blue")
                                      (world-posn w)
                                      (empty-scene WIDTH HEIGHT))))

;; collide?: world -> boolean
(define (collide? w)
  (< (distance (world-posn w) (world-target w))
     (world-r w)))

;; game-ends?: world -> boolean
(define (game-ends? w)
  (or (<= (world-r w) 1) (collide? w)))


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

(big-bang WIDTH HEIGHT 1/20 initial-world)
(on-redraw render-world)
(on-tick-event tick)
(stop-when game-ends?)
(on-orientation-change-event tilt)
