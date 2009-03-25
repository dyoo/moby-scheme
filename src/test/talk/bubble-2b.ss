;; The first three lines of this file were inserted by DrScheme. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname bubble-2b) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ())))
(require (lib "world.ss" "moby" "stub"))

(define WIDTH 320)
(define HEIGHT 480)

;; A velocity has an x and y component.
(define-struct vel (x y))

;; A target is at a random position.
(define target (make-posn 200 175 #;(random WIDTH) #;(random HEIGHT)))

;; A world is a posn, a radius, and a vel.
(define-struct world (posn r vel))

(define initial-w 
  (make-world (make-posn 150 80 #;(quotient WIDTH 2) #;(quotient HEIGHT 2))
              20
              (make-vel 0 0)))

;; tick: world -> world
(define (tick w)
  (make-world (posn+vel (world-posn w) (world-vel w))
              (- (world-r w) 1/3)
              (world-vel w)))

;; tilt: world number number number -> world
(define (tilt w azimuth pitch roll)
  (make-world (world-posn w)
              (world-r w)
              (make-vel roll (- pitch))))

;; render: world -> scene
(define (render w)
  (place-image/posn (circle 30 "solid" "red")
                    target
                    (place-image/posn (circle (world-r w) "solid" "blue")
                                      (world-posn w)
                                      (empty-scene WIDTH HEIGHT))))

;; collide?: world -> boolean
(define (collide? w)
  (< (distance (world-posn w) target)
     (world-r w)))

;; game-ends?: world -> boolean
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


(big-bang WIDTH HEIGHT 1/20 initial-w)
(on-redraw render)
;(on-tick tick)
(stop-when game-ends?)
(on-tilt tilt)
