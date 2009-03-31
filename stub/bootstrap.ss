#lang scheme/base

(require htdp/world
         lang/prim)

(provide (except-out (all-from-out htdp/world) on-key-event on-mouse-event))

(provide start)

(define-higher-order-primitive start animate/proc  (_ _ update-target-x update-target-y update-player 
                                                      update-object
                                                      _ _ _
                                                      target-offscreen?))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Game Structures
; a Ufo is a (make-target Number Number String)
(define-struct target [x y])
; a Missile is a (make-object Number Number)
(define-struct object [x y])

; a Player is a Number (x)
; a World is a (make-world Target Number Number Number Number)
(define-struct world [target player object score timer])

;;;;;;;;;;;;;;;;;;; INITIAL VARIABLES ;;;;;;;;;;;;;;;;;;;
(define target1  (make-target 5 235))
(define object1 (make-object 380 -400))
(define player1 	 320)
(define world1   (make-world target1 player1 object1 0 1))




(define (animate/proc TITLE bg
                      update-target-x update-target-y update-player update-object 
                      target player object target-offscreen?)
  (let* ((draw-world* (lambda (w) (draw-world TITLE bg w object target player)))
         (keypress* (lambda (w k) (keypress w k update-player)))
         (collide?* (lambda (w) (struct-collide? (world-target w) (world-object w) collide?)))
         (update-t* (lambda (w)
                      (make-target (update-target-x (target-x (world-target w)))
                                   (update-target-y (target-y (world-target w))))))
         (update-m* (lambda (m)
                      (if (< (object-y m) -1000) object1 (make-object (object-x m) (update-object (object-y m))))))
         (update-world (lambda (w)
                         (cond
                           [(target-offscreen? (target-x (world-target w)) (target-y (world-target w)))
                            (make-world target1 (world-player w) (update-m* (world-object w)) (world-score w) 0)]
                           [(collide?* w) 
                            (make-world target1 (world-player w) (update-m* (world-object w)) (+ 100 (world-score w)) 151)]
                           [(> (world-timer w) 1)
                            (make-world (world-target w) (world-player w) (update-m* (world-object w)) 
                                        (world-score w) (- (world-timer w) 15))]
                           [else (make-world (update-t* w) (world-player w) (update-m* (world-object w)) 
                                             (world-score w) (world-timer w))]))))
    (big-bang 320 480 .1 world1 (> 3 2))
    (on-tick-event update-world)
    (on-redraw draw-world*)
    (on-key-event keypress*)))



; draw-world : String Image World Image Image Image -> Image
; assemble layers: bg + target/explosion + object + player + text
(define (draw-world TITLE bg w object target player)
  (let* ((explosion (circle (+ (world-timer w) 1) "solid" (if (even? (world-timer w)) "red" "orange")))
         (score-text (string-append TITLE "      score: " (number->string (world-score w))))
         (add-target (cond
                       [(> (world-timer w) 1) (place-image explosion (object-x (world-object w))
                                                           (object-y (world-object w)) (put-pinhole bg 0 0))]
                       [else (place-image target (target-x (world-target w))
                                          (target-y (world-target w)) (put-pinhole bg 0 0))]))
         (add-object (place-image object (object-x (world-object w)) (object-y (world-object w)) add-target))
         (add-player (place-image player (world-player w) 410 add-object)))
    (place-image (text score-text 10 'black) 10 0 add-player)))





; keypress : World String (Number String -> Number) -> World
; Given a world and a key, return a new world with updated player or object
; convert symbols (some implementations), exit peacefully if char
(define (keypress w key update-player)
  (cond
    [(symbol? key) (keypress w (symbol->string key) update-player)]
    [(char? key) w]
    [(string=? key "left") (make-world (world-target w) (update-player (world-player w) key)
                                       (world-object w) (world-score w) (world-timer w))]
    [(string=? key "right") (make-world (world-target w) (update-player (world-player w) key)
                                        (world-object w) (world-score w) (world-timer w))]
    [(string=? key "up") (make-world (world-target w) (world-player w) (fire-object w) (world-score w) 0)]
    [else w]))

; struct-collide? : Target Missile (Number Number Number Number->Boolean) -> Boolean
; break down the world structure and use user's collide?
(define (struct-collide? t m collide?) 
  (collide? (target-x t) (target-y t) (object-x m) (object-y m)))



; distance : Number Number Number Number-> Number
; determines the distance between the target's (x,y) and the object's (x,y)
(define (distance x1 y1 x2 y2)
  (sqrt (+ (sqr (- x2 x1)) (sqr (- y2 y1)))))



; collide? : Number Number Number Number -> Boolean
; return true if the coordinates are within 15 of each other
(define (collide? x1 y1 x2 y2)
  (< (distance x1 y1 x2 y2) 30))


; fire-object : World -> Missile
; set the object to the player's coordinates, unless it's been fired recently
(define (fire-object w)
  (if (< (object-y (world-object w)) 100)
      (make-object (world-player w) 400)
      (make-object (object-x (world-object w)) (object-y (world-object w)))))


(define (sqr x)
  (* x x))