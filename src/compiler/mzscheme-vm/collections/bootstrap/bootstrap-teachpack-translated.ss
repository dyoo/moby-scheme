;#lang s-exp "../../moby-lang.ss"

#;(require 2htdp/universe
           lang/prim
           lang/posn
           (except-in htdp/testing test)
           (for-syntax scheme/base))

(provide START 
         #;EXAMPLE 
         test-frame score sq sine cosine tangent
         #;(except-out (all-from-out 2htdp/universe) on-key on-mouse))

; pass all student-defined functions to animate/proc, exposed as START
#;(define-higher-order-primitive START animate/proc (title title-color
                                                           background objectImgs targetImgs playerImg projectileImg
                                                           direction
                                                           update-player update-target update-object update-projectile
                                                           collide? offscreen?))

;;
;;SETTINGS 


(define WIDTH 640)
(define HEIGHT 480)
(define EXPLOSION-COLOR "gray")
(define TITLE-COLOR (box "white"))
(define PROJECTILE-IMG (box (star 5 20 30 "solid" "yellow")))
(define BACKGROUND (box (rectangle WIDTH HEIGHT "solid" "black")))
(define DIRECTION (box "left"))
(define score (box 0))
; how far between each being?
(define (spacing) (random 500))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Game Structures
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; A being is a (make-being Posn Image)
(define-struct being [posn costume])

; A World is a (make-world (Being list) (Being list) (Being list) Being Image Number String Integer)
(define-struct world [objects targets player projectiles bg score title timer])

; some easy accessor shortcuts
(define being-x (compose posn-x being-posn))
(define being-y (compose posn-y being-posn))

; Convert world position to screen position.
(define (posn->point posn) (make-posn (posn-x posn) (+ HEIGHT (- (posn-y posn)))))

; draw-being : Being Image -> Image
; place a being at their screen location, on the BG, in their costume
(define (draw-being being background)
  (let ((screen-posn (posn->point (being-posn being))))
    (place-image (being-costume being) 
                 (posn-x screen-posn) (posn-y screen-posn)
                 background)))

; draw-world : World -> Image
; draw the world, using either a player's costume or an explosion for the player
(define (draw-world w)
  (let* ((score-string (string-append (world-title w) "    score:" (number->string (world-score w))))
         (player (if (> (world-timer w) 0)
                     (make-being (being-posn (world-player w))
                                 (star 7 (* 1.5 (world-timer w)) (* .25 (world-timer w)) "solid" EXPLOSION-COLOR))
                     (world-player w)))
         (all-beings (append (world-targets w) (world-objects w) (world-projectiles w) (list player))))
    (place-image (text score-string 20 (unbox TITLE-COLOR)) 10 0 
                 (foldl draw-being (put-pinhole (unbox BACKGROUND) 0 0) all-beings))))


; wrap-update : (Number->Number / Number Number -> Posn) -> (Being -> Being)
; wrap the update function to ensure that it takes and returns a Being
(define (wrap-update f)
  (cond
    [(and (= (procedure-arity f) 1) (member (unbox DIRECTION) (list "top" "bottom")))
     (lambda (b) (make-being (make-posn (being-x b) (f (being-y b))) (being-costume b)))]
    [(and (= (procedure-arity f) 1) (member (unbox DIRECTION) (list "left" "right")))
     (lambda (b) (make-being (make-posn (f (being-x b)) (being-y b)) (being-costume b)))]
    [else (lambda (b) (make-being (f (being-x b) (being-y b)) (being-costume b)))]))

; reset : Being -> Being
; returns a new being with the same costume, who is ready to enter from DIRECTION
(define (reset being)
  (make-being
   (cond 
     [(string=? (unbox DIRECTION) "left") (make-posn (* (spacing) -1) (random HEIGHT))]
     [(string=? (unbox DIRECTION) "right") (make-posn (+ (spacing) WIDTH) (random HEIGHT))]
     [(string=? (unbox DIRECTION) "top") (make-posn (random WIDTH) (+ (spacing) HEIGHT))]
     [(string=? (unbox DIRECTION) "bottom") (make-posn (random WIDTH) (* (spacing) -1))])
   (being-costume being)))

; move-all : (Being list) (Number Number -> Being/Number) (Being->Boolean) -> (Being list)
; update every Being in a list according to a 'move' function
(define (move-all beings move offscreen?)
  (map (lambda (b) (if (offscreen? b) (reset b) (move b))) beings))

; keypress : World Key (Player String -> Player) -> World
; if the key is a direction, reutrn a new world with the moved player
; handle update-player where only the x is passed in, and where a number or posn is returned
(define (keypress w key update-player)
  (cond
    [(key=? key " ") 
     (make-world (world-objects w)
                 (world-targets w)
                 (world-player w)
                 (cons (make-being (being-posn (world-player w)) (unbox PROJECTILE-IMG))
                       (if (= 20 (length (world-projectiles w)))
                           (rest (world-projectiles w))
                           (world-projectiles w)))
                 (world-bg w)
                 (world-score w)
                 (world-title w)
                 (world-timer w))]
    [else
     (make-world (world-objects w)
                 (world-targets w)
                 (update-player (world-player w) key)
                 (world-projectiles w)
                 (world-bg w)
                 (world-score w)
                 (world-title w)
                 (world-timer w))]))

; any-collide? : (Being Being -> Boolean) (Being list) -> Boolean
; has the thing (t) collided with any beings?
(define (any-collide? collide? t beings)
  (not (empty? (filter (lambda (b) (collide? b t)) beings))))

; check-collision : (Being list) (Being list) (Being Being -> Boolean) -> (Being list) 
; return a list of beings, with their positions reset if hit a projectile
(define (check-collision beings projectiles collide?) 
  (map (lambda (being) (if (any-collide? collide? being projectiles) 
                           (reset being)
                           being))
       beings))

; animate/proc:String Image (Image list) (Image list) Image 
;              (Being -> Being) (Being -> Being) (Being -> Being)
;              (Being Being -> Boolean) (Being -> Boolean) -> Boolean
; takes in World components, updating functions and geometry functions and starts the universe
(define (START title title-color
               background objectImgs targetImgs playerImg projectileImg
               direction
               update-player* update-target* update-object* update-projectile*
               collide*? offscreen*?)
  (begin
    (set-box! PROJECTILE-IMG projectileImg)
    (set-box! TITLE-COLOR title-color)
    (set-box! BACKGROUND background)
    (set-box! DIRECTION direction)
    (let* ((player (make-being (make-posn (/ WIDTH 2) (/ HEIGHT 2)) playerImg))
           (targetImgs (if (list? targetImgs) targetImgs (list targetImgs)))
           (objectImgs (if (list? objectImgs) objectImgs (list objectImgs)))
           (targets (map (lambda (t) (make-being (make-posn (- 0 (spacing)) (random HEIGHT)) t)) targetImgs))
           (objects (map (lambda (o) (make-being (make-posn (- 0 (spacing)) (random HEIGHT)) o)) objectImgs))
           (projectiles empty)
           (update-object (wrap-update update-object*))
           (update-target (wrap-update update-target*))
           (update-projectile (wrap-update update-projectile*))
           (update-player (cond
                            [(and (= (procedure-arity update-player*) 2) (member (unbox DIRECTION) (list "left" "right")))
                             (lambda (p k) (make-being (make-posn (being-x p) (update-player* (being-y p) k))
                                                       (being-costume p)))]
                            [(and (= (procedure-arity update-player*) 2) (member (unbox DIRECTION) (list "top" "bottom")))
                             (lambda (p k) (make-being (make-posn (update-player* (being-x p) k) (being-y p))
                                                       (being-costume p)))]
                            [else (lambda (p k) (make-being (update-player* (being-x p) (being-y p) k)
                                                            (being-costume p)))]))
           (offscreen? (lambda (b) (if (= (procedure-arity offscreen*?) 1)
                                       (offscreen*? (being-x b))
                                       (offscreen*? (being-x b) (being-y b)))))
           (collide? (lambda (b1 b2) (collide*? (being-x b1) (being-y b1) (being-x b2) (being-y b2))))
           (world (make-world objects targets player projectiles
                              (put-pinhole background 0 0)
                              0
                              title
                              0))
           (keypress* (lambda (w k) (keypress w k update-player)))
           (update-world (lambda (w) 
                           (begin 
                             (set-box! score (world-score w))
                             (let* ((objects (move-all (check-collision (world-objects w) (world-projectiles w) collide?)
                                                       update-object offscreen?))
                                    (targets (move-all (world-targets w) update-target offscreen?))
                                    (projectiles (filter (lambda (x) (not (offscreen? x)))
                                                         (move-all (world-projectiles w) update-projectile offscreen?)))
                                    (score (world-score w))
                                    (player (world-player w))
                                    (bg (world-bg w))
                                    (title (world-title w))
                                    (timer (world-timer w)))
                               (cond
                                 [(> timer 0)
                                  (make-world objects targets player projectiles bg score title (- timer 10))]
                                 [(any-collide? collide? player objects)
                                  (begin
                                    #;(play-sound "hit.wav" true)
                                    (make-world objects targets player projectiles bg (- score 50) title 100))]
                                 [(any-collide? collide? player targets)
                                  (begin
                                    #;(play-sound "score.wav" true)
                                    (make-world objects targets player projectiles bg (+ score 20) title 100))]
                                 [else (make-world objects targets player projectiles bg score title timer)]))
                             ))))
      (js-big-bang world
                   (on-tick update-world .1)
                   (on-redraw draw-world)
                   (on-key keypress*)))))

; test-frame : String Image Image Image Image -> Image
; draws a test frame of the game, using a stock world
(define (test-frame title bg objectImgs targetImgs playerImg projectileImgs)
  (let* ((targetImgs (if (list? targetImgs) targetImgs (list targetImgs)))
         (objectImgs (if (list? objectImgs) objectImgs (list objectImgs)))
         (player (make-being (make-posn 320 400) playerImg))
         (targets (map (lambda (t) (make-being (make-posn (random 640) (random 480)) t)) targetImgs))
         (objects (map (lambda (o) (make-being (make-posn (random 640) (random 480)) o)) objectImgs))
         (projectiles (list (make-being (make-posn -200 0) (unbox PROJECTILE-IMG))))
         (world (make-world objects targets player projectiles
                            (put-pinhole bg 0 0)
                            100
                            title
                            0)))
    (draw-world world)))


; sq : Number -> Number
(define (sq x) (* x x))
; sine : Degrees -> Number
(define (sine x) (sin (* x (/ pi 180))))
; cosine : Degrees -> Number
(define (cosine x) (cos (* x (/ pi 180))))
; tangent : Degrees -> Number
(define (tangent x) (tan (* x (/ pi 180))))


;; a `test' macro that is a synonym for `check-expect', catches expansion
;; errors and pretends that they come from `test'.
#;(require (for-syntax syntax/kerncase))
#;(define-syntax (EXAMPLE stx)
    (syntax-case stx ()
      [(_ x ...)
       (with-handlers ([exn? (lambda (e)
                               (raise (make-exn
                                       (regexp-replace*
                                        #rx"check-expect"
                                        (exn-message e)
                                        "test")
                                       (exn-continuation-marks e))))])
         (local-expand (syntax/loc stx (check-expect x ...))
                       (syntax-local-context)
                       (kernel-form-identifier-list)))]))





