#lang s-exp "../moby-lang.ss"

;; A silencing music player.


;; The world is the amount of time that's passed since the start, and the volume.
(define-struct world (time volume))


;; timer and initial volume will be user-inputted parameters later
(define init-time 45)
(define fade-time 30)
(define init-volume 100)
(define song "http://hashcollision.org/tones/tank.ogg")

;; The world is a number counting the number of seconds until the music turns off
(define initial-world (make-world init-time init-volume))

;; this is the update for every tick
;; update: world -> world
(define (update a-world)
  (local [(define t (sub1 (world-time a-world)))]
    (make-world t
                (cond
                  [(< t 0) 0]
                  [(< t fade-time) (quotient (* t init-volume) fade-time)]
                  [else init-volume]))))

(define (should-stay-awake? a-world)
  (>= (sub1 (world-time a-world)) 0))


;; this resets the world by just returning the initial world
;; reset-world: world -> world
(define (reset-world a-world)
  initial-world)

;; On a tick, we set the volume.
;; change-volume: world -> effect
(define (change-volume a-world)
  (make-effect:set-sound-volume (world-volume a-world)))

;; play-music: world -> effect
(define (play-music a-world)
  (cond
    [(> (world-volume a-world) 0)
     (make-effect:play-sound-url song)]
    [(<= (world-volume a-world) 0) 
     (make-effect:pause-sound-url song)]))

;; sleep-check: world -> effect
(define (sleep-check a-world)
  (cond [(should-stay-awake? a-world)
         (make-effect:set-wake-lock 6)]
        [else
         (make-effect:release-wake-lock)]))

;; get-effects: world -> (listof effect)
(define (get-effects a-world)
  (list (change-volume a-world)
        (play-music a-world)
        (sleep-check a-world)))

;; shut-off? returns true if the timer is down to -5 or below
;; shut-off?: world -> boolean
(define (shut-off? a-world)
  (<= (world-time a-world) -5))

;; stop-music: world -> effect
(define (stop-music a-world)
  (make-effect:stop-sound-url song))


;; draw: world -> DOM-sexp
(define (draw a-world)
  (list (js-p '(("id" "aPara")))
        (list (js-text 
               (string-append 
                "volume = "
                (number->string (world-volume a-world)))))))

;; draw-css: world -> CSS-sexp
(define (draw-css a-world)
  '(("aPara" ("font-size" "30px"))))


(js-big-bang initial-world
             (on-draw draw draw-css)
             (on-tick* 1 update get-effects)
             (on-shake* reset-world get-effects))
