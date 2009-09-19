#lang s-exp "../moby-lang.ss"

;; A silencing music player.  Plays a song
;; whose volume will decrease with time.


;; The world is the amount of time that's
;; passed since the start, and the volume.
(define-struct world (time volume))


(define init-time 45)
(define fade-time 30)
(define init-volume 100)
(define song
  "http://hashcollision.org/tones/tank.ogg")

;; The world is a number counting the number
;; of seconds until the music turns off
(define initial-world 
  (make-world init-time init-volume))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; tick: world -> world
;; Decrease the volume after a threshold.
(define (tick a-world)
  (local [(define t 
            (sub1 (world-time a-world)))]
    (make-world 
     t
     (cond
       [(< t 0) 0]
       [(< t fade-time)
        (quotient (* t init-volume) 
                  fade-time)]
       [else init-volume]))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; shake: world -> world
;; On a shake, resets the world by returning
;; initial-world.
(define (shake a-world)
  initial-world)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; get-effects: world -> (listof effect)
;; We react with the external world by playing
;; a song, setting its volume, and making sure
;; the phone doesn't sleep during a playthrough.
(define (get-effects a-world)
  (list (change-volume a-world)
        (play-music a-world)
        (sleep-check a-world)))


;; On a tick, we set the volume.
;; change-volume: world -> effect
(define (change-volume a-world)
  (make-effect:set-sound-volume 
   (world-volume a-world)))

;; play-music: world -> effect
(define (play-music a-world)
  (cond
    [(> (world-volume a-world) 0)
     (make-effect:play-sound song)]
    [(<= (world-volume a-world) 0) 
     (make-effect:pause-sound song)]))


;; sleep-check: world -> effect
(define (sleep-check a-world)
  (cond [(should-stay-awake? a-world)
         (make-effect:set-wake-lock 6)]
        [else
         (make-effect:release-wake-lock)]))

;; should-stay-awake?: world -> boolean
(define (should-stay-awake? a-world)
  (>= (sub1 (world-time a-world)) 0))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; draw: world -> DOM-sexp
;;
;; Produces an HTML page for the user interface.
(define (draw a-world)
  (list (js-p '(("id" "aPara")))
        (list (js-text 
               (string-append 
                "volume = "
                (number->string 
                 (world-volume a-world)))))))

;; draw-css: world -> CSS-sexp
;; Produces a stylesheet to decorate the user
;; interface.
(define (draw-css a-world)
  '(("aPara" ("font-size" "30px"))))


(js-big-bang initial-world
             (on-tick* 1 tick get-effects)
             (on-shake* shake get-effects)
             
             (on-draw draw draw-css))