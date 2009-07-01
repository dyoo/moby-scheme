;; The first three lines of this file were inserted by DrScheme. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname silencing-music) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ())))
(require (lib "world.ss" "moby" "stub"))

(define WIDTH 320)
(define HEIGHT 480)

(define-struct world (time volume song wake))

;; timer and initial volume will be user-inputted parameters later
(define init-time 45)
(define fade-time 30)
(define init-volume 60)
(define init-song "/sdcard/audio/06-Love_Like_Winter.mp3")

;; The world is a number counting the number of seconds until the music turns off
(define initial-world (make-world init-time init-volume init-song true))

;; this is the update for every tick
;; update: world -> world
(define (update a-world)
  (local [(define time (sub1 (world-time a-world)))]
    (make-world time
                (cond
                  [(< time 0) 0]
                  [(< time fade-time) (quotient (* time init-volume) fade-time)]
                  [else init-volume])
                (world-song a-world)
                (>= time 0))))

;; this resets the world by just returning the initial world
;; reset-world: world -> world
(define (reset-world a-world)
  initial-world)

;; this is what to run when the tick happens
;; change-volume: world -> effect
(define (change-volume a-world)
  (make-effect:set-sound-volume (world-volume a-world)))

;; play-music: world -> effect
(define (play-music a-world)
  (cond
    [(> (world-volume a-world) 0) (make-effect:play-sound-url (world-song a-world))]
    [(<= (world-volume a-world) 0) (make-effect:pause-sound-url (world-song a-world))]))

;; sleep-check: world -> effect
(define (sleep-check a-world)
  (if (world-wake a-world)
      (make-effect:set-wake-lock 6)
      (make-effect:release-wake-lock)))

;; get-effects: world -> listof effect
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
  (make-effect:stop-sound-url (world-song a-world)))

;; Used for generating the time as a string
;; takes a number of seconds and gives back the time in hh:mm:ss
;; secs->string: number -> string
#;(define (secs->string total-secs)
  (local [(define hours (quotient total-secs 3600))
          (define minutes (remainder (quotient total-secs 60) 60))
          (define seconds (remainder total-secs 60))]
    (string-append (if (< hours 10) "0" "")
                   (number->string hours)
                   ":"
                   (if (< minutes 10) "0" "")
                   (number->string minutes)
                   ":"
                   (if (< seconds 10) "0" "")
                   (number->string seconds))))

;; render: world -> scene
#;(define (render w)
  (place-image (text (string-append (number->string (world-time w))
                                    " rings")
                     10
                     "black")
               50
               50
               (empty-scene WIDTH HEIGHT)))



(big-bang WIDTH HEIGHT initial-world
          (on-tick* 1 update get-effects)
          #;(on-redraw render)
          (on-shake* reset-world get-effects)
          #;(stop-when* shut-off? stop-music))
