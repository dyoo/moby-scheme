;; The first three lines of this file were inserted by DrScheme. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname silencing-music) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ())))
;; Plays a song with a decaying volume.

;; The world is a volume number between 0 and 70.

;; timer and initial volume will be user-inputted parameters later
(define song "file:///android_asset/song.ogg")

;; The world is a number counting the number of seconds until the music turns off
(define initial-world 100)


;; update: world -> world
;; Every tick, reduces the volume.
(define (tick w)
  (max 0 (sub1 w)))


;; reset: world -> world
;; Resets the world to the initial value.
(define (reset w)
  initial-world)


;; get-effects: world -> (listof effect)
;; Returns the list of effects that the world should be applying.
;; Ensures the song is playing, at a particular volume,
;; and that the phone doesn't go to sleep prematurely.
(define (get-effects w)
  (list (make-effect:play-sound-url song)
        (make-effect:set-sound-volume w)))


;; draw: world -> DOM-sexp
;; Draws the current volume on screen.
(define (draw w)
  (list (js-p '(("id" "aPara")))
        (list (js-text (string-append "volume = " (number->string w))))))


;; draw-css: world -> CSS-sexp
;; The paragraph will have large text.
(define (draw-css a-world)
  '(("aPara" ("font-size" "30px"))))


(js-big-bang initial-world
             '()
             (on-draw draw draw-css)
             (on-shake reset)
             (on-tick* 1/5 tick get-effects))