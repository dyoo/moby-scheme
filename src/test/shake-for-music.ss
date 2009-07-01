;; The first three lines of this file were inserted by DrScheme. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname shake-for-music) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ())))
(require (lib "world.ss" "moby" "stub"))

(define WIDTH 320)
(define HEIGHT 480)

(define file-path "file:///android_asset/audio/Sweet Child.mp3")
(define initial-world false)

(define (swap w)
  (not w))

(define (play w)
  (if w
      (make-effect:play-sound-url file-path)
      (make-effect:pause-sound-url file-path)))

(big-bang WIDTH HEIGHT initial-world
          (on-shake* swap play))