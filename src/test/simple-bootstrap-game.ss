;; The first three lines of this file were inserted by DrScheme. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname simple-bootstrap-game) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ())))

(define WIDTH 320)
(define HEIGHT 480)

(define TITLE "My Simple Game")
(define background (empty-scene WIDTH HEIGHT))

(define (update-target-x x)
  (add1 x))

(define (update-target-y y)
  (sub1 y))

(define (update-player x key)
  (cond
    [(string=? key "left")
     (- x 10)]
    [(string=? key "right")
     (+ x 10)]
    [else x]))

(define (update-object y)
  (- y 20))

(define target (circle 20 "solid" "green"))

(define player (rectangle 30 40 "solid" "blue"))

(define object (circle 10 "solid" "black"))

(define (offscreen? x y)
  (or (< x 0)
      (> x WIDTH)
      (< y 0)
      (> y HEIGHT)))



(start TITLE background update-target-x update-target-y update-player update-object target player object offscreen?)

