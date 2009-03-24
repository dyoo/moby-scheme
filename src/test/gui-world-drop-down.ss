;; The first three lines of this file were inserted by DrScheme. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname gui-world-drop-down) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ())))
(require (lib "gui-world.ss" "gui-world"))

(define initial-world "black")

;; current-choice: world -> string
(define (current-choice w)
  w)

;; choices: world -> (listof string)
(define (choices w)
  (list "red" "green" "blue" "black" "white"))

;; update-choice: world string -> world
(define (update-choice w new-choice)
  new-choice)

;; status: world -> string
(define (status w)
  (string-append "The current world is: " w))

(big-bang initial-world
          (col
           (drop-down current-choice choices update-choice)
           (message status)))