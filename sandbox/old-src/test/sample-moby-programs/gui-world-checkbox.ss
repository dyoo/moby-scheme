;; The first three lines of this file were inserted by DrScheme. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname gui-world-checkbox) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ())))
(require (lib "gui-world.ss" "gui-world"))

(define (world-update w b)
  b)

(define (world-value w)
  w)

(define (description w)
  (cond
    [w "This is true"]
    [else
     "this is false"]))

(define (flip w)
  (not w))

(big-bang true (row (checkbox "Click me" world-value world-update)
                    (message description)
                    (button "Reverse" flip)
                    ))