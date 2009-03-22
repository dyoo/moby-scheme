;; The first three lines of this file were inserted by DrScheme. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname gui-world-hello-world) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ())))
(require (lib "gui-world.ss" "gui-world"))

(define initial-world 0)


(define (world-message a-world)
  (number->string a-world))

(define (on-button-pressed a-world)
  (add1 a-world))


(define view
  (col (row "hello" (col "*world*" 
                         (row "goodbye" 
                              (message "world"))))
       (message world-message)
       (button world-message on-button-pressed)))

(big-bang initial-world view)