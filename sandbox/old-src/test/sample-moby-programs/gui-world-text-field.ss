;; The first three lines of this file were inserted by DrScheme. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname gui-world-text-field) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ())))
(require (lib "gui-world.ss" "gui-world"))

(define initial-world "hello")

(define (content w)
  w)

(define (update-content w new-w)
  new-w)

(define (show-content w)
  (string-append "The world contains: " w))

(big-bang initial-world (col (text-field content update-content)
                             (message show-content)))