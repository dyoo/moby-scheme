;; The first three lines of this file were inserted by DrScheme. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname net) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ())))

(define (content w)
  w)

(define (ignore-change w n)
  w)

(define (refresh w)
  (get-url "http://ip.hashcollision.org/"))

(define view
  (col
   (text-field content ignore-change)
   (button "Refresh!" refresh)))

(big-bang (get-url "http://ip.hashcollision.org/")
          view)