;; The first three lines of this file were inserted by DrScheme. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-reader.ss" "lang")((modname local) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ())))
;; Simple text example.  
(define WIDTH 320)
(define HEIGHT 480)

(define (f x)
  42)

(define some-value
  (+ (f 2)
     (local [(define (f x y)
               (* x y))]
       (f 3 3))))
;; We expect some value to be 42 + 9 = 51.



(define (draw-scene y)
  (place-image (text (if (= some-value 51) "yes" "no") 10 "red") 
               0 0
               (empty-scene WIDTH HEIGHT)))

(big-bang WIDTH HEIGHT 0
          (on-redraw draw-scene))
