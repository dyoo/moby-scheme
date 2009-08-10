;; The first three lines of this file were inserted by DrScheme. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname rectangles) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ())))

(define WIDTH 320)
(define HEIGHT 480)

(define (draw-world a-world)
  (place-image (nw:rectangle 100 10 "solid" "red") 
               0
               0
               (place-image 
                (nw:rectangle 100 10 "solid" "green")
                0 
                20
                (empty-scene WIDTH HEIGHT))))


(big-bang WIDTH HEIGHT false
          (on-redraw draw-world))