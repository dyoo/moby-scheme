;; The first three lines of this file were inserted by DrScheme. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname hello-world) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ())))

;; Simple text example.  
(define WIDTH 320)
(define HEIGHT 480)


(define (draw-scene y)
  (place-image (text "hello world" 10 "red") 
               0 0
               (empty-scene WIDTH HEIGHT)))

(big-bang WIDTH HEIGHT 0
          (on-redraw draw-scene))
