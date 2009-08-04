;; The first three lines of this file were inserted by DrScheme. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname approx-equal) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ())))

;; Simple text example.  
(define WIDTH 320)
(define HEIGHT 480)

(define t1 (text (cond [(=~ 3 4 0.5)
                            "blah"]
                           [else
                            "yes"])
                     10 
                     "black"))
(define t2 (text (cond [(=~ 3 4 1.0)
                            "yes"]
                           [else
                            "blah"])
                     10 
                     "black"))

;; we expect to see "yes, yes" on this
(define (draw-scene y)
  (place-image t2 0 30
               (place-image t1 
                            0 0
                            (empty-scene WIDTH HEIGHT))))

(big-bang WIDTH HEIGHT 
          0
          (on-redraw draw-scene))