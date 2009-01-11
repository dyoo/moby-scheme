;; The first three lines of this file were inserted by DrScheme. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname struct-question) (read-case-sensitive #t) (teachpacks ((lib "world.ss" "teachpack" "htdp"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "world.ss" "teachpack" "htdp")))))
;; Simple text example.  
(define WIDTH 100)
(define HEIGHT 100)

(define t1 (text (cond [(struct? 42)
                        "blah"]
                       [else
                        "yes"])
                 10 
                 "black"))
(define t2 (text (cond [(struct? (make-posn 1 2))
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

(big-bang WIDTH HEIGHT 1 0)
(on-redraw draw-scene)