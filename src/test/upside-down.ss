;; The first three lines of this file were inserted by DrScheme. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname upside-down) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ())))

(define width 320)
(define height 480)

;; The world is a boolean, which is true if we've been
;; flipped upside down.
(define initial-world false)

(define (render-world a-world)
  (place-image 
   (text (cond [a-world
                "upside down"]
               [else
                "right side up"])
         20
         "blue")
   0
   50
   (empty-scene width height)))


(define (handle-orientation-change a-world azimuth pitch roll)
  (or (> (abs pitch) 120)
      (> (abs roll) 120)))

(big-bang width height initial-world
          (on-redraw render-world)
          (on-tilt handle-orientation-change))