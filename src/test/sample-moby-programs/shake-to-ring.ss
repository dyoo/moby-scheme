;; The first three lines of this file were inserted by DrScheme. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname shake-to-ring) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ())))

(define WIDTH 320)
(define HEIGHT 480)
(define THRESHOLD 20)

;; The world records the acceleration vector.
(define-struct world (x y z))
(define initial-world (make-world 0 0 -9))


;; update: world number number number -> world
(define (update a-world new-x new-y new-z)
  (make-world new-x new-y new-z))


;; maybe-ring: world number number number -> effect
;; Maybe produce a ring, if the old world and the new world
;; have a significant distance difference.
(define (maybe-ring a-world new-x new-y new-z)
  (cond [(> (distance a-world (update a-world new-x new-y new-z))
            THRESHOLD)
         (make-effect:beep)]
        [else
         (make-effect:none)]))


;; distance: world world -> boolean
;; Returns the "distance" between two worlds.
(define (distance w1 w2)
  (sqrt (+ (sqr (- (world-x w1)
                   (world-x w2)))
           (sqr (- (world-y w1)
                   (world-y w2)))
           (sqr (- (world-z w1)
                   (world-z w2))))))


;; render: world -> scene
(define (render w)
  (place-image (text (string-append (number->string (world-x w))
                                    " "
                                    (number->string (world-y w))
                                    " "
                                    (number->string (world-z w)))
                     10
                     "black")
               50
               50
               (empty-scene WIDTH HEIGHT)))



(big-bang WIDTH HEIGHT initial-world
          (on-redraw render)
          (on-acceleration* update maybe-ring))
                            