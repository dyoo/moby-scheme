;; The first three lines of this file were inserted by DrScheme. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname shake-to-ring) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ())))
(require (lib "world.ss" "moby" "stub"))

(define WIDTH 320)
(define HEIGHT 480)

;; The world records the acceleration vector.
(define-struct world (x y z))
(define initial-world (make-world 0 0 9))

(define (update a-world new-x new-y new-z)
  a-world)


(define (maybe-ring a-world new-x new-y new-z)
  (cond [(> (distance a-world (update a-world new-x new-y new-z))
            THRESHOLD)
         'beep]
        [else
         'none]))


;; distance: world world -> boolean
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
               20
               20
               (empty-scene WIDTH HEIGHT)))


(big-bang WIDTH HEIGHT initial-world
          (on-redraw render)
          (on-acceleration* update maybe-ring))
                            