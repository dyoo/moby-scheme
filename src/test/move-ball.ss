;; The first three lines of this file were inserted by DrScheme. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname move-ball) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ())))
;; Movable circle with posns.

;; A world is a posn representing the x,y position of the red ball.

(define WIDTH 320)
(define HEIGHT 480)
(define RADIUS 5)

(define (tick w) w)

(define (move-ball w k)
  (cond
    [(key=? k 'up)
     (make-posn (posn-x w) (- (posn-y w) 5))]
    [(key=? k 'down)
     (make-posn (posn-x w) (+ (posn-y w) 5))]
    [(key=? k 'left)
     (make-posn (- (posn-x w) 5) (posn-y w))]
    [(key=? k 'right)
     (make-posn (+ (posn-x w) 5) (posn-y w))]
    [else
     w]))


(define (draw-scene w)
  (place-image (circle RADIUS "solid" "red") 
               (posn-x w)
               (posn-y w)
               (empty-scene WIDTH HEIGHT)))


(big-bang WIDTH HEIGHT (make-posn (/ WIDTH 2) (/ HEIGHT 2))
          (on-tick 1 tick)
          (on-redraw draw-scene)
          (on-key move-ball))