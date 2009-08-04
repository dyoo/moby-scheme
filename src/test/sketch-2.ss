;; The first three lines of this file were inserted by DrScheme. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname sketch-2) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ())))

;; The width and height of the drawing canvas.
(define WIDTH 320)
(define HEIGHT 480)
(define BLANK-COLOR "lightgray")
(define DRAW-COLOR "darkgray")
(define DOT-RADIUS 3)


;; amount of tilt required before we move in that direction.
(define TILT-THRESHOLD 10)


;; A world will be the current position,
;; and a scene of the drawn dots
;; and the drifting direction (one-of "left" "right" "up" "down" "stable")
(define-struct world (posn scene direction))


;; update-world-posn: world posn -> world
(define (update-world-posn a-world posn)
  (make-world posn 
              (world-scene a-world)
              (world-direction a-world)))

;; update-world-scene: world scene -> world
(define (update-world-scene a-world a-scene)
  (make-world (world-posn a-world)
              a-scene
              (world-direction a-world)))

;; update-world-direction: world direction -> world
(define (update-world-direction a-world a-direction)
  (make-world (world-posn a-world)
              (world-scene a-world)
              a-direction))


;; update-posn-x: posn number -> posn
(define (update-posn-x a-posn x)
  (make-posn x (posn-y a-posn)))


;; update-posn-y: posn number -> posn
(define (update-posn-y a-posn y)
  (make-posn (posn-x a-posn) y))



;; We start things off by putting the position at the very center, on an empty
;; canvas, with no drifting.
(define initial-world 
  (make-world (make-posn (/ WIDTH 2)
                         (/ HEIGHT 2))
              (place-image (nw:rectangle WIDTH HEIGHT "solid" BLANK-COLOR) 
                           0 
                           0
                           (empty-scene WIDTH HEIGHT))
              "stable"))



;; world-reset: world -> world
(define (world-reset a-world)
  initial-world)


;; draw-world-posn: posn scene -> scene
;; Draws the pin posn onto the scene.
(define (draw-world-posn a-posn a-scene)
  (place-image (nw:rectangle 1 3 "solid" "black")
               (posn-x a-posn)
               (posn-y a-posn)
               a-scene))


;; draw-dot: posn scene -> scene
;; Draws the dot onto a-scene.
(define (draw-dot a-posn a-scene)
  (place-image (circle DOT-RADIUS "solid" DRAW-COLOR)
               (posn-x a-posn)
               (posn-y a-posn)
               a-scene))


;; add-posn-to-dots: world -> world
(define (world-add-posn-to-scene a-world)
  (update-world-scene a-world
                      (draw-dot (world-posn a-world)
                                (world-scene a-world))))

;; move-left: world -> world
(define (move-left a-world)
  (world-add-posn-to-scene
   (update-world-posn a-world 
                      (update-posn-x (world-posn a-world)
                                     (max 0
                                          (- (posn-x (world-posn a-world)) DOT-RADIUS))))))

;; move-right: world -> world
(define (move-right a-world)
  (world-add-posn-to-scene
   (update-world-posn a-world
                      (update-posn-x (world-posn a-world)
                                     (min (sub1 WIDTH)
                                          (+ (posn-x (world-posn a-world)) DOT-RADIUS))))))
;; move-up: world -> world
(define (move-up a-world)
  (world-add-posn-to-scene
   (update-world-posn a-world
                      (update-posn-y (world-posn a-world)
                                     (max 0
                                          (- (posn-y (world-posn a-world)) DOT-RADIUS))))))
;; move-down: world -> world
(define (move-down a-world)
  (world-add-posn-to-scene
   (update-world-posn a-world
                      (update-posn-y (world-posn a-world)
                                     (min (sub1 HEIGHT)
                                          (+ (posn-y (world-posn a-world)) DOT-RADIUS))))))


;; move-by-drifting: world -> world
;; Move the pin based on the current drifting direction.
(define (move-by-drifting a-world)
  (cond
    [(string=? (world-direction a-world) "stable")
     a-world]
    [(string=? (world-direction a-world) "left")
     (move-left a-world)]
    [(string=? (world-direction a-world) "right")
     (move-right a-world)]
    [(string=? (world-direction a-world) "up")
     (move-up a-world)]
    [(string=? (world-direction a-world) "down")
     (move-down a-world)]))
               



;; render-etch-a-sketch: world -> scene
;; Draws the etch-a-sketch's canvas
(define (render-etch-a-sketch a-world)
  (draw-world-posn (world-posn a-world)
                   (world-scene a-world)))

  

;; handle-orientation-change: world number number number -> world
(define (handle-orientation-change a-world azimuth pitch roll)
  (cond
    [(upside-down? pitch roll)
     (world-reset a-world)]
    [else
     (update-world-direction a-world (get-orientation-direction pitch roll))]))
  


;; get-orientation-direction: number number -> direction
(define (get-orientation-direction pitch roll)
  (cond
    [(< 0 TILT-THRESHOLD pitch)
     "up"]
    [(<  pitch (- TILT-THRESHOLD) 0)
     "down"]
    [(< 0 TILT-THRESHOLD roll)
     "right"]
    [(< roll (- TILT-THRESHOLD) 0)
     "left"]
    [else
     "stable"]))


;; upside-down?: number number -> boolean
;; Returns true if we've gone upside-down.
(define (upside-down? pitch roll)
  (or (> (abs pitch) 120)
      (> (abs roll) 120)))


(big-bang WIDTH HEIGHT initial-world
          (on-redraw render-etch-a-sketch)
          (on-tick 1/20 move-by-drifting)
          (on-tilt handle-orientation-change))
