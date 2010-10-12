;; The first three lines of this file were inserted by DrScheme. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname sketch) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ())))

;; The width and height of the drawing canvas.
(define WIDTH 320)
(define HEIGHT 480)
(define BLANK-COLOR "lightgray")
(define DRAW-COLOR "darkgray")
(define DOT-RADIUS 3)


;; amount of tilt required before we move in that direction.
(define TILT-THRESHOLD 10)


;; A world will be the current position,
;; and a list of posns representing
;; the dots drawn already,
;; and the drifting direction (one-of "left" "right" "up" "down" "stable")
(define-struct world (posn dots direction))


;; update-world-posn: world posn -> world
(define (update-world-posn a-world posn)
  (make-world posn 
              (world-dots a-world)
              (world-direction a-world)))

;; update-world-dots: world (listof posn) -> world
(define (update-world-dots a-world dots)
  (make-world (world-posn a-world)
              dots
              (world-direction a-world)))

;; update-world-direction: world direction -> world
(define (update-world-direction a-world a-direction)
  (make-world (world-posn a-world)
              (world-dots a-world)
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
              empty
              "stable"))


;; draw-world-posn: posn scene -> scene
;; Draws the pin posn onto the scene.
(define (draw-world-posn a-posn a-scene)
  (place-image (nw:rectangle 1 3 "solid" "black")
               (posn-x a-posn)
               (posn-y a-posn)
               a-scene))


;; draw-dots: (listof posn) scene -> scene
;; Draws the dots onto a-scene.
(define (draw-dots dots a-scene)
  (cond
    [(empty? dots)
     a-scene]
    [else
     (draw-dots (rest dots)
                (place-image (circle DOT-RADIUS "solid" DRAW-COLOR)
                             (posn-x (first dots))
                             (posn-y (first dots))
                             a-scene))]))


;; add-posn-to-dots: world -> world
(define (add-posn-to-dots a-world)
  (update-world-dots a-world
                     (cons (world-posn a-world)
                           (world-dots a-world))))

;; move-left: world -> world
(define (move-left a-world)
  (add-posn-to-dots
   (update-world-posn a-world 
                      (update-posn-x (world-posn a-world)
                                     (max 0
                                          (- (posn-x (world-posn a-world)) DOT-RADIUS))))))

;; move-right: world -> world
(define (move-right a-world)
  (add-posn-to-dots
   (update-world-posn a-world
                      (update-posn-x (world-posn a-world)
                                     (min (sub1 WIDTH)
                                          (+ (posn-x (world-posn a-world)) DOT-RADIUS))))))
;; move-up: world -> world
(define (move-up a-world)
  (add-posn-to-dots
   (update-world-posn a-world
                      (update-posn-y (world-posn a-world)
                                     (max 0
                                          (- (posn-y (world-posn a-world)) DOT-RADIUS))))))
;; move-down: world -> world
(define (move-down a-world)
  (add-posn-to-dots
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
                   (draw-dots (world-dots a-world)
                              (place-image (nw:rectangle WIDTH HEIGHT "solid" BLANK-COLOR)
                                           0
                                           0
                                           (empty-scene WIDTH HEIGHT)))))

  

;; handle-orientation-change: world number number number -> world
(define (handle-orientation-change a-world azimuth pitch roll)
  (update-world-direction a-world (get-orientation-direction pitch roll)))
  


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



(big-bang WIDTH HEIGHT initial-world
          (on-redraw render-etch-a-sketch)
          (on-tick 1/20 move-by-drifting)
          (on-tilt handle-orientation-change))
