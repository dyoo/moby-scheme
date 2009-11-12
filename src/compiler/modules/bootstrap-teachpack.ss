#lang s-exp "../../moby-lang.ss"





; start:String Image (Being list) (Being list) Being 
;              (Being -> Being) (Being -> Being) (Being -> Being)
;              (Being Being -> Boolean) (Being -> Boolean) -> Boolean
; takes in World components, updating functions and geometry functions and starts the universe
(define (start title background playerImg targetImgs objectImgs  
               update-player update-target update-object 
               collide? offscreen?)
  (local [;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
          ;; Game Structures
          ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

          ; A being is a (make-being Posn Image)
          ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
          ;; Game Structures
          ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
          (define-struct being [posn costume])
          
          ; A World is a (make-world (Being list) (Being list) Being Image Number String Integer)
          (define-struct world [objects targets player bg score title timer])
          
          ; make the score visible via a set!'d variable
          (define score 0)
          
          ; place a being at their location, on the BG, in their costume
          (define (draw-being being background)
            (place-image (being-costume being) 
                         (posn-x (being-posn being)) (posn-y (being-posn being))
                         background))
          
          ; draw all beings on the BG
          (define (draw-all beings background)
            (foldl draw-being background beings))
          
          ; draw-world : World -> Image
          ; draw the world, using either a player's costume or an explosion for the player
          (define (draw-world w)
            (let* ((score-string (string-append (world-title w) "    score:" (number->string (world-score w))))
                   (target-layer (draw-all (world-targets w) (world-bg w)))
                   (object-layer (draw-all (world-objects w) target-layer))
                   (player (if (> (world-timer w) 0)
                               (make-being (make-posn (posn-x (being-posn (world-player w)))
                                                      (posn-y (being-posn (world-player w))))
                                           (circle (* 1 5 (world-timer w))
                                                   "solid"
                                                   "gray")
                                           #;(star 7 (* 1.5 (world-timer w)) 
                                                 (* .25 (world-timer w))
                                                 "solid"
                                                 "gray"))
                               (world-player w)))
                   (player-layer (draw-being player object-layer)))
              (place-image (text score-string 20 "white") 10 0 player-layer)))
          
          ; test-frame : String Image Image Image Image -> Image
          ; draws a test frame of the game, using a stock world
          (define (test-frame title bg playerImg targetImgs objectImgs)
            (let* ((targetImgs (if (list? targetImgs) targetImgs (list targetImgs)))
                   (objectImgs (if (list? objectImgs) objectImgs (list objectImgs)))
                   (player (make-being (make-posn 320 400) playerImg))
                   (targets (map (lambda (t) (make-being (make-posn (random 640) (random 480)) t)) targetImgs))
                   (objects (map (lambda (o) (make-being (make-posn (random 640) (random 480)) o)) objectImgs))
                   (world (make-world objects targets player
                                      (put-pinhole bg 0 0)
                                      100
                                      title
                                      0)))
              (draw-world world)))
          
          ; update* : Being (Number / Number Number -> Number/Posn) -> Being
          ; allow update-target and update-object to take 1 or 2 numbers, and
          ; return posns or a single number
          (define (update* b update-function)
            (let* ((new-loc (if (= (procedure-arity update-function) 1)
                                (update-function (posn-x (being-posn b)))
                                (update-function (posn-x (being-posn b)) (posn-y (being-posn b)))))
                   (new-posn (if (posn? new-loc) new-loc (make-posn new-loc (posn-y (being-posn b))))))
              (make-being new-posn (being-costume b))))
          
          ; move-all : (Being list) (Number Number -> Being/Number) (Being->Boolean) -> (Being list)
          ; update everything according to an update-function, being sure to use update*
          (define (move-all beings update-function offscreen?)
            (map (lambda (b)
                   (if (offscreen? (posn-x (being-posn b)) (posn-y (being-posn b)))
                       (make-being (make-posn (+ 600 (object-spacing)) (random 480)) (being-costume b))
                       (update* b update-function)))
                 beings))
          
          (define (object-spacing) (+ (random 150) 40))
          
          ;; char->string : char -> String
          (define (char->string c)
            (cond [(not (char? c))
                   (error 'char->string (format "not a character ~s" c))]
                  [(eq? c #\space) "space"]
                  [else (string c)]))
          
          ; keypress : World Key (Player String -> Player)
          ; if the key is a direction, reutrn a new world with the moved player
          ; handle update-player where only the x is passed in, and where a number or posn is returned
          (define (keypress w key update-player)
            (cond
              [(symbol? key) (keypress w (symbol->string key) update-player)]
              [(char? key)   (keypress w (char->string key)   update-player)]
              [(not (string? key)) w]
              [(member key (list "up" "down" "left" "right"))
               (let* ((p (being-posn (world-player w)))
                      (new-loc (if (= (procedure-arity update-player) 2)
                                   (update-player (posn-y p) key)
                                   (update-player (posn-x p) (posn-y p) key)))
                      (new-posn (if (posn? new-loc) new-loc (make-posn (posn-x p) new-loc ))))
                 (make-world (world-objects w)
                             (world-targets w)
                             (make-being new-posn (being-costume (world-player w)))
                             (world-bg w)
                             (world-score w)
                             (world-title w)
                             (world-timer w)))]
              [else w]))
          
          ; world-collide? : (Being Being -> Boolean) (Being list) -> Boolean
          ; has the player collided with any objects?
          (define (any-collide? collide? player beings)
            (foldl (lambda (b bool) (or (collide? (posn-x (being-posn player))
                                                  (posn-y (being-posn player))
                                                  (posn-x (being-posn b))
                                                  (posn-y (being-posn b)))
                                        bool))
                   false
                   beings))]
    
    (let* ((player (make-being (make-posn 320 400) playerImg))
           (targetImgs (if (list? targetImgs) targetImgs (list targetImgs)))
           (objectImgs (if (list? objectImgs) objectImgs (list objectImgs)))
           (targets (map (lambda (t) (make-being (make-posn (random 640) (random 480)) t)) targetImgs))
           (objects (map (lambda (o) (make-being (make-posn (random 640) (random 480)) o)) objectImgs))
           (world (make-world objects targets player
                              (put-pinhole background 0 0)
                              100
                              title
                              0))
           (keypress* (lambda (w k) (keypress w k update-player)))
           (update-world (lambda (w) 
                           (let* ((objects (move-all (world-objects w) update-object offscreen?))
                                  (targets (move-all (world-targets w) update-target offscreen?))
                                  (score (world-score w))
                                  (player (world-player w))
                                  (bg (put-pinhole (world-bg w) 0 0))
                                  (title (world-title w))
                                  (timer (world-timer w)))
                             (begin 
                               (set! score (world-score w))
                               (cond
                                 [(> timer 0)
                                  (make-world objects targets player bg score title (- timer 10))]
                                 [(any-collide? collide? player objects)
                                  (begin
                                    #;(play-sound "hit.wav" true)
                                    (make-world objects targets player bg (- score 50) title 100))]
                                 [(any-collide? collide? player targets)
                                  (begin
                                    #;(play-sound "score.wav" true)
                                    (make-world objects targets player bg (+ score 20) title 100))]
                                 [else (make-world objects targets player bg score title timer)]))
                             ))))
      
      (js-big-bang world
                   (on-redraw draw-world)
                   (on-tick .1 update-world)
                   (on-key keypress*)))))


;(provide start)
