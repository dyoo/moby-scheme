(provide play make-game *score* *player-x* *player-y*
	 #;(all-from-out "bootstrap-common.rkt")
         #;(except-out (all-from-out 2htdp/universe) on-key on-mouse))




;; SETTINGS 
(define WIDTH 640)
(define HEIGHT 480)
(define EXPLOSION-COLOR "gray")
(define TITLE-COLOR (box "white"))
(define BACKGROUND (box (rectangle WIDTH HEIGHT "solid" "black")))
(define (spacing) (random 200))
(define *target-increment* 20)
(define *danger-increment* -50)

;; Globals available to the students:
(define *score* (box 0))
(define *player-x* (box 0))
(define *player-y* (box 0))

;; Student debugging:
(define *line-length* (box (lambda(a b) 0)))
(define *distance* (box (lambda(px cx py cy) 0)))
(define *distances-color* (box ""))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Game Structures
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; A being is a (make-being Posn Image String)
(define-struct being [posn costume source])

; A World is a (make-world (Being list) (Being list) (Being list) Being Image Number String Integer)
(define-struct world [dangers shots targets player bg score title timer])

; some easy accessor shortcuts
(define being-x (compose posn-x being-posn))
(define being-y (compose posn-y being-posn))

; Convert world position to screen position.
(define (posn->point posn) (make-posn (posn-x posn) (+ HEIGHT (- (posn-y posn)))))

; world-not-mutators
(define (world-with-dangers w d) (make-world d                 (world-shots w) (world-targets w) (world-player w) (world-bg w) (world-score w) (world-title w) (world-timer w)))
(define (world-with-shots   w s) (make-world (world-dangers w) s               (world-targets w) (world-player w) (world-bg w) (world-score w) (world-title w) (world-timer w)))
(define (world-with-targets w t) (make-world (world-dangers w) (world-shots w) t                 (world-player w) (world-bg w) (world-score w) (world-title w) (world-timer w)))
(define (world-with-player  w p) (make-world (world-dangers w) (world-shots w) (world-targets w) p                (world-bg w) (world-score w) (world-title w) (world-timer w)))
(define (world-with-score   w s) (make-world (world-dangers w) (world-shots w) (world-targets w) (world-player w) (world-bg w) s               (world-title w) (world-timer w)))
(define (world-with-timer   w t) (make-world (world-dangers w) (world-shots w) (world-targets w) (world-player w) (world-bg w) (world-score w) (world-title w) t))

; add-informative-triangle : Number Number String Image -> Image
(define (add-informative-triangle cx cy color background)
  (let* ((player-point (posn->point (make-posn (unbox *player-x*) (unbox *player-y*))))
         (px (posn-x player-point))
         (py (posn-y player-point)))
    (if (and (= px cx) (= py cy))
        background ;; don't inform about the player itself
        (let ((dx (- cx px))
              (dy (- cy py))
              (n->s (lambda (num) (number->string (inexact->exact (round num))))))
          (place-image 
           (text (n->s ((unbox *line-length*) cx px)) 12 color)
           (- cx (/ dx 2)) cy
           (place-image
            (line dx 0 color)
            (- cx (/ dx 2)) cy
            (place-image
             (text (n->s ((unbox *line-length*) cy py)) 12 color)
             px (- cy (/ dy 2))
             (place-image
              (line 0 dy color)
              px (- cy (/ dy 2))
              (place-image
               (text (n->s ((unbox *distance*) px py cx cy)) 12 color)
               (- cx (/ dx 2)) (- cy (/ dy 2))
               (place-image
                (line dx dy color)
                (- cx (/ dx 2)) (- cy (/ dy 2))
                background))))))))))
  
; draw-being : Being Image -> Image
; place a being at their screen location, on the BG, in their costume
(define (draw-being being background)
  (let* ((screen-point (posn->point (being-posn being)))
         (cx (posn-x screen-point))
         (cy (posn-y screen-point))
         (dbg-bkgnd (if (string=? (unbox *distances-color*) "")
                        background
                        (add-informative-triangle cx cy (unbox *distances-color*)
                                                  background))))
    (place-image (being-costume being) cx cy dbg-bkgnd)))
  
; (draw-being (make-being (make-posn 10 20) (circle 20 "solid" "red") "left") (rectangle 640 480 "solid" "yellow"))

; draw-world : World -> Image
; draw the world, using either a player's costume or an explosion for the player
(define (draw-world w)
  (let* ((score-string (string-append (world-title w) "                    score:"
				      (number->string (world-score w))))
         (player (if (> (world-timer w) 0)
                     (make-being (being-posn (world-player w))
                                 (radial-star 7 (* 1.5 (world-timer w)) (* .25 (world-timer w))
                                               "solid" EXPLOSION-COLOR)
				 (being-source (world-player w))
				 )
                     (world-player w)))
         (all-beings
	  (append (world-targets w) (world-dangers w) (world-shots w) (list player))))
    (begin
      (set-box! *player-x* (posn-x (being-posn (world-player w))))
      (set-box! *player-y* (posn-y (being-posn (world-player w))))
      (place-image (text/font score-string 18 (unbox TITLE-COLOR) #f 'default 'italic 'bold '#t)
         (quotient (image-width (unbox BACKGROUND)) 2) 20 
         (foldl draw-being (unbox BACKGROUND) all-beings)))))

(define example-world
  (make-world
   (list (make-being (make-posn 500 50) (triangle 30 "solid" "red") "right")) ; dangers
   '()
   (list (make-being (make-posn 100 50) (circle 30 "solid" "green") "right")) ; targets
   (make-being (make-posn 300 50) (rectangle 30 30 "solid" "blue") #f) ; player
   (rectangle WIDTH HEIGHT "solid" "yellow") ; background
   34075
   "I am a Title!"
   17)) ; timer (non-zero means draw it mid-explosion)
; (draw-world example-world)


; check-collision : Being (list Being) (lambda Being Being -> Boolean) -> Boolean
(define (check-collision player beings collide?)
  (not (empty? (filter (lambda (being) (collide? player being))
		       beings))))

(EXAMPLE (check-collision (make-being (make-posn 50 50) #f #f)
                          (list (make-being (make-posn 50 50) #f #f))
                          (lambda (p c) (= (being-x p) (being-x c))))
         true)
(EXAMPLE (check-collision (make-being (make-posn 50 50) #f #f)
                          (list (make-being (make-posn 51 50) #f #f))
                          (lambda (p c) (= (being-x p) (being-x c))))
         false)
(EXAMPLE (check-collision (make-being (make-posn 50 50) #f #f)
                          (list (make-being (make-posn 51 50) #f #f)
                                (make-being (make-posn 50 50) #f #f)
                                (make-being (make-posn 52 51) #f #f))
                          (lambda (p c) (= (being-x p) (being-x c))))
         true)

; survival-checker : (lambda x y -> Boolean) (lambda Being Being -> Boolean) (list Being) -> (lambda Being -> Boolean)
(define (survival-checker onscreen? collide? others)
  (lambda (being)
    (and (onscreen? (being-x being) (being-y being))
         (not (check-collision being others collide?)))))

; wrap-update : (Number->Number or Number Number -> Posn) (list String) -> (Being -> Being)
; wrap the update function to ensure that it takes and returns a Being
(define (wrap-update f)
  (cond
    [(= (procedure-arity f) 1)
     (lambda (b) (make-being (make-posn (f (being-x b)) (being-y b)) 
			     (being-costume b) (being-source b)))]
    [(= (procedure-arity f) 2)
     (lambda (b) (make-being (f (being-x b) (being-y b)) 
			     (being-costume b) (being-source b)))]
    [(= (procedure-arity f) 3)
     (lambda (b) (make-being (f (being-x b) (being-y b) (being-source b))
			     (being-costume b) (being-source b)))]))

(define (random-posn source)
  (let ((s (spacing))
	(p 1/3)) ;; spread
    (cond
     [(string=? source "left") 
      (make-posn (* s -1) (random HEIGHT))]
     [(string=? source "right") 
      (make-posn (+ s WIDTH) (random HEIGHT))]
     [(string=? source "top") 
      (make-posn (random WIDTH) (+ s HEIGHT))]
     [(string=? source "bottom") 
      (make-posn (random WIDTH) (* s -1))]
     [(string=? source "bottom-left") 
      (pick 
       (list 
	(make-posn (- s)                         (random (floor (* p HEIGHT))))
	(make-posn (random (floor (* p WIDTH)))  (- s))))]
     [(string=? source "bottom-right")
      (pick
       (list
	(make-posn (- WIDTH (* p WIDTH))         (- s))
	(make-posn (+ WIDTH s)                (random (floor (* p HEIGHT))))))]
     [(string=? source "top-left")
      (pick
       (list
	(make-posn (- s)                         (- HEIGHT (* p HEIGHT)))
	(make-posn (random (floor (* p WIDTH)))  (+ HEIGHT s))))]
     [(string=? source "top-right")
      (pick 
       (list 
	(make-posn (- WIDTH (* p WIDTH))         (+ HEIGHT s))
	(make-posn (+ WIDTH s)                   (- HEIGHT (* p HEIGHT)))))]
     [(string=? source "onscreen")
      (make-posn (+ (/ s 2) (random (- WIDTH s)))
		 (+ (/ s 2) (random (- HEIGHT s))))]
     [(string=? source "screen-left")
      (make-posn (* WIDTH 1/4) (+ (/ s 2) (random (- HEIGHT s))))]
     [(string=? source "screen-right")
      (make-posn (* WIDTH 3/4) (+ (/ s 2) (random (- HEIGHT s))))]
     [(string=? source "center")
      (make-posn (* WIDTH 1/2) (* HEIGHT 1/2))]
     [else (error (format "don't know what position ~a means" source))]
     )))

; get-direction : Function -> String
(define (get-direction updater)
  (let* ((next-posn (being-posn (updater (make-being (make-posn 1 1) #f #f))))
         (next-x (- (posn-x next-posn) 1))
         (next-y (- (posn-y next-posn) 1)))
    (if (and (= 0 next-y) (= 0 next-x))
        "onscreen"
        (if (> (abs next-y) (abs next-x))
            (if (< next-y 0) "top" "bottom")
            (if (< next-x 0) "right" "left")))))

; reset : Being -> Being
; returns a new being with the same costume, 
;   who is ready to enter from (being-source being)
(define (reset being srcspec)  
  (let ((source
         (cond 
           [(procedure? srcspec) (get-direction srcspec)]
           [(list? srcspec)      (pick srcspec)]
           [(string? srcspec)    srcspec]
           [else (error "strange being source specification")])))
    (make-being (random-posn source) (being-costume being) source)))

; move-all : (Being list) (Number Number -> Being/Number) (Being->Boolean) -> (Being list)
; update every Being in a list according to a 'move' function
(define (move-all beings move in-domain?)
  (map (lambda (b) 
	 (if (in-domain? b) 
	     (move b) 
	     (reset b move))) 
       beings))

; keypress : World Key (Player String -> Player) -> World
; if the key is a direction, reutrn a new world with the moved player
; handle update-player where only the x is passed in, and where a number or posn is returned
(define (keypress w key update-player make-shot)
  (cond
    [(string=? key "release") w]
    [(string=? key "escape") (world-with-timer w -1)]
    [(string=? key " ") (world-with-shots w (append (make-shot) (world-shots w)))] 
    [else (world-with-player w (update-player (world-player w) key))]))

(define (make-game title title-color background dangerImgs update-danger targetImgs update-target playerImg update-player projectileImgs update-projectile distances-color line-length distance collide onscreen)
  (list title title-color background dangerImgs update-danger targetImgs update-target playerImg update-player projectileImgs update-projectile distances-color line-length distance collide onscreen))
(define (play game)
  (apply animate/proc game))

; animate/proc:String Image (Image list) (Image list) Image 
;              (Being -> Being) (Being -> Being) (Being -> Being)
;              (Being Being -> Boolean) (Being -> Boolean) -> Boolean
; takes in World components, updating functions and geometry functions and starts the universe
(define (animate/proc title title-color
                      background 
		      dangerImgs    update-danger*
		      targetImgs    update-target*
		      playerImg     update-player*
                      projectileImg update-projectile*
                      distances-color line-length distance
                      collide*? onscreen*?)
  (begin
    (set-box! TITLE-COLOR title-color)
    (set-box! BACKGROUND background)
    (set-box! *line-length* line-length)
    (set-box! *distance* distance)
    (set-box! *distances-color* distances-color)
    (let* ((player (make-being (make-posn (/ WIDTH 2) (/ HEIGHT 2)) 
			       playerImg "center"))
           (targetImgs (if (list? targetImgs) targetImgs (list targetImgs)))
           (dangerImgs (if (list? dangerImgs) dangerImgs (list dangerImgs)))
	   (origTargets
            (map (lambda (i) (make-being (random-posn "screen-right") i false))
                 targetImgs))
           (origDangers
            (map (lambda (i) (make-being (random-posn "screen-left") i false))
                 dangerImgs))
           (update-danger (wrap-update update-danger*))
           (update-target (wrap-update update-target*))
           (update-projectile (wrap-update update-projectile*))
           (targets (if (and (= 1 (procedure-arity update-target*))
			     (equal? 100 (update-target* 100)))
                        origTargets ;; update-target is the template version
                        (map (lambda (t) (reset t update-target)) origTargets)))
           (dangers (if (and (= 1 (procedure-arity update-danger*))
			     (equal? 100 (update-danger* 100)))
                        origDangers ;; update-danger is the template version
                        (map (lambda (d) (reset d update-danger)) origDangers)))
           (projectiles '())
           (update-player (lambda (p k)
                            (make-being               
                             (if (= (procedure-arity update-player*) 2) 
                                 (make-posn (being-x p) (update-player* (being-y p) k))
                                 (update-player* (being-x p) (being-y p) k))
                             (being-costume p)
                             (being-source p))))
           (onscreen? (if (= (procedure-arity onscreen*?) 1) 
			  (lambda (x y) (onscreen*? x)) 
                          onscreen*?))
           (collide? (lambda (b1 b2) 
		       (collide*? (being-x b1) (being-y b1) 
				  (being-x b2) (being-y b2))))
           (world (make-world dangers projectiles targets player background
                              0 ; score
                              title
                              0 ; timeout
                              ))
           (keypress* (lambda (w k) (keypress w k update-player 
                                              (if (or (> (procedure-arity update-projectile*) 1)
                                                      (not (= (update-projectile* 100) 100)))
                                                  (lambda () (list (make-being (make-posn (unbox *player-x*) (unbox *player-y*)) 
                                                                         projectileImg "player")))
                                                  (lambda() '())))))
           (update-world (lambda (w) 
                           (begin 
                             (set-box! *score* (world-score w))
                             (let* ((dangers
				     (move-all (world-dangers w) update-danger 
                                               (survival-checker onscreen? collide? (world-shots w))))
                                    (projectiles
                                     (move-all (filter (survival-checker 
                                                        onscreen? collide? 
                                                        (append (world-dangers w) (world-targets w))) 
                                                       (world-shots w))
                                               update-projectile (lambda (b) true)))
                                    (targets
				     (move-all (world-targets w) update-target 
                                               (survival-checker onscreen? collide? (world-shots w))))
                                    (score (+ (world-score w)
                                              (if (ormap (lambda (s) (check-collision s (world-dangers w) collide?)) (world-shots w))
                                                  *target-increment* 0)
                                              (if (ormap (lambda(s) (check-collision s (world-targets w) collide?)) (world-shots w))
                                                  *danger-increment* 0)
                                              ))
                                    (player (world-player w))
                                    (bg (world-bg w))
                                    (title (world-title w))
                                    (timer (world-timer w)))
                               (cond
                                 [(> timer 0)
                                  (make-world dangers projectiles targets player bg 
					      score title (- timer 10))]
                                 [(check-collision player dangers collide?)
                                  (begin
                                    #;(play-sound "hit.wav" true)
                                    (make-world dangers projectiles targets player bg 
						(+ score *danger-increment*) title 100))]
                                 [(check-collision player targets collide?)
                                  (begin
                                    #;(play-sound "score.wav" true)
                                    (make-world dangers projectiles targets player bg 
						(+ score *target-increment*) title 100))]
                                 [else (make-world dangers projectiles targets player bg 
						   score title timer)]))
                             ))))
      (big-bang world
                (stop-when (lambda (w) (= (world-timer w) -1)))
                (on-tick update-world .1)
                (on-redraw draw-world)
                (on-key keypress*)))))

















(provide   sq sine cosine tangent
           pick subset? in?
           type #;warn number->image string->image put-image overlay-at)
  
  ;; warn : any* -> any, and a side effect.
  ;; display all arguments and return the last one.
  #;(define (warn . args)
    (begin
      (map display args)
      (newline)
      (last args)))
  
  ;; type : any -> String
  (define (type obj)
    (cond
      [(procedure? obj) "Function"]
      [(number? obj) "Number"]
      [(string? obj) "String"]
      [(image? obj) "Image"]
      [(boolean? obj) "Boolean"]
      [(posn? obj) "Position"]
      [(symbol? obj) "Symbol"]
      [else "I don't know."]))
  
  ;; string->image : String -> Image
  ;; convert the given string to an image.
  (define (string->image s)
    (text s 14 'black))
  
  ;; number->image : Number -> Image
  ;; convert the given number to an image.
  (define (number->image n)
    (string->image (number->string n)))

  ;; overlay-at : Image Number Number Image -> Image
  ;; Place the foreground on the background at x y 
  ;; (in positive-y point space) relative to the center
  (define (overlay-at background x y foreground)
    (overlay/xy background x (- 0 y) foreground))

  ;; put-image : Image Number Number Image -> Image
  ;; Place the foreground on the background at x y
  ;; (in positive-y point space) relative to the lower left
  (define (put-image foreground x y background)
    (place-image foreground x (- (image-height background) y) background))

  ; sq : Number -> Number
  (define (sq x) (* x x))
  ;; sine : Degrees -> Number
  ;; For a right triangle with non-right angle x in degrees,
  ;; find the ratio of the length of the opposite leg to the 
  ;; length of the hypotenuse.      sin = opposite / hypotenuse
  (define (sine x) (sin (* x (/ pi 180))))
  ;; cosine : Degrees -> Number
  ;; For a right triangle with non-right angle x in degrees,
  ;; find the ratio of the length of the adjacent leg to the 
  ;; length of the hypotenuse.      cos = adjacent / hypotenuse
  (define (cosine x) (cos (* x (/ pi 180))))
  ;; tangent : Degrees -> Number
  ;; For a right triangle with non-right angle x in degrees,
  ;; find the ratio of the length of the opposite leg to the
  ;; length of the adjacent leg.    tan = opposite / adjacent
  (define (tangent x) (tan (* x (/ pi 180))))

  ;; pick : List -> Element
  ;; pick a random element from the list
  (define (pick lst)
    (list-ref lst (random (length lst))))
  
  ;; subset? : List List -> Boolean
  ;; return true if list a is a (proper or improper) subset of b
  (define (subset? a b) 
    (andmap
     (lambda (ele) (member ele b))
     a))
  
  (define (in? a b)
    (if (list? a) (subset? a b) (member a b)))
  

