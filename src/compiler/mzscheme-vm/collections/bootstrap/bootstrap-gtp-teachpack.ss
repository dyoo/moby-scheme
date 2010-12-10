;#lang s-exp "../../moby-lang.ss"

#;(require 2htdp/universe
           htdp/image
           htdp/world ;; for place-image
	   (except-in htdp/testing test)
           lang/prim
           lang/posn
           scheme/base
	   (for-syntax scheme/base))

(provide assert-equal 
	 #;EXAMPLE
	 #;(all-from-out lang/posn)
	 #;(except-out (all-from-out htdp/image) 
	    circle rectangle triangle star ellipse)
	 #;(rename-out 
	    (safer-circle circle)
	    (safer-triangle triangle)
	    (safer-rectangle rectangle)
	    (safer-star star)
	    (safer-ellipse ellipse))
	 type #;warn number->image string->image put-image overlay-at
	 start score sq sine cosine tangent
	 #;(except-out (all-from-out 2htdp/universe) on-key on-mouse)
	 )

#;(define-higher-order-primitive start animate/proc
    (title title-color
	   background dangerImgs dangerSource targetImgs targetSource playerImg
	   update-player update-target update-danger
	   collide? onscreen?))

 ;; a `test' macro that is a synonym for `check-expect', catches expansion
 ;; errors and pretends that they come from `test'.


(define *functions-complained-about* (box (make-hash)))

(define (assert-equal expected-value observed-value 
		      expected-expr observed-expr lineno)
  (if (equal? expected-value observed-value)
      (display "") ;; great
      (display
       (if (equal? (format "~a" expected-expr) (format "~a" expected-value))
	   (format
	    (string-append
	     "EXAMPLE CHECK FAILED: \n"
	     "  For ~a at line ~a\n"
	     "  Expected: ~s\n"
	     "   But got: ~s\n")
	    observed-expr lineno
	    expected-value
	    observed-value)
	   (format 
	    (string-append
	     "EXAMPLE CHECK FAILED: \n"
	     "  For ~a at line ~a\n"
	     "  Expected: ~a  =>  ~s\n"
	     "   But got: ~s\n")
	    observed-expr lineno
	    expected-expr expected-value
	    observed-value)))))

;(test-silence #t)

;(require (for-syntax syntax/kerncase))
;(require (for-syntax syntax/stx))
;(require (for-syntax syntax/to-string))

#;(define-for-syntax (stx/pp stx)
    (format "~a"
      (if (stx-list? stx)
          (string-append "(" (syntax->string stx) ")")
	  (syntax-e stx))))

#;(define-syntax (EXAMPLE stx)
    (syntax-case stx ()
      [(_ (fn arg ...) answer)
       (if (and (identifier? #'fn) (identifier-binding #'fn))
	   (with-syntax ([observed-expr (stx/pp (syntax (fn arg ...)))]
			 [expected-expr (stx/pp (syntax answer))]
			 [lineno (syntax-line stx)])
			(syntax
			 (assert-equal answer (fn arg ...) 
				       expected-expr observed-expr lineno)))
       
	   ;; if the identifier is just not bound
	   (syntax
	    (let ((complained?
		   (hash-ref (unbox *functions-complained-about*) 
			     (quote fn) #f)))
	      (display
	       (if complained? ""
		   (begin
		     (hash-set! (unbox *functions-complained-about*) 
				(quote fn) #t)
		     (format (string-append 
			      "Either the ~s function is not yet defined or "
			      "an example is above the definition.\n")
			     (quote fn))))))))]))


;; warn : any* -> SideEffect
;; display all arguments and return the last one.
#;(define (warn . args)
  (map display args)
  (newline)
  (last args))

;; type : Thing -> String
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

;; put-image : Image Number Number -> Image
;; Place the given image at the given location on an
;; otherwise empty 640x480 scene
(define (put-image img x y)
  (place-image img x (- 480 y) (empty-scene 640 480)))

;; overlay-at : Image Number Number Image -> Image
(define (overlay-at background x y foreground)
  (overlay/xy background x (- 0 y) foreground))

(define (safer-circle size fill color)
  (cond
   [(and (number? size) (<= 10000 size))
    (raise (string-append "circle: expected <positive real number less than 10000> as first argument, given: " (number->string size)))]
   [else (circle size fill color)]))
(define (safer-triangle size fill color)
  (cond
   [(and (number? size) (<= 10000 size))
    (raise (string-append "triangle: expected <positive real number less than 10000> as first argument, given: " (number->string size)))]
   [else (triangle size fill color)]))
(define (safer-rectangle width height fill color)
  (cond
   [(and (number? width) (<= 10000 width))
    (raise (string-append "rectangle: expected <positive real number less than 10000> as first argument, given: " (number->string width)))]
   [(and (number? height) (<= 10000 height))
    (raise (string-append "rectangle: expected <positive real number less than 10000> as second argument, given: " (number->string height)))]
   [else (rectangle width height fill color)]))
(define (safer-ellipse width height fill color)
  (cond
   [(and (number? width) (<= 10000 width))
    (raise (string-append "ellipse: expected <positive real number less than 10000> as first argument, given: " (number->string width)))]
   [(and (number? height) (<= 10000 height))
    (raise (string-append "ellipse: expected <positive real number less than 10000> as second argument, given: " (number->string height)))]
   [else (ellipse width height fill color)]))
(define (safer-star points outside inside fill color)
  (cond
   [(and (number? points) (<= 10000 points))
    (raise (string-append "star: expected <positive real number less than 10000> as first argument, given: " (number->string points)))]
   [(and (number? outside) (<= 10000 outside))
    (raise (string-append "star: expected <positive real number less than 10000> as second argument, given: " (number->string outside)))]
   [(and (number? inside) (<= 10000 inside))
    (raise (string-append "star: expected <positive real number less than 10000> as third argument, given: " (number->string inside)))]
   [else (star points outside inside fill color)]))

;; SETTINGS 
(define WIDTH 640)
(define HEIGHT 480)
(define EXPLOSION-COLOR "gray")
(define TITLE-COLOR (box "white"))
(define BACKGROUND (box (rectangle WIDTH HEIGHT "solid" "black")))
(define score (box 0))
; how far between each being?
(define (spacing) (random 200))
;(define (spacing) 0)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Game Structures
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; A being is a (make-being Posn Image String)
(define-struct being [posn costume source])

; A World is a (make-world (Being list) (Being list) (Being list) Being Image Number String Integer)
(define-struct world [dangers targets player bg score title timer])

; some easy accessor shortcuts
(define being-x (compose posn-x being-posn))
(define being-y (compose posn-y being-posn))

; Convert world position to screen position.
(define (posn->point posn) (make-posn (posn-x posn) (+ HEIGHT (- (posn-y posn)))))

; draw-being : Being Image -> Image
; place a being at their screen location, on the BG, in their costume
(define (draw-being being background)
  (let ((screen-posn (posn->point (being-posn being))))
    (place-image (being-costume being) 
                 (posn-x screen-posn) (posn-y screen-posn)
                 background)))

; draw-world : World -> Image
; draw the world, using either a player's costume or an explosion for the player
(define (draw-world w)
  (let* ((score-string (string-append (world-title w) "    score:"
				      (number->string (world-score w))))
         (player (if (> (world-timer w) 0)
                     (make-being (being-posn (world-player w))
                                 (star 7 (* 1.5 (world-timer w)) 
				       (* .25 (world-timer w)) 
				       "solid" EXPLOSION-COLOR)
				 (being-source (world-player w))
				 )
                     (world-player w)))
         (all-beings
	  (append (world-targets w) (world-dangers w) (list player))))
    (place-image (text score-string 20 (unbox TITLE-COLOR)) 10 0 
                 (foldl draw-being (put-pinhole (unbox BACKGROUND) 0 0) all-beings))))

(define (check-collision player beings collide?)
  (not (empty? (filter (lambda (being) (collide? player being))
		       beings))))

;; pick : List -> Element
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

; wrap-update : (Number->Number / Number Number -> Posn) (list String) -> (Being -> Being)
; wrap the update function to ensure that it takes and returns a Being
(define (wrap-update f sources)
  (cond
    [(and (= (procedure-arity f) 1) (in? sources (list "top" "bottom")))
     (lambda (b) (make-being (make-posn (being-x b) (f (being-y b))) 
			     (being-costume b) (being-source b)))]
    [(and (= (procedure-arity f) 1) (in? sources (list "left" "right")))
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

; reset : Being -> Being
; returns a new being with the same costume, 
;   who is ready to enter from (being-source being)
(define (reset being sources)
   (cond 
    [(list? sources)
     (let ((source (pick sources)))
       (make-being (random-posn source) (being-costume being) source))]
    [(string? sources)
     (make-being (random-posn sources) (being-costume being) sources)]
    [else (error "strange being source")]))

; move-all : (Being list) (Number Number -> Being/Number) (Being->Boolean) -> (Being list)
; update every Being in a list according to a 'move' function
(define (move-all beings move in-domain? sources)
  (map (lambda (b) 
	 (if (in-domain? (being-x b) (being-y b)) 
	     (move b) 
	     (reset b sources))) 
       beings))

; keypress : World Key (Player String -> Player) -> World
; if the key is a direction, reutrn a new world with the moved player
; handle update-player where only the x is passed in, and where a number or posn is returned
(define (keypress w key update-player)
  (cond
    [(string=? key "release") w]
    [else
     (make-world (world-dangers w)
                 (world-targets w)
                 (update-player (world-player w) key)
                 (world-bg w)
                 (world-score w)
                 (world-title w)
                 (world-timer w))]))

; animate/proc:String Image (Image list) (Image list) Image 
;              (Being -> Being) (Being -> Being) (Being -> Being)
;              (Being Being -> Boolean) (Being -> Boolean) -> Boolean
; takes in World components, updating functions and geometry functions and starts the universe
(define (start title title-color
                      background 
		      dangerImgs dangerSource
		      targetImgs targetSource
		      playerImg
                      update-player* update-target* update-danger*
                      collide*? onscreen*?)
  (begin
    (set-box! TITLE-COLOR title-color)
    (set-box! BACKGROUND background)
    (let* ((player (make-being (make-posn (/ WIDTH 2) (/ HEIGHT 2)) 
			       playerImg "center"))
           (targetImgs (if (list? targetImgs) targetImgs (list targetImgs)))
           (dangerImgs (if (list? dangerImgs) dangerImgs (list dangerImgs)))
	   (origTargets
	    (map (lambda (i)
		   (make-being (random-posn "screen-right") i targetSource))
		 targetImgs))
	   (origDangers
	    (map (lambda (i)
		   (make-being (random-posn "screen-left") i dangerSource))
		 dangerImgs))
           (targets (if (and (= 1 (procedure-arity update-target*))
			     (equal? 100 (update-target* 100)))
                        origTargets ;; update-target is the template version
                        (map (lambda (t) (reset t targetSource)) origTargets)))
           (dangers (if (and (= 1 (procedure-arity update-danger*))
			     (equal? 100 (update-danger* 100)))
			origDangers ;; update-danger is the template version
			(map (lambda (d) (reset d dangerSource)) origDangers)))
           (update-danger (wrap-update update-danger* dangerSource))
           (update-target (wrap-update update-target* targetSource))
           (update-player (cond
                            [(and (= (procedure-arity update-player*) 2) 
				  (member targetSource (list "left" "right"))
				  (member dangerSource (list "left" "right")))
                             (lambda (p k) 
			       (make-being 
				(make-posn (being-x p) 
					   (update-player* (being-y p) k))
				(being-costume p)
				(being-source p)))]
                            [(and (= (procedure-arity update-player*) 2) 
				  (member targetSource (list "top" "bottom"))
				  (member dangerSource (list "top" "bottom")))
                             (lambda (p k) 
			       (make-being
				(make-posn (update-player* (being-x p) k) 
					   (being-y p))
				(being-costume p)
				(being-source p)))]
                            [else (lambda (p k) 
				    (make-being (update-player* (being-x p) 
								(being-y p) k)
						(being-costume p)
						(being-source p)))]))
           (onscreen? (if (= (procedure-arity onscreen*?) 1) 
			  (lambda (x y) (onscreen*? x)) onscreen*?))
           (collide? (lambda (b1 b2) 
		       (collide*? (being-x b1) (being-y b1) 
				  (being-x b2) (being-y b2))))
           (world (make-world dangers targets player
                              (put-pinhole background 0 0)
                              0
                              title
                              0))
           (keypress* (lambda (w k) (keypress w k update-player)))
           (update-world (lambda (w) 
                           (begin 

			     ;; export it so students can read and act on it:
                             (set-box! score (world-score w))

                             (let* ((dangers
				     (move-all (world-dangers w) update-danger
					       onscreen? dangerSource))
                                    (targets
				     (move-all (world-targets w) update-target 
					       onscreen? targetSource))
                                    (score (world-score w))
                                    (player (world-player w))
                                    (bg (world-bg w))
                                    (title (world-title w))
                                    (timer (world-timer w)))
                               (cond
                                 [(> timer 0)
                                  (make-world dangers targets player bg 
					      score title (- timer 10))]
                                 [(check-collision player dangers collide?)
                                  (begin
                                    #;(play-sound "hit.wav" true)
                                    (make-world dangers targets player bg 
						(- score 50) title 100))]
                                 [(check-collision player targets collide?)
                                  (begin
                                    #;(play-sound "score.wav" true)
                                    (make-world dangers targets player bg 
						(+ score 20) title 100))]
                                 [else (make-world dangers targets player bg 
						   score title timer)]))
                             ))))
      (js-big-bang world
                (on-tick update-world .1)
                (on-redraw draw-world)
                (on-key keypress*)))))


; sq : Number -> Number
(define (sq x) (* x x))
; sine : Degrees -> Number
(define (sine x) (sin (* x (/ pi 180))))
; cosine : Degrees -> Number
(define (cosine x) (cos (* x (/ pi 180))))
; tangent : Degrees -> Number
(define (tangent x) (tan (* x (/ pi 180))))
