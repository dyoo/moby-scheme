#lang scheme/base


(require scheme/class
         scheme/local
         scheme/bool
         scheme/gui/base
         (prefix-in error: htdp/error)
         htdp/image
         
	 (only-in htdp/world nw:rectangle place-image empty-scene scene+line )
         mrlib/cache-image-snip
         lang/prim
         (for-syntax scheme/base)
	 (planet dyoo/version-case:1:8)
         "../collects/moby/runtime/effect-struct.ss"
         "world-effects.ss")

(version-case 
 [(version<= (version) "4.2.1")
  (require (only-in lang/htdp-beginner image?))]
 [else
  (void)])


(require mrlib/gif)
(require mzlib/runtime-path)

(require mrlib/bitmap-label
         string-constants)

;; --- provide ---------------------------------------------------------------


;                                                   
;                                                   
;   ;;;;                         ;         ;        
;   ;   ;                                  ;        
;   ;   ;                                  ;        
;   ;   ;  ; ;;;   ;;;   ;   ; ;;;      ;;;;   ;;;  
;   ;  ;   ;;  ;  ;   ;  ;   ;   ;     ;   ;  ;   ; 
;   ;;;    ;      ;   ;   ; ;    ;     ;   ;  ;;;;; 
;   ;      ;      ;   ;   ; ;    ;     ;   ;  ;     
;   ;      ;      ;   ;   ; ;    ;     ;  ;;  ;     
;   ;      ;       ;;;     ;     ;;;    ;; ;   ;;;; 
;                                                   
;                                                   
;                                                   


;; image manipulation functions:
;; =============================
(provide (all-from-out htdp/image))

(provide
 ;; Scene is Image with pinhole in origin 
 nw:rectangle ;; Number Number Mode Color -> Image
 place-image  ;; Image Number Number Scene -> Scene
 empty-scene  ;; Number Number -> Scene 
 scene+line   ;; Scene Number Number Number Number Color -> Scene 
 ;; cut all pieces that are outside the given rectangle 
 )

;; world manipulation functions: 
;; =============================
(provide      ;; forall(World):
 big-bang	;; Number Number World [Boolean] -> true
 )


;; KeyEvent is one of: 
;; -- Char 
;; -- Symbol 

(provide
 key-event? ;; Any -> Boolean
 key=? ;; KeyEvent KeyEvent -> Boolean
 )

;; A MouseEventType is one of:
;; - 'button-down
;; - 'button-up
;; - 'drag
;; - 'move
;; - 'enter
;; - 'leave



(provide-higher-order-primitive
 run-simulation (_ _ _ create-scene) ; (Number Number Number (Nat -> Scene) -> true)
 )

(provide
 run-movie ;; [Listof Image] -> true 
 )


;                                                                        
;                                                                        
;   ;;;;;                                           ;  ;  ;;;;         ; 
;   ;                                    ;          ;  ;  ;  ;         ; 
;   ;                                    ;          ;  ;  ;  ;         ; 
;   ;      ;  ;   ; ;;    ;;;   ; ;;;  ;;;;;        ; ; ; ;  ;      ;;;; 
;   ;;;;;  ;  ;   ;;  ;  ;   ;  ;;  ;    ;          ; ; ; ;  ;     ;   ; 
;   ;       ;;    ;   ;  ;   ;  ;        ;           ;; ;;   ;     ;   ; 
;   ;       ;;    ;   ;  ;   ;  ;        ;           ;   ;   ;     ;   ; 
;   ;      ;  ;   ;   ;  ;   ;  ;        ;           ;   ;   ;     ;  ;; 
;   ;;;;;  ;  ;   ;;;;    ;;;   ;         ;;         ;   ;   ;;;    ;; ; 
;                 ;                                                      
;                 ;                                                      
;                                                                        

;; Number Number Number World [Boolean] -> true
;; create the visible world (canvas)
(define big-bang
  (lambda x 
    (define args (length x))
    (if (>= args 3)
        (apply big-bang0 x) 
        (error 'big-bang msg))))
(define msg
  (string-append
   "big-bang consumes at least 3 or 4 arguments:\n"
   "-- (big-bang <width> <height> <world0> handlers ...)\n"
   "-- (big-bang <width> <height> <world0> <animated-gif> handlers ...)\n"
   "see Help Desk."))
(define *running?* #f)
(define (big-bang0 w h world . args)
  (let ([animated-gif #f])
    (when (and (not (null? args))
               (boolean? (car args)))
      (set! animated-gif (car args))
      (set! args (cdr args)))
    
    (check-pos 'big-bang w "first")
    (check-pos 'big-bang h "second")
    ;; ============================================
    ;; WHAT IF THEY ARE NOT INTs?
    ;; ============================================
    (error:check-arg 'big-bang 
                     (boolean? animated-gif)
                     "boolean expected"
                     "fourth"
                     animated-gif)
    
    ;; fixme: improve error reporting.
    (for-each (lambda (x) 
                (error:check-arg 'big-bang 
                                 (procedure? x)
                                 "configuration option (on-tick, stop-when, ...) expected"
                                 "nth"
                                 x))
              args)
    
    
    (let ([w (coerce w)]
          [h (coerce h)])
      (when *running?*  (error 'big-bang "the world is still running"))
      (set! *running?* #t)
      (callback-stop!)
      ;; (when (vw-init?) (error 'big-bang "big-bang already called once"))
      (install-world world) ;; call first to establish a visible world
      (set-and-show-frame w h animated-gif) ;; now show it
      (unless animated-gif (set! add-event void)) ;; no recording if image undesired
      (for-each (lambda (x) (x)) args)
      (yield last-world-channel))))

;; Number -> Int 
(define (coerce x) (inexact->exact (floor x)))

(define *the-delta* 0.0)


(define (key-event? k)
  (or (char? k) (symbol? k)))

(define (key=? k m)
  (error:check-arg 'key=? (key-event? k) 'KeyEvent "first" k)
  (error:check-arg 'key=? (key-event? m) 'KeyEvent "first" m)
  (eqv? k m))


(define (run-movie movie)
  (error:check-arg 'run-movie (list? movie) "list (of images)" "first" movie)
  (for-each (lambda (cand) 
              (check-image 'run-movie cand "first" "list of images"))
            movie)
  (let* ([fst (car movie)]
         [wdt (image-width fst)]
         [hgt (image-height fst)])
    (big-bang wdt hgt (/ 1 27) movie)
    (let run-movie ([movie movie])
      (cond
        [(null? movie) #t]
        [(pair? movie)
         (update-frame (car movie))
         (sleep/yield .05)
         (run-movie (cdr movie))]))))

#;(define run-simulation 
    (lambda x 
      (define args (length x))
      (if (or (= args 5) (= args 4))
          (apply run-simulation0 x) 
          (error 'run-simulation msg-run-simulation))))
(define msg-run-simulation
  (string-append
   "consumes 4 or 5 arguments:\n"
   "-- (run-simulation <width> <height> <rate> <world-to-world-function>)\n"
   "-- (run-simulation <width> <height> <rate> <world-to-world-function> <create-animated-gif?>)\n"
   "see Help Desk."))


#;(define run-simulation0
    (case-lambda
      [(width height rate f record?)
       (error:check-pos 'run-simulation width "first")
       (error:check-pos 'run-simulation height "second")
       (error:check-arg 'run-simulation (number? rate) 'number "third" rate)
       (error:check-proc 'run-simulation f 1 "fourth" "one argument")
       (error:check-arg 'run-simulation (boolean? record?) 'number "fifth [and optional]" record?)
       (big-bang width height rate 1 record?)
       (on-redraw f)
       (on-tick add1)]
      [(width height rate f)
       (run-simulation width height rate f #f)]))

;; ---------------------------------------------------------------------------

;                                     
;                                     
;     ;;;  ;                    ;     
;    ;     ;                    ;     
;   ;      ;                    ;     
;   ;      ; ;;    ;;;    ;;;;  ;   ; 
;   ;      ;;  ;  ;   ;  ;      ;  ;  
;   ;      ;   ;  ;;;;;  ;      ; ;   
;   ;      ;   ;  ;      ;      ;;;   
;    ;     ;   ;  ;      ;      ;  ;  
;     ;;;  ;   ;   ;;;;   ;;;;  ;   ; 
;                                     
;                                     
;                                     

;; Symbol Any String -> Void
(define (check-pos tag c rank)
  (error:check-arg tag (and (number? c) (> (coerce c) 0))
                   "positive integer" rank c))

;; Symbol Any String String *-> Void
(define (check-image tag i rank . other-message)
  (if (and (pair? other-message) (string? (car other-message)))
      (error:check-arg tag (image? i) (car other-message) rank i)
      (error:check-arg tag (image? i) "image" rank i)))

;; Symbol Any String -> Void
(define (check-scene tag i rank)
  (if (image? i)
      (unless (scene? i)
        (error tag "scene expected, given image whose pinhole is at (~s,~s) instead of (0,0)"
               (pinhole-x i) (pinhole-y i)))
      (error:check-arg tag #f "image" rank i)))


(version-case 
 [(version< (version) "4.2.4")
  (define (scene? i) (and (= 0 (pinhole-x i)) (= 0 (pinhole-y i))))]
 [else
  (void)])


;; Symbol Any String -> Void
(define (check-color tag width rank)
  (error:check-arg tag (or (symbol? width) (string? width)) 
                   "color symbol or string" rank width))

;; Symbol (union Symbol String) Nat -> Void
(define (check-mode tag s rank)
  (error:check-arg tag (or (eq? s 'solid)
                           (eq? s 'outline)
                           (string=? "solid" s)
                           (string=? "outline" s)) "mode (solid or outline)" rank s))

;                                                                        
;                                                                        
;   ;;;;;                                     ;;;;;                      
;     ;                                       ;                          
;     ;                                       ;                          
;     ;   ;;; ;    ;;;;   ;;;;   ;;;          ;      ;   ;  ; ;;    ;;;  
;     ;   ; ;;;   ;   ;  ;   ;  ;   ;         ;;;;;  ;   ;  ;;  ;  ;   ; 
;     ;   ; ; ;   ;   ;  ;   ;  ;;;;;         ;      ;   ;  ;   ;   ;;   
;     ;   ; ; ;   ;   ;  ;   ;  ;             ;      ;   ;  ;   ;     ;  
;     ;   ; ; ;   ;  ;;  ;  ;;  ;             ;      ;  ;;  ;   ;  ;   ; 
;   ;;;;; ; ; ;    ;; ;   ;; ;   ;;;;         ;       ;; ;  ;   ;   ;;;  
;                            ;                                           
;                        ;;;;                                            
;                                                                        

;; Image Number Number Image -> Image 
#;
(define (place-image0 image x y scene)
  (define sw (image-width scene))
  (define sh (image-height scene))
  (define ns (overlay/xy scene x y image))
  (define nw (image-width ns))
  (define nh (image-height ns))
  (if (and (= sw nw) (= sh nh)) ns (shrink ns 0 0 sw sh)))

(define (place-image0 image x y scene)
  (define sw (image-width scene))
  (define sh (image-height scene))
  (define ns (overlay/xy scene x y image))
  (define nw (image-width ns))
  (define nh (image-height ns))
  (if (and (= sw nw) (= sh nh)) ns (shrink ns 0 0 (- sw 1) (- sh 1)))) 

;; Image Number Number Number Number Color -> Image
(define (add-line-to-scene0 img x0 y0 x1 y1 c)
  (define w (image-width img))  
  (define h (image-height img))
  (cond
    [(and (<= 0 x0 w) (<= 0 x1 w) (<= 0 y0 w) (<= 0 y1 w))
     (shrink (add-line img x0 y0 x1 y1 c) 0 0 (- w 1) (- h 1))]
    [(= x0 x1) ;; vertical that leaves bounds 
     (if (<= 0 x0 w) (add-line img x0 (app y0 h) x0 (app y1 h) c) img)]
    [(= y0 y1) ;; horizontal that leaves bounds 
     (if (<= 0 y0 h) (add-line img (app x0 w) y0 (app x1 w) y0 c) img)]
    [else 
     (local ((define lin (points->line x0 y0 x1 y1))
             (define dir (direction x0 y0 x1 y1))
             (define-values (upp low lft rgt) (intersections lin w h))
             (define (add x y) (add-line img x0 y0 x y c)))
       (cond
         [(and (< 0 x0 w) (< 0 y0 h)) ;; (x0,y0) is in the interior
          (case dir
            [(upper-left)  (if (number? upp) (add upp 0) (add 0 lft))]
            [(lower-left)  (if (number? low) (add low h) (add 0 lft))]
            [(upper-right) (if (number? upp) (add upp 0) (add w rgt))]
            [(lower-right) (if (number? low) (add low h) (add w rgt))]
            [else (error 'dir "contract violation: ~e" dir)])]
         [(and (< 0 x1 w) (< 0 y1 h)) ;; (x1,y1) in interior; symmetry!
          (add-line-to-scene0 img x1 y1 x0 y0 c)]
         [else 
          (cond
            [(and (number? upp) (number? low)) (add-line img upp 0 low h c)]
            [(and (number? upp) (number? lft)) (add-line img upp 0 0 lft c)]
            [(and (number? upp) (number? rgt)) (add-line img upp 0 w rgt c)]
            [(and (number? low) (number? lft)) (add-line img low h 0 lft c)]
            [(and (number? low) (number? rgt)) (add-line img low h w rgt c)]
            [(and (number? lft) (number? rgt)) (add-line img 0 lft w rgt c)]
            [else img])]))]))
;; Nat Nat -> Nat 
;; y if in [0,h], otherwise the closest boundary
(define (app y h)
  (cond
    [(and (<= 0 y) (< y h)) y]
    [(< y 0) 0]
    [else (- h 1)]))

;; Nat Nat Nat Nat -> (union 'upper-left 'upper-right 'lower-left 'lower-right)
;; how to get to (x1,y1) from (x0,y0)
(define (direction x0 y0 x1 y1)
  (string->symbol
   (string-append 
    (if (<= y0 y1) "lower" "upper") "-" (if (<= x0 x1) "right" "left"))))

#| TESTS
'direction 
(equal? (direction 10 10 0 0) 'upper-left)
(equal? (direction 10 10 20 20) 'lower-right)
(equal? (direction 10 10 0 20) 'lower-left)
(equal? (direction 10 10 20 0) 'upper-right)
|#

;; -----------------------------------------------------------------------------
;; LINEs 

;; Number Number -> LINE
;; create a line from a slope and the intersection with the y-axis
(define-struct lyne (slope y0))

;; Nat Nat Nat Nat -> LINE
;; determine the line function from the four points (or the attributes)
;; ASSUME: (not (= x0 x1))
(define (points->line x0 y0 x1 y1)
  (local ((define slope  (/ (- y1 y0) (- x1 x0))))
    (make-lyne slope (- y0 (* slope x0)))))

;; LINE Number -> Number 
(define (of ln x) (+ (* (lyne-slope ln) x) (lyne-y0 ln)))

;; LINE Nat Nat -> [Opt Number] [Opt Number] [Opt Number] [Opt Number]
;; where does the line intersect the rectangle [0,w] x [0,h]
;; (values UP LW LF RT) means the line intersects with 
;;  the rectangle [0,w] x [0,h] at (UP,0) or (LW,h) or (0,LF) or (w,RT)
;;  when a field is false, the line doesn't interesect with that side 
(define (intersections l w h)
  (values
   (opt (X l 0) w) (opt (X l h) w) (opt (lyne-y0 l) h) (opt (of l w) h)))

;; Number Number -> [Opt Number]
(define (opt z lft) (if (<= 0 z lft) z false))

;; LINE Number -> Number 
;; the x0 where LINE crosses y(x) = h
;; assume: LINE is not a horizontal
(define (X ln h) (/ (- h (lyne-y0 ln)) (lyne-slope ln)))

;; --- TESTS --- 
#|
(define line1 (points->line 0 0 100 100))
(= (of line1 0) 0)
(= (of line1 100) 100)
(= (of line1 50) 50)

(= (X (make-lyne 1 0) 0) 0)
(= (X (make-lyne 1 0) 100) 100)

(equal? (call-with-values 
         (lambda () (intersections (points->line -10 -10 110 110) 100 100))
         list)
        (list 0 100 0 100))
(equal? (call-with-values 
         (lambda () (intersections (points->line 0 10 100 80) 100 100))
         list)
        (list false false 10 80))
|#
;; ---------------------------------------------------------------------------


;                                     
;                                     
;  ;  ;  ;              ;;;         ; 
;  ;  ;  ;                ;         ; 
;  ;  ;  ;                ;         ; 
;  ; ; ; ;  ;;;   ; ;;;   ;      ;;;; 
;  ; ; ; ; ;   ;  ;;  ;   ;     ;   ; 
;   ;; ;;  ;   ;  ;       ;     ;   ; 
;   ;   ;  ;   ;  ;       ;     ;   ; 
;   ;   ;  ;   ;  ;       ;     ;  ;; 
;   ;   ;   ;;;   ;       ;;;    ;; ; 
;                                     
;                                     
;                                     

(define unique-world (cons 1 1))
(define (check-world tag)
  (when (eq? unique-world the-world) 
    (error tag "evaluate (big-bang Number Number Number World) first")))

(define last-world-channel (make-channel))

(define the-world unique-world)
(define the-world0 unique-world)

;; Nat World -> Void
;; effects: init event-history, the-delta, the-world, the-world0
(define (install-world w)
  (reset-event-history)
  (set! the-world w)
  (set! the-world0 w)
  (vw-setup))



;; Text-- The One and Only Visible World
(define visible-world #f)

;; Bool -> Void
(define (vw-setup)
  (set! visible-world (new pasteboard%))
  (send visible-world set-cursor (make-object cursor% 'arrow)))

;; -> Boolean 
(define (vw-init?) (is-a? visible-world pasteboard%))

;; Image -> Void
;; show the image in the visible world
(define (update-frame pict)
  (send visible-world begin-edit-sequence)
  (send visible-world lock #f)
  (let ([s (send visible-world find-first-snip)])
    (when s
      (send visible-world delete s)))
  (let ([c (send visible-world get-canvas)])
    (let-values ([(px py)
                  (if (is-a? pict cache-image-snip%)
                      (send pict get-pinhole)
                      (values 0 0))]
                 [(cw ch)
                  (send c get-client-size)])
      (send visible-world insert (send pict copy) (- px) (- py))))
  (send visible-world lock #t)
  (send visible-world end-edit-sequence))

;; Nat Nat Boolean -> Void
;; effect: create, show and set the-frame
;; assume: visible-world is a pasteboard%, i.e., install-world has been called. 
(define (set-and-show-frame w h animated-gif)
  (define the-play-back-custodian (make-custodian))
  (define frame (create-frame the-play-back-custodian))
  (set! WIDTH w)
  (set! HEIGHT h)
  (when animated-gif
    (add-stop-and-image-buttons frame the-play-back-custodian))
  (add-editor-canvas frame visible-world w h)
  (send frame show #t))

(define WIDTH 0) 
(define HEIGHT 0)

;; [Box (union false Thread)] -> Frame
;; create a frame that shuts down the custodian on close
(define (create-frame the-play-back-custodian)
  (new (class frame%
         (super-new)
         (define/augment (on-close)  
           (callback-stop!)
           (thread (lambda () (channel-put last-world-channel the-world)))
           (custodian-shutdown-all the-play-back-custodian)))
       (label "DrScheme")
       (stretchable-width #f)
       (stretchable-height #f)
       (style '(no-resize-border metal))))

;; Frame [Box (union false Thread)] -> Void
;; adds the stop animation and image creation button, 
;; whose callbacks runs as a thread in the custodian
(define IMAGES "Images")
(define-runtime-path s:pth '(lib "icons/break.png"))
(define-runtime-path i:pth '(lib "icons/file.gif"))
(define (add-stop-and-image-buttons  frame the-play-back-custodian)
  (define p (new horizontal-pane% [parent frame][alignment '(center center)]))
  (define S ((bitmap-label-maker (string-constant break-button-label) s:pth) '_))
  (define I ((bitmap-label-maker IMAGES i:pth) '_))
  (define stop-button
    (new button% [parent p] [label S] [style '(border)]
         [callback (lambda (this-button e) 
                     (callback-stop!)
                     (send this-button enable #f)
                     (send image-button enable #t))]))
  (define image-button 
    (new button% [parent p] [enabled #f] [label I] [style '(border)]
         [callback (lambda (b e)
                     (parameterize ([current-custodian the-play-back-custodian])
                       (define th (thread play-back))
                       (send b enable #f)))]))
  (void))

;; Frame Editor Nat Nat -> Void
;; adds the visible wold to the frame and hooks it up with the callbacks
(define (add-editor-canvas frame visible-world w h)
  (define c 
    (new (class editor-canvas%
           (super-new)
           (define/override (on-char e) 
             (key-effect-callback (send e get-key-code))
             (key-callback (send e get-key-code)))
           (define/override (on-event e) 
             (mouse-effect-callback e)
             (mouse-callback e)))
         (parent frame)
         (editor visible-world)
         (style '(no-hscroll no-vscroll))
         (horizontal-inset INSET)
         (vertical-inset INSET)))
  (send c min-client-width (+ w INSET INSET))
  (send c min-client-height (+ h INSET INSET))
  (send c focus))

;; Amount of space around the image in the world window:
(define INSET 5)

;                                                                                      
;                                                                                      
;  ;;;;;;                                    ;;;;;                                  ;; 
;   ;   ;                        ;            ;   ;                                  ; 
;   ; ;   ;;; ;;;  ;;;  ;; ;;   ;;;;;         ;   ;   ;;;    ;;;;   ;;;   ;; ;;   ;; ; 
;   ;;;    ;   ;  ;   ;  ;;  ;   ;            ;   ;  ;   ;  ;   ;  ;   ;   ;;    ;  ;; 
;   ; ;    ;   ;  ;;;;;  ;   ;   ;            ;;;;   ;;;;;  ;      ;   ;   ;     ;   ; 
;   ;       ; ;   ;      ;   ;   ;            ;  ;   ;      ;      ;   ;   ;     ;   ; 
;   ;   ;   ; ;   ;      ;   ;   ;   ;        ;   ;  ;      ;   ;  ;   ;   ;     ;   ; 
;  ;;;;;;    ;     ;;;; ;;; ;;;   ;;;        ;;;   ;  ;;;;   ;;;    ;;;   ;;;;;   ;;;;;
;                                                                                      
;                                                                                      
;                                                                                      
;                                                                                      

(define TICK 'tick)
(define MOUSE 'mouse)
(define KEY 'key)
;; Evt =   (list utick) 
;;       | (list KEY (union Char Symbol)) 
;;       | (list MOUSE MouseEventType)
;; [Listof Evt]
(define event-history '())

;; -> Void 
(define (reset-event-history)
  (set! event-history '()))

;; Symbol  Any *-> Void
(define (add-event type . stuff)
  (set! event-history (cons (cons type stuff) event-history)))


;; zfill: natural-number natural-number -> string
;; Converts a-num to a string, adding leading zeros to make it at least as long as a-len.
(define (zfill a-num a-len)
  (let ([n (number->string a-num)])
    (string-append (build-string (max (- a-len (string-length n)) 0)
                                 (lambda (i) #\0))
                   n)))

;; --> Void
;; re-play the history of events, creating a png per step, create animated gif
;; effect: write to user-chosen file 
(define (play-back)
  ;; --- state transitions 
  (define (world-transition world fst)
    (case (car fst)
      [(tick)  (timer-callback0 world)]
      [(key)   (key-callback0 world (cadr fst))]
      [(mouse) (mouse-callback0 world (cadr fst) (caddr fst) (cadddr fst))]
      [else (error 'play-back "bad type of event: ~s" fst)]))
  ;; --- creating images 
  (define total (+ (length event-history) 1))
  (define image-count 0)
  (define bitmap-list '())
  (define (save-image img)
    (define-values (w h) (send img get-size)) 
    (define (make-bitmap)
      (define bm (make-object bitmap% w h))
      (define dc (make-object bitmap-dc% bm))
      (send dc clear)
      (send img draw dc 0 0 0 0 w h 0 0 #f)
      bm)
    (define bm (make-bitmap)) 
    (set! bitmap-list (cons bm bitmap-list))
    (set! image-count (+ image-count 1))
    (send bm save-file (format "i~a.png" (zfill image-count (string-length (number->string total)))) 'png))
  ;; --- choose place 
  (define target:dir
    (let* ([cd (current-directory)]
           [dd (get-directory "Select directory for images" #f cd)])
      (if dd dd cd)))
  (parameterize ([current-directory target:dir])
    (let replay ([ev (reverse event-history)][world the-world0])
      (define img (redraw-callback0 world))
      (update-frame (text (format "~a/~a created" image-count total) 18 'red))
      (save-image img)
      (cond
        [(null? ev)
         (update-frame (text (format "creating ~a" ANIMATED-GIF-FILE) 18 'red))
         (create-animated-gif (reverse bitmap-list))
         (update-frame img)]
        [else
         (let ([world1 (world-transition world (car ev))])
           (replay (cdr ev) world1))]))))

;; [Listof (-> bitmap)] -> Void
;; turn the list of thunks into animated gifs 
;; effect: overwrite the ANIMATED-GIF-FILE (in current directory)
(define (create-animated-gif bitmap-list)
  (define intv (if (> +inf.0 *the-delta* 0) (inexact->exact (floor (* 100 *the-delta*))) 5))
  (when (file-exists? ANIMATED-GIF-FILE)
    (delete-file ANIMATED-GIF-FILE))
  (write-animated-gif bitmap-list intv ANIMATED-GIF-FILE #:one-at-a-time? #t #:loop? #f))

(define ANIMATED-GIF-FILE "i-animated.gif")


;                                                                 
;                                                                 
;     ;;;        ;;;    ;;;     ;                    ;            
;    ;             ;      ;     ;                    ;            
;   ;              ;      ;     ;                    ;            
;   ;       ;;;;   ;      ;     ; ;;    ;;;;   ;;;;  ;   ;   ;;;  
;   ;      ;   ;   ;      ;     ;;  ;  ;   ;  ;      ;  ;   ;   ; 
;   ;      ;   ;   ;      ;     ;   ;  ;   ;  ;      ; ;     ;;   
;   ;      ;   ;   ;      ;     ;   ;  ;   ;  ;      ;;;       ;  
;    ;     ;  ;;   ;      ;     ;   ;  ;  ;;  ;      ;  ;   ;   ; 
;     ;;;   ;; ;   ;;;    ;;;   ;;;;    ;; ;   ;;;;  ;   ;   ;;;  
;                                                                 
;                                                                 
;                                                                 

;; callbacks: timer, mouse, key, redraw, stop-when 

;; Definition = (define-callback Symbol String Symbol Expression ...)
;; effect: (define-callback introduces three names: name, name0, set-name
(define-syntax (define-callback stx)
  (syntax-case stx ()
    [(_ n msg (f esp ...) para body ...)
     (let* ([n:str (symbol->string (syntax-e (syntax n)))]
            [callback (lambda (before after)
                        (string->symbol 
                         (string-append before n:str "-callback" after)))]
            [name (datum->syntax stx (callback "" ""))]
            [name0 (datum->syntax stx (callback "" "0"))]
            [set-name (datum->syntax stx (callback "set-" ""))])
       #`(define-values (#,name #,name0 #,set-name)
           (values 
            void void 
            (lambda (f esp ...)
              #;
              (when (callback-set? #,name) 
                (error (format "the ~a has already been specified") msg))
              (set! #,name0 f)
              (set! #,name (lambda para body ...))))))]))

;; -> Void
(define (callback-stop!)
  (send the-time stop)
  (set! timer-callback void)
  (set! mouse-callback void)
  (set! key-callback void)
  (set! stop-when-callback (lambda () #f))
  (set! redraw-callback void)
  (set! *running?* #f))

;; Any -> Boolean
;; is the callback set to the default value 
(define (callback-set? cb) (not (eq? cb void)))

;; Timer
(define the-time (new timer% [notify-callback (lambda () 
                                                (timer-effect-callback)
                                                (timer-callback))]))

;; f : [World -> World]
(define-callback timer "tick-event hander" (f) ()
  (with-handlers ([exn:break? break-handler][exn? exn-handler])
    (set! the-world (f the-world))
    (add-event TICK)
    (redraw-callback)))


;; f: [World -> Effect]
(define-callback timer-effect "tick-effect event hander" (f) ()
  (with-handlers ([exn:break? break-handler][exn? exn-handler])
    (let ([an-effect (f the-world)])
      (effect-apply! an-effect))))




;; f : [World -> Image]
(define-callback redraw "redraw function" (f) ()
  (with-handlers ([exn:break? break-handler][exn? exn-handler])
    (define result (f the-world))
    (define fname (object-name f))
    (define tname (if fname fname 'your-redraw-function))
    (if (image? result)
        (error:check-result tname scene? "scene" result
                            (format "image with pinhole at (~s,~s)"
                                    (pinhole-x result) (pinhole-y result)))
        (error:check-result tname (lambda (x) (image? x)) "scene" result))
    (update-frame result)
    ;; if this world is the last one, stop the world
    (when (stop-when-callback)
      (callback-stop!)
      (thread (lambda () (channel-put last-world-channel the-world))))))

;; f : [World -> Boolean]
(define-callback stop-when "is end of world check" (f) ()
  (define result (f the-world))
  (define fname (object-name f))
  (define tname (if fname fname 'your-redraw-function))
  (error:check-result fname boolean? "boolean" result)
  result)

;; f : [World KeyEvent -> World]
;; esp : EventSpace 
;; e : KeyEvent 
(define-callback key "key-event handler" (f evt-space) (e)
  (parameterize ([current-eventspace evt-space])
    (queue-callback 
     (lambda ()
       (with-handlers ([exn:break? break-handler][exn? exn-handler])
         (let ([new-world (f the-world e)])
           (unless (equal? new-world the-world)
             (set! the-world new-world)
             (add-event KEY e)
             (redraw-callback))))))))

(define-callback key-effect "key-effect event handler" (f evt-space) (e)
  (parameterize ([current-eventspace evt-space])
    (queue-callback 
     (lambda ()
       (with-handlers ([exn:break? break-handler][exn? exn-handler])
         (let ([new-effect (f the-world e)])
           (effect-apply! new-effect)))))))



;; f : [World Nat Nat MouseEventType -> World]
;; esp : EventSpace 
;; e : MouseEvent
(define-callback mouse "mouse event handler" (f evt-space) (e)
  (parameterize ([current-eventspace evt-space])
    (queue-callback
     (lambda ()
       (define x (- (send e get-x) INSET))
       (define y (- (send e get-y) INSET))
       (define m (mouse-event->symbol e))
       (when (and (<= 0 x WIDTH) (<= 0 y HEIGHT))
         (with-handlers ([exn:break? break-handler][exn? exn-handler])
           (let ([new-world (f the-world x y m)])
             (unless (equal? new-world the-world)
               (set! the-world new-world)
               (add-event MOUSE x y m)
               (redraw-callback)))))))))

(define-callback mouse-effect "mouse event handler" (f evt-space) (e)
  (parameterize ([current-eventspace evt-space])
    (queue-callback
     (lambda ()
       (define x (- (send e get-x) INSET))
       (define y (- (send e get-y) INSET))
       (define m (mouse-event->symbol e))
       (when (and (<= 0 x WIDTH) (<= 0 y HEIGHT))
         (with-handlers ([exn:break? break-handler][exn? exn-handler])
           (let ([new-effect (f the-world x y m)])
             (effect-apply! new-effect))))))))


;; MouseEvent -> MouseEventType
(define (mouse-event->symbol e)
  (cond [(send e button-down?) 'button-down]
        [(send e button-up?)   'button-up]
        [(send e dragging?)    'drag]
        [(send e moving?)      'move]
        [(send e entering?)    'enter]
        [(send e leaving?)     'leave]
        [else ; (send e get-event-type)
         (error 'on-mouse
                (format 
                 "Unknown event type: ~a"
                 (send e get-event-type)))]))

;; --- library 
(define (exn-handler e)
  (callback-stop!)
  (raise e))

(define (break-handler . _) 
  (printf "animation stopped")
  (callback-stop!)
  the-world)

;; Number -> Integer
(define (number->integer x)
  (inexact->exact (floor x)))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;                                          
;                                          
;                                          
;     ;;;;                         ;       
;    ;;  ;;     ;;                 ;       
;   ;;          ;;                 ;       
;   ;;        ;;;;;;;    ;    ;    ; ;;;   
;   ;;          ;;       ;    ;    ;;  ;;  
;    ;;;;       ;;       ;    ;    ;    ;  
;      ;;;;     ;;       ;    ;    ;    ;; 
;         ;     ;;       ;    ;    ;    ;; 
;         ;;    ;;       ;    ;    ;    ;; 
;         ;     ;;       ;    ;    ;    ;  
;   ;;   ;;      ;       ;;  ;;    ;;  ;;  
;    ;;;;;       ;;;;     ;;; ;    ; ;;;   
;                                          
;                                          
;                                          
;                                          



(define (on-tick the-delta f)
  (on-tick* the-delta f (lambda (w) (make-effect:none))))


(define (on-tick* the-delta f-world f-effect)
  (error:check-arg 'on-tick
                   (and (number? the-delta) (<= 0 the-delta 1000))
                   "number [of seconds] between 0 and 1000"
                   "first"
                   the-delta)
  (error:check-proc 'on-tick f-world 1 "on-tick" "one argument")
  (lambda ()
    (set! *the-delta* the-delta)
    (set-timer-callback f-world)
    (set-timer-effect-callback f-effect)
    (send the-time start
          (let* ([w (ceiling (* 1000 the-delta))])
            (if (exact? w) w (inexact->exact w))))
    #t))



(define (on-redraw f)
  (error:check-proc 'on-redraw f 1 "on-redraw" "one argument")
  (lambda ()
    (set-redraw-callback f)
    (redraw-callback)
    #t))


(define (on-key f)
  (on-key* f (lambda (w k)
               (make-effect:none))))

(define (on-key* f f-effect)
  (error:check-proc 'on-key f 2 "on-key" "two arguments")
  (lambda ()
    (set-key-callback f (current-eventspace))
    (set-key-effect-callback f-effect (current-eventspace))
    #t))

(define (on-mouse f)
  (on-mouse* f (lambda (w x y b)
                 (make-effect:none))))


(define (on-mouse* f f-effect)
  (error:check-proc 'on-mouse f 4 "on-mouse" "four arguments")
  (lambda ()
    (set-mouse-callback f (current-eventspace))
    (set-mouse-effect-callback f-effect (current-eventspace))
    #t))


(define (stop-when f)
  (error:check-proc 'stop-when f 1 "stop-when" "one argument")
  (lambda ()
    (set-stop-when-callback f)
    #t))


(define (on-tilt handler)
  (lambda ()
    ;; fixme
    #t))

(define (on-tilt* handler effect-handler)
  (lambda ()
    ;; fixme
    #t))

(define (on-acceleration handler)
  (lambda ()
    ;; fixme
    #t))

(define (on-acceleration* handler effect-handler)
  (lambda ()
    ;; fixme
    #t))

(define (on-shake handler)
  (lambda ()
    ;; fixme
    #t))

(define (on-shake* handler effect-handler)
  (lambda ()
    ;; fixme
    #t))


(define (on-location-change f)
  (on-location-change* f (lambda (w x y) (make-effect:none))))


(define (on-location-change* f effect-f)
  (error:check-proc 'on-location-change f 3 "on-location-change" 
                    "three arguments")
  (error:check-proc 'on-location-change effect-f 3 "on-location-change" 
                    "three arguments")
  (lambda ()
    (set-location-callback f (current-eventspace))
    (set-location-effect-callback effect-f (current-eventspace))
    (show-location-gui)
    #t))

;; f : [World KeyEvent -> World]
;; esp : EventSpace 
;; e : KeyEvent 
(define-callback location "location-change handler" (f evt-space) 
  (lat long)
  (parameterize ([current-eventspace evt-space])
    (queue-callback 
     (lambda ()
       (with-handlers ([exn:break? break-handler][exn? exn-handler])
         (let ([new-world (f the-world lat long)])
           (unless (equal? new-world the-world)
             (set! the-world new-world)
             (redraw-callback))))))))


(define-callback location-effect "location-change handler" (f evt-space) 
  (lat long)
  (parameterize ([current-eventspace evt-space])
    (queue-callback 
     (lambda ()
       (with-handlers ([exn:break? break-handler][exn? exn-handler])
         (let ([new-effect (f the-world lat long)])
           (effect-apply! new-effect)))))))



(define (show-location-gui)
  (define a-frame (new frame%
                       [label "Location Stub"]))
  (define t-x (new text-field% 
                   [parent a-frame]
                   [label "lat"]
                   [init-value "0"]))
  
  (define t-y (new text-field% 
                   [parent a-frame]
                   [label "long"]
                   [init-value "0"]))
  
  (define b (new button% 
                 [parent a-frame]
                 [label "Set!"]
                 [callback (lambda (b e)
                             (let ([x (string->number
                                       (send t-x get-value))]
                                   [y (string->number
                                       (send t-y get-value))])
                               (location-effect-callback x y)
                               (location-callback x y)))]))
  (send a-frame show #t))





;; open-image-url: string -> snip
(define (open-image-url a-url)
  (let* ([bm (make-object bitmap% 1 1)]
         [a-snip (make-object image-snip% bm)])
   a-snip))



(provide open-image-url)

;
;
;(provide-higher-order-primitive
; on-tick (_ tock) ;; Number (World -> World) -> true
; )
;
;(provide-higher-order-primitive
; on-tick* (_ tock event-tock) ;; Number (World -> World) (World->Event) -> true
; )
;
;
;(provide-higher-order-primitive
; on-redraw (world-to-image) ;; (World -> Image) -> true
; )
;
;(provide-higher-order-primitive
; on-key (control) ;; (World KeyEvent -> World) -> true
; )
;
;(provide-higher-order-primitive
; on-key* (control key-effect) ;; (World KeyEvent -> World) -> true
; )
;
;
;(provide-higher-order-primitive
; on-mouse (clack)  ;; (World Number Number MouseEvent -> World) -> true
; )
;
;(provide-higher-order-primitive
; on-mouse* (clack mouse-effect)  ;; (World Number Number MouseEvent -> World) -> true
; )
;
;(provide-higher-order-primitive
; stop-when (last-world)  ;; (World -> Boolean) -> true
; )
;
;
;;; handler: World number number -> world
;(provide-higher-order-primitive on-location-change (handler))
;
;;; handler: World number number -> world
;;; effect-handler: World number number -> Effect
;(provide-higher-order-primitive on-location-change* (handler effect-handler))
;
;;; handler: World number number number -> World
;(provide-higher-order-primitive on-tilt (handler))
;
;;; handler: World number number number -> World
;;; effect-handler: World number number number -> Effect
;(provide-higher-order-primitive on-tilt* (handler effect-handler))
;
;;; handler: World number number number -> World
;(provide-higher-order-primitive on-acceleration (handler))
;
;;; handler: World number number number -> World
;;; effect-handler: World number number number -> Effect
;(provide-higher-order-primitive on-acceleration* (handler effect-handler))
;
;(provide-higher-order-primitive on-shake (handler))
;(provide-higher-order-primitive on-shake* (handler effect-handler))


;; FIXME: changes to location or tilt should reflect on the world.