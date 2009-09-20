#lang s-exp "../moby-lang.ss"

;; Go (Baduk) game clock.

;; A player is a:
;;
;; (make-player time in-byo-yomi? periods)
;;
;; where time is a number representing the amount
;; of time in milliseconds, in-byo-yomi? is a boolean,
;; and periods is a number representing the number
;; of byo-yomi periods.
(define-struct player (time in-byo-yomi? periods))


;; TIME-DELTA is the amount of time between clock ticks
;; in seconds.
(define TIME-DELTA (/ 1 10))


;; a world is a:
;;
;; (make-world black white turn started?)
;;
;; where black and white are players,
;; turn is one of (list 'black 'white)
;; and running? is a boolean indicated whether or not
;; the clock should be running.
(define-struct world (black white turn running?))


;; make-initial-world: -> world
;; Produces the initial world state.
(define (make-initial-world)
  (make-world 
   (make-player (seconds->milliseconds 30) false 5)
   (make-player (seconds->milliseconds 30) false 5)
   'black
   true #;false))


;; update-player-time: player number -> player
;; Updates the time of a player.
(define (update-player-time p t)
  (make-player t
               (player-in-byo-yomi? p)
               (player-periods p)))


;; tick: world -> world
;; Applies a clock tick to whoever's clock is running.
(define (tick w)
  (cond
    [(world-running? w)
     (make-world (cond [(symbol=? (world-turn w) 'black)
                        (tick-player (world-black w))]
                       [(symbol=? (world-turn w) 'white)
                        (world-black w)])
                 
                 (cond [(symbol=? (world-turn w) 'black)
                        (world-white w)]
                       [(symbol=? (world-turn w) 'white)
                        (tick-player (world-white w))])
                 (world-turn w)
                 (world-running? w))]
    [else
     w]))


;; tick-player: player -> player
;; Applies the clock tick to the player's clock.
(define (tick-player p)
  (cond [(<= (- (player-time p)
                (seconds->milliseconds TIME-DELTA))
             0)
         (cond [(player-in-byo-yomi? p)
                (cond [(> (player-periods p) 0)
                       (make-player
                        BYO-YOMI-MILLISECONDS
                        (player-in-byo-yomi? p)
                        (sub1 (player-periods p)))]
                      [else
                       (make-player 0 true 0)])]
               [else
                (make-player BYO-YOMI-MILLISECONDS
                             true 
                             (sub1 (player-periods p)))])]
        [else
         (update-player-time 
          p
          (max 0
               (- (player-time p)
                  (seconds->milliseconds TIME-DELTA))))]))


;; game-over?: world -> boolean
;; Produces true when the game's over.
(define (game-over? w)
  (or (player-loses? (world-black w))
      (player-loses? (world-white w))))


;; player-loses?: player -> player
;; Produces true when the player's clock is exhausted.
(define (player-loses? p)
  (and (= (player-time p) 0)
       (player-in-byo-yomi? p)
       (= (player-periods p) 0)))


;; player-players: player -> player
;; Updates the clock whenever the player plays.  In
;; byo-yomi, making a play will cause the time to reset
;; back to one minute.
(define (player-plays p)
  (cond
    [(player-in-byo-yomi? p)
     (update-player-time p
                         (minutes->milliseconds 1))]
    [else
     p]))


;; someone-plays: world -> world
(define (someone-plays w)
  (cond
    [(symbol=? (world-turn w) 'black)
     (make-world (player-plays (world-black w))
                 (world-white w)
                 'white
                 (world-running? w))]
    [(symbol=? (world-turn w) 'white)
     (make-world (world-black w)
                 (player-plays (world-white w))
                 'black
                 (world-running? w))]))
  


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; HELPERS
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; minutes->seconds: number -> number
;; Converts minutes to seconds.
(define (minutes->seconds m)
  (* m 60))

;; seconds->milliseconds: number number -> number
;; Converts seconds to milliseconds
(define (seconds->milliseconds s)
  (* s 1000))


;; minutes->milliseconds: number -> number
;; converts minutes to milliseconds.
(define (minutes->milliseconds m)
  (seconds->milliseconds (minutes->seconds m)))

;; milliseconds->seconds: number -> number
;; converts milliseconds to seconds.
(define (milliseconds->seconds m)
  (/ m 1000))

;; BYO-YOMI-MILLISECONDS: number
;; It's how much time is given per period.
(define BYO-YOMI-MILLISECONDS 
  (seconds->milliseconds 10))


;; time-string: number -> string
;; Given time in milliseconds, produces a nice string.
(define (time-string time)
  (format "~a:~a"
          (quotient (floor (milliseconds->seconds time))
                    60)
          (remainder (floor (milliseconds->seconds time))
                     60)))
          

;; player-display: player string string -> dom-sexp
(define (player-display p name)
  (list (js-div)
        (list (js-text name))
        (list (js-text (format "Periods: ~a" 
                               (player-periods p))))
        (list (js-text (time-string (player-time p))))))


(define THE-PLAY-BUTTON 
  (js-button someone-plays 
             '(("id" "the-play-button"))))


;; maybe-wrap-button: dom-sexp boolean -> dom-sexp
;; Conditionally adds a button around a-dom-sexp.
(define (maybe-wrap-button a-dom-sexp yes?)
  (cond
    [yes?
     (list THE-PLAY-BUTTON
           a-dom-sexp)]
    [else
     a-dom-sexp]))

;; draw: world -> dom-sexp
(define (draw w)
  (list (js-div '(("id" "main")))
         (list (js-div '(("id" "black-side")))
               (maybe-wrap-button
                (player-display (world-black w)
                                "black")
                (symbol=? (world-turn w) 'black)))
         (list (js-div '(("id" "white-side")))
               (maybe-wrap-button
                (player-display (world-white w) 
                                "white")
                (symbol=? (world-turn w) 'white)))))


;; draw-css: world -> css-sexp
(define (draw-css w)
  '(("main" ("border-style" "solid")
            ("width" "100%")
            ("height" "100%"))
    ("the-play-button" ("width" "100%")
                       ("height" "100%"))
    ("black-side" ("border-style" "solid")
                  ("width" "100%")
                  ("height" "50%"))
    ("white-side" ("border-style" "solid")
                  ("width" "100%")
                  ("height" "50%"))))

;; key: world key -> world
(define (key w k)
  (cond
    [(key=? k "space")
     (someone-plays w)]
    [else
     w]))


(js-big-bang (make-initial-world)
             
             (on-tick TIME-DELTA tick)
             (on-key key)
             
             (on-draw draw draw-css)
             (stop-when game-over?))