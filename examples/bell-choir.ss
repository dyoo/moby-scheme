#lang s-exp "../moby-lang.ss"
;; Bell ringing program for Moby.


;; The world is a number counting how many times we've been shaken, 
;; and the current tone.
(define-struct world (shaken tone))

(define MAX-TONE 4)
(define MIN-TONE 1)

(define initial-world (make-world 0 MIN-TONE))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; shake: world -> world
;; On a shake, first increment the world-shaken.
(define (shake a-world)
  (make-world (add1 (world-shaken a-world))
              (world-tone a-world)))

;; ring: world -> effect
;; Also, on a shake, produce a sound effect.
(define (ring a-world)
  (make-effect:play-sound 
   (string-append 
    "http://hashcollision.org/tones/"
    (number->letter (world-tone a-world))
    "-tone.wav")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; up: world -> world
(define (up a-world)
  (make-world (world-shaken a-world)
              (min (add1 (world-tone a-world))
                   MAX-TONE)))

;; down: world -> world
(define (down a-world)
  (make-world (world-shaken a-world)
              (max (sub1 (world-tone a-world))
                   MIN-TONE)))



(define button-up 
  (js-button up '(("id" "button-up"))))
(define button-down 
  (js-button down '(("id" "button-down"))))
(define button-ring 
  (js-button! shake ring '(("id" "button-ring"))))
(define background-div 
  (js-div '(("id" "background-div"))))



;; converts a number 1-4 into the letter corresponding to that tone
;; number->letter: number -> string
(define (number->letter num)
  (cond
    [(= num 1) "C"]
    [(= num 2) "D"]
    [(= num 3) "E"]
    [(= num 4) "G"]))

;; render: world -> (sexpof dom)
(define (render w)
  (list background-div
        (list (js-div)
              (list (js-text (number->string 
                              (world-shaken w))))
              (list (js-text " "))
              (list (js-text "rings")))
        (list (js-div '(("id" "note-text")))
              (list (js-text (number->letter
                              (world-tone w)))))
        (list button-up (list (js-text "Up")))
        (list button-down (list (js-text "Down")))
        (list button-ring (list (js-text "Play ring!")))))

;; render-css: world -> (sexpof css-style)
(define (render-css w)
  '(("background-div" ("background-color" "gray")
                      ("border" "solid")
                      ("text-align" "center")
                      ("width" "100%"))
    
    ("note-text" ("font-size" "30px"))
    
    ("button-up" ("width" "80%")
                 ("height" "50px")
                 ("margin" "5px"))
    ("button-down" ("width" "80%")
                   ("height" "50px")
                   ("margin" "5px"))
    ("button-ring" ("width" "80%")
                   ("height" "50px"))))



(js-big-bang initial-world
             (initial-effect (make-effect:set-sound-volume 90))
             (on-shake! shake ring)
             (on-draw render render-css))