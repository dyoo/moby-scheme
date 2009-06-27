;; The first three lines of this file were inserted by DrScheme. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname shake-to-dtmf) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ())))
;; DTMF ringing program for Moby.
(require (lib "moby.ss" "moby" "stub"))



(define WIDTH 320)
(define HEIGHT 480)
(define MAX-TONE 15)
(define MIN-TONE 0)


;; The world is a number counting how many times we've been shaken, 
;; and the current tone.
(define-struct world (shaken tone wake-lock))

(define initial-world (make-world 0 4 false))


;; ignore: world -> world
(define (ignore a-world)
  a-world)

;; update: world -> world
(define (update a-world)
  (make-world (add1 (world-shaken a-world))
              (world-tone a-world)
              (world-wake-lock a-world)))


;; ring: world -> effect
(define (ring a-world)
  (make-effect:play-dtmf-tone (world-tone a-world) 500))

;; check-wake-lock: world -> effect
#;(define (check-wake-lock a-world)
  (if (world-wake-lock a-world)
      (make-effect:set-wake-lock 6)
      (make-effect:release-wake-lock)))

;; up: world -> world
(define (up a-world)
  (make-world (world-shaken a-world)
              (min (add1 (world-tone a-world))
                   MAX-TONE)
              (world-wake-lock a-world)))

;; down: world -> world
(define (down a-world)
  (make-world (world-shaken a-world)
              (max (sub1 (world-tone a-world))
                   MIN-TONE)
              (world-wake-lock a-world)))

;; toggle-sleep: world -> world
#;(define (toggle-sleep a-world)
  (make-world (world-shaken a-world)
              (world-tone a-world)
              (not (world-wake-lock a-world))))



(define button-up (js-button up))
(define button-down (js-button down))
#;(define button-sleep (js-button toggle-sleep))

;; render: world -> (sexpof dom)
(define (render w)
  (list (js-div)
        (list (js-div)
              (list (js-text (number->string (world-shaken w))))
              (list (js-text " "))
              (list (js-text "rings")))
        (list (js-div)
              (list (js-text "note is: "))
              (list (js-text (number->string (world-tone w)))))
        (list button-up (list (js-text "Up")))
        (list button-down (list (js-text "Down")))
        #;(list button-sleep (list (js-text (if (world-wake-lock w) "Let Sleep" "Keep Awake"))))))

;; render-css: world -> (sexpof css-style)
(define (render-css w)
  '())


(js-big-bang initial-world
             '()
             (on-draw render render-css)
             #;(on-tick* 1 ignore check-wake-lock)
             (on-shake* update ring))