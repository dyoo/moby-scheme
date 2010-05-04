#lang s-exp "../../../private/restricted-runtime-scheme.ss"

;; Effect structures: will be exported out to the Moby toplevel.

  
  
(define-struct effect:none ())
(define-struct effect:beep ())
(define-struct effect:play-dtmf-tone (tone duration))
(define-struct effect:send-sms (address  ;; string
                                msg ;; string
                                ))


(define-struct playlist-sound (playlist))

(define (sound? x)
  (or (string? x)
      (playlist-sound? x)))


;; a sound is either a string url, or a playlist-sound.
(define-struct effect:play-sound (sound))
(define-struct effect:pause-sound (sound))
(define-struct effect:stop-sound (sound))

(define-struct effect:set-sound-volume (volume))
(define-struct effect:raise-sound-volume ())
(define-struct effect:lower-sound-volume ())

;; The beep volume is a separate volume control in Android.
(define-struct effect:set-beep-volume (volume))

(define-struct effect:set-wake-lock (locks))
(define-struct effect:release-wake-lock ())
(define-struct effect:pick-playlist (update-f))
(define-struct effect:pick-random (n update-f))





;; effect: X -> boolean
;; Check to see if the thing is an effect.
(define (effect? thing)
  (or (effect:none? thing)
      (effect:beep? thing)
      (effect:play-dtmf-tone? thing)
      (effect:send-sms? thing)
      (effect:play-sound? thing)
      (effect:pause-sound? thing)
      (effect:stop-sound? thing)
      (effect:set-sound-volume? thing)
      (effect:set-beep-volume? thing)
      (effect:raise-sound-volume? thing)
      (effect:lower-sound-volume? thing)
      (effect:set-wake-lock? thing)
      (effect:release-wake-lock? thing)
      (effect:pick-playlist? thing)
      (effect:pick-random? thing)))


(provide/contract [struct effect:none ()]
                  [struct effect:beep ()]
                  [struct effect:play-dtmf-tone ([tone number?]
                                                 [duration number?])]
                  [struct effect:send-sms ([address string?]
                                           [msg string?])]
                  [struct effect:play-sound ([sound sound?])]
                  [struct effect:pause-sound ([sound sound?])]
                  [struct effect:stop-sound ([sound sound?])]

                  [struct effect:set-sound-volume ([volume number?])]
                  [struct effect:set-beep-volume ([volume number?])]

                  [struct effect:lower-sound-volume ()]
                  [struct effect:raise-sound-volume ()]
                  [struct effect:set-wake-lock ([locks number?])]
                  [struct effect:release-wake-lock ()]
                  [struct effect:pick-playlist ([update-f (any/c any/c . -> . any)])]
                  [struct effect:pick-random ([n number?]
                                              [update-f (any/c number? . -> . any/c)])]
                  
                  [effect? (any/c . -> . boolean?)]

		  [struct playlist-sound ([playlist any/c])])