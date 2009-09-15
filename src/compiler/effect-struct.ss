#lang s-exp "lang.ss"

;; Effect structures: will be exported out to the Moby toplevel.

  
  
  
(define-struct effect:none ())
(define-struct effect:beep ())
(define-struct effect:play-dtmf-tone (tone duration))
(define-struct effect:send-sms (address  ;; string
                                msg ;; string
                                ))
(define-struct effect:play-sound-url (url ;; string
                                  ))
(define-struct effect:pause-sound-url (url))
(define-struct effect:stop-sound-url (url))
(define-struct effect:set-sound-volume (volume))
(define-struct effect:raise-sound-volume ())
(define-struct effect:lower-sound-volume ())
(define-struct effect:set-wake-lock (locks))
(define-struct effect:release-wake-lock ())
(define-struct effect:pick-playlist (f))


;; effect: X -> boolean
;; Check to see if the thing is an effect.
(define (effect? thing)
  (or (effect:none? thing)
      (effect:beep? thing)
      (effect:play-dtmf-tone? thing)
      (effect:send-sms? thing)
      (effect:play-sound-url? thing)
      (effect:pause-sound-url? thing)
      (effect:stop-sound-url? thing)
      (effect:set-sound-volume? thing)
      (effect:raise-sound-volume? thing)
      (effect:lower-sound-volume? thing)
      (effect:set-wake-lock? thing)
      (effect:release-wake-lock? thing)
      (effect:pick-playlist? thing)))

(provide/contract [struct effect:none ()]
                  [struct effect:beep ()]
                  [struct effect:play-dtmf-tone ([tone number?]
                                                 [duration number?])]
                  [struct effect:send-sms ([address string?]
                                           [msg string?])]
                  [struct effect:play-sound-url ([url string?])]
                  [struct effect:pause-sound-url ([url string?])]
                  [struct effect:stop-sound-url ([url string?])]
                  [struct effect:set-sound-volume ([volume number?])]
                  [struct effect:lower-sound-volume ()]
                  [struct effect:raise-sound-volume ()]
                  [struct effect:set-wake-lock ([locks number?])]
                  [struct effect:release-wake-lock ()]
                  [struct effect:pick-playlist ([f (any/c any/c . -> . any)])]
                  [effect? (any/c . -> . boolean?)])