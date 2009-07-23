;; The first three lines of this file were inserted by DrScheme. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname location-announcer) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ())))
;; Extention to the GPS program: it also sends an SMS message
;; every 30 seconds of the last known latitude/longitude.

;; The world is a latitude and longitude.
(define-struct world (lat long))

;; The initial world is set to (0, 0).
(define initial-world (make-world 0 0))


;; How many seconds we wait between sending sms messages.
(define delay-between-announcements 30)

;; Announcements will be sent to a phone number.
(define phone "5554")

;; update: world number number -> world
;; Update the world to the latest latitude and longitude reading.
(define (update w lat long)
  (make-world lat long))

;; world->string: world -> string
;; Produces a string description of the world.
(define (world->string w)
  (string-append "(" 
                 (number->string (world-lat w)) 
                 ", "
                 (number->string (world-long w))
		 ")"))


;; draw: world -> DOM-sexp
(define (draw w)
  (list (js-p '(("id" "aPara")))
        (list (js-text "(latitude, longitude) = "))
	(list (js-text (world->string w)))))


;; draw-css: world -> CSS-sexp
;; Style the paragraph so its internal text is large.
(define (draw-css w)
  '(("aPara" ("font-size" "30px"))))


;; no-world-change: world -> world
;; Don't change the world.
(define (no-world-change w)
  w)


;; announce-location: world -> effect
;; Produces an effect that sends an sms message out.
(define (announce-location w)
  (make-effect:send-sms phone-number 
                        (string-append "Current location is: " 
                                       (world-string w))))

(js-big-bang initial-world
             '()
             (on-draw draw draw-css)
             (on-location-change update)
             (on-tick* delay-between-announcements
                       no-world-change announce-location))

