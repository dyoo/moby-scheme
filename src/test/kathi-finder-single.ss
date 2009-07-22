;; The first three lines of this file were inserted by DrScheme. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname kathi-finder-single) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ())))
;; Kathi finder: A program to report where Kathi is.

(define WIDTH 320)
(define HEIGHT 480)

;; A loc is a lat/long pair representing a location.
(define-struct loc (lat long))

;; A place is centered on a location and extends 
;; to a radius measured in meters.
(define-struct place (name loc radius))

;; mile->meter: number -> number
;; Converts miles to meters.
(define (mile->meter miles)
  (* miles 1609.344))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Here are a few places of interest.
;; 100 Institute Rd, in a radius of 500 meters
(define WPI-PLACE 
  (make-place "WPI" (make-loc 42.272824 -71.808207) 500))
;; Parking place.
(define WPI-PARKING-PLACE 
  (make-place "WPI Parking" (make-loc 42.2737222 -71.8058627) 50))
;; Worcester, in a radius of 3 miles.
(define WORCESTER-PLACE 
  (make-place "Worcester" (make-loc 42.274514 -71.798744) (mile->meter 3)))

;; This is a list of the places.
(define ALL-PLACES
  (list WPI-PLACE
        WPI-PARKING-PLACE
        WORCESTER-PLACE))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;; The telephone number to send messages to.
(define ADDRESS "5554")
  
;; The world is the current location.
(define initial-world (make-loc 0 0))

;; loc->string: loc -> string
(define (loc->string w)
  (string-append "(" 
		 (number->string (loc-lat w))
		 ", "
		 (number->string (loc-long w))
		 ")"))

;; change-location: world number number -> world
(define (change-location w lat long)
  (make-loc lat long))

;; identity: world -> world
;; Doesn't change the world.
(define (identity w)
  w)

;; current-place: world -> place
;; Returns the closest place.
(define (current-place w)
  (cond [(empty? (find-places ALL-PLACES w))
         (make-place "Unknown" w 0)]
        [else
         (choose-smallest
          (find-places ALL-PLACES w))]))

;; send-report: world -> effect
;; Sends out a text message of the world description,
;; and produces the world.
(define (send-report w)
  (make-effect:send-sms ADDRESS 
                        (string-append (description w) 
                                       "\n" 
                                       (maps-url (place-loc (current-place w))))))

;; maps-url: loc -> string
;; Creates the Google maps url for a location.
(define (maps-url a-loc)
  (string-append "http://maps.google.com/maps?q="
                 (number->string 
                  (exact->inexact (loc-lat a-loc)))
                 ",+"
                 (number->string 
                  (exact->inexact (loc-long a-loc)))
                 "&iwloc=A&hl=en"))
  
;; description: world -> string
;; Produces a text description of the current place.
(define (description w)
  (place-name (current-place w)))


;; choose-smallest: (listof place) -> place
;; Returns the place with the smallest radius.
(define (choose-smallest places)
  (cond
    [(empty? (rest places))
     (first places)]
    [(< (place-radius (first places)) (place-radius (second places)))
     (choose-smallest (cons (first places) (rest (rest places))))]
    [else
     (choose-smallest (rest places))]))


;; find-places: world loc -> (listof place)
;; Finds places that match the a-loc.
(define (find-places places a-loc)
  (cond
    [(empty? places)
     empty]
    [(place-matches? (first places) a-loc)
     (cons (first places) (find-places (rest places) a-loc))]
    [else
     (find-places (rest places) a-loc)]))


;; place-matches?: place loc -> boolean
;; Returns true if the place matches the location.
(define (place-matches? a-place a-loc)
  (<= (location-distance (loc-lat a-loc)
                         (loc-long a-loc)
                         (loc-lat (place-loc a-place))
                         (loc-long (place-loc a-place)))
      (place-radius a-place)))


;; draw: world -> DOM-sexp
(define (draw w)
  (list (js-div)
        (list (js-p '(("id" "aPara")))
	      (list (js-text (description w)))
	      (list (js-text " "))
	      (list (js-text (loc->string w))))))
        


;; draw-css: world -> CSS-sexp
(define (draw-css w)
  '(("aPara" ("font-size" "30px"))))
 

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define tick-delay 10 #;(* 5 60))  ;; wait every five minutes before updates.

(js-big-bang initial-world
             '()
             (on-draw draw draw-css)
             (on-tick* tick-delay identity send-report)
             (on-location-change change-location))