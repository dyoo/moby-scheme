;; The first three lines of this file were inserted by DrScheme. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname kathi-finder) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ())))

;; Kathi finder: A program to report where Kathi is.


(define WIDTH 320)
(define HEIGHT 480)


;; A place is centered on a latitude and longitude, and extends 
;; to a radius measured in meters.
(define-struct named-place (name latitude longitude radius))

;; An unnamed-place is a location somewhere that we don't know about
(define-struct unnamed-place (latitude longitude))


;; place->string: place -> string
(define (place->string a-place)
  (cond
    [(named-place? a-place)
     (named-place-name a-place)]
    [(unnamed-place? a-place)
     (string-append "Unknown (" 
                    (number->string (unnamed-place-latitude a-place))
                    ", "
                    (number->string (unnamed-place-longitude a-place))
                    ")")]))



;; place-radius: place -> number
;; Given a place, returns its radius.
(define (place-radius a-place)
  (cond
    [(named-place? a-place)
     (named-place-radius a-place)]
    [(unnamed-place? a-place)
     (mile->meter 0.5)]))


;; place-latitude: place -> number
(define (place-latitude a-place)
  (cond
    [(named-place? a-place)
     (named-place-latitude a-place)]
    [(unnamed-place? a-place)
     (unnamed-place-latitude a-place)]))


;; place-longitude: place -> number
(define (place-longitude a-place)
  (cond
    [(named-place? a-place)
     (named-place-longitude a-place)]
    [(unnamed-place? a-place)
     (unnamed-place-longitude a-place)]))


;; mile->meter: number -> number
;; Converts miles to meters.
(define (mile->meter miles)
  (* miles 1609.344))


;; Here are a few places of interest.

;; 100 Institute Rd, in a radius of 500 meters
(define WPI-PLACE (make-named-place "WPI"
                                    42.272824 
                                    -71.808207
                                    500))

(define WPI-PARKING-PLACE (make-named-place "WPI Parking"
                                            42.2737222
                                            -71.8058627
                                            50))

;; Worcester, in a radius of 3 miles.
(define WORCESTER-PLACE (make-named-place "Worcester"
                                          42.274514
                                          -71.798744
                                          (mile->meter 3)))




;; The world consists of a list of interested places and a place.
(define-struct world (places place phone-number))

(define initial-world (make-world (list WPI-PLACE
                                        WPI-PARKING-PLACE
                                        WORCESTER-PLACE)
                                  (make-unnamed-place 0 0)
                                  ;; The telephone number is 
                                  ;; currently hardcoded to
                                  ;; the emulator.
                                  "5556"))



;; update-world-place: world place -> world
(define (update-world-place a-world a-place)
  (make-world (world-places a-world)
              a-place
              (world-phone-number a-world)))


;; handle-location-change: world number number -> world
(define (handle-location-change a-world latitude longitude)
  (cond
    [(places-are-significantly-different? 
      (world-place a-world) 
      (choose-place a-world latitude longitude))
     (report-new-place 
      (update-world-place a-world 
                          (choose-place a-world latitude longitude)))]
    [else
     (update-world-place a-world 
                         (choose-place a-world latitude longitude))]))


;; places-are-significantly-different?: place place -> boolean
(define (places-are-significantly-different? p1 p2)
  (cond
    [(and (named-place? p1) (named-place? p2))
     (not (equal? p1 p2))]
    [(and (unnamed-place? p1) (unnamed-place? p2))
     false]
    [else
     true]))


;; report-new-place: world -> world
;; Sends out a text message, and then returns the world.
(define (report-new-place a-world)
  (send-text-message (world-phone-number a-world)
                     (string-append "Kathi is at " 
                                    (place->string (world-place a-world))
                     
                     
                                    ".  Link: "
                                    "http://maps.google.com/maps?q="
                                    (number->string (place-latitude (world-place a-world)))
                                    ",+"
                                    (number->string (place-longitude (world-place a-world)))
                                    "&iwloc=A&hl=en")
                     a-world))


;; choose-place: world number number -> place
(define (choose-place a-world a-lat a-long)
  (cond
    [(empty? (find-places (world-places a-world) a-lat a-long))
     (make-unnamed-place a-lat a-long)]
    [else
     (find-smallest-place
      (find-places (world-places a-world) a-lat a-long))]))


;; find-smallest-place: (listof place) -> place
;; Returns the place with the smallest radius.
(define (find-smallest-place places)
  (cond
    [(empty? (rest places))
     (first places)]
    [(< (place-radius (first places))
        (place-radius (second places)))
     (find-smallest-place (cons (first places)
                                (rest (rest places))))]
    [else
     (find-smallest-place (rest places))]))
    


;; find-places: world number numbber -> (listof place)
(define (find-places places a-lat a-long)
  (cond
    [(empty? places)
     empty]
    [(place-matches? (first places) a-lat a-long)
     (cons (first places)
           (find-places (rest places) a-lat a-long))]
    [else
     (find-places (rest places) a-lat a-long)]))



;; place-matches?: place number number -> boolean
(define (place-matches? a-place a-lat a-long)
  (<= (location-distance a-lat 
                         a-long 
                         (place-latitude a-place) 
                         (place-longitude a-place))
      (place-radius a-place)))


;; render-world: world -> world
(define (render-world a-world)
  (place-image
   (text (place->string (world-place a-world)) 10 "black")
   20 
   20
   (empty-scene WIDTH HEIGHT)))



(big-bang WIDTH HEIGHT initial-world
          (on-redraw render-world)
          (on-location-change handle-location-change))