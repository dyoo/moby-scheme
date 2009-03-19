;; The first three lines of this file were inserted by DrScheme. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname kathi-finder) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ())))
(require (lib "location.ss" "moby" "stub"))
(require (lib "sms.ss" "moby" "stub"))
;; Kathi finder: A program to report where Kathi is.


(define WIDTH 400)
(define HEIGHT 400)


;; A place is centered on a latitude and longitude, and extends 
;; to a radius measured in meters.
(define-struct place (latitude longitude radius))


;; mile->meter: number -> number
;; Converts miles to meters.
(define (mile->meter miles)
  (* miles 1609.344))


;; Here are a few places of interest.

;; 100 Institute Rd, in a radius of 500 meters
(define WPI-PLACE (make-place 42.272824 
                              -71.808207
                              500))

;; Worcester, in a radius of 3 miles.
(define WORCESTER-PLACE (make-place 42.274514
                                    -71.798744
                                    (mile->meter 3)))

;; Within half a mile of parent's place.
(define PARENTS-PLACE (make-place 42.274514
                                  -71.798744
                                  (mile->meter 0.5)))


;; The world consists of a list of places and a current place.
(define-struct world (places current-place))

