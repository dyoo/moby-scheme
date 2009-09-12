;; The first three lines of this file were inserted by DrScheme. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname gui-world-location) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ())))
(require (lib "gui-world.ss" "gui-world"))
;; Location, using the on-location-changed hook.


(define-struct world (latitude longitude))
(define initial-world (make-world 0 0))


;; lat-msg: world -> string
(define (lat-msg w)
  (number->string (world-latitude w)))


;; long-msg: world -> string
(define (long-msg w)
  (number->string (world-longitude w)))


;; The gui shows the latitude and longitude.
(define view
  (col
   (row "Latitude: " (message lat-msg))
   (row "Longitude: " (message long-msg))))


;; handle-location-change: world number number -> world
(define (handle-location-change a-world a-latitude a-longitude)
  (make-world a-latitude
              a-longitude))


(big-bang initial-world view
          (on-location-change handle-location-change))