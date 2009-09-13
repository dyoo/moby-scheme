#lang scheme/base
(require lang/prim)

(require "lat-lon-distance.ss")

;; Mock module for the location service
;; 
;; http://www.bcca.org/misc/qiblih/latlong_us.html#MASSACHUSETTS

;; latitude: horizontal lines
(define (get-latitude)
  (degree&minute->float 42 16))

;; longitude: vertical lines
(define (get-longitude)
  (- (degree&minute->float 71 52)))

(define (get-altitude)
  0)

(define (get-bearing)
  0)

(define (get-speed)
  0)


(define (degree&minute->float degree minute)
  (exact->inexact (+ degree (/ minute 60))))

    
;; returns distance in meters between (lat-1, long-1) and (lat-2, long-2).
;; fixme: put some approximation here.
(define (location-distance lat-1 long-1 lat-2 long-2)
  (compute-distance lat-1 long-1 lat-2 long-2))

         
         
(provide-primitives get-latitude
                    get-longitude 
                    get-attitude 
                    get-bearing 
                    get-speed
                    location-distance)