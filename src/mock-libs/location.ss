#lang scheme/base
(require lang/prim)

;; Mock module for the location service
;; 
;; http://www.bcca.org/misc/qiblih/latlong_us.html#MASSACHUSETTS

;; latitude: horizontal lines
(define (get-latitude)
  (degree&minute->float 42 16))

;; longitude: vertical lines
(define (get-longitude)
  (- (degree&minute->float 71 52)))

(define (get-attitude)
  #f)

(define (get-bearing)
  #f)

(define (get-speed)
  #f)


(define (degree&minute->float degree minute)
  (exact->inexact (+ degree (/ minute 60))))

         
         
         
(provide-primitives get-latitude
                    get-longitude 
                    get-attitude 
                    get-bearing 
                    get-speed)