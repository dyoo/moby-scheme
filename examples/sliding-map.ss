#lang s-exp "../moby-lang.ss"

(require jsworld/phonegap)
(require jsworld/google-maps)

(define-struct latlng (lat lng))

;; A velocity has an x and y component.
(define-struct vel (x y))


;; The world is a latlng and a velocity.
(define-struct world (latlng vel))




(define initial-world (make-world (make-latlng 41.817513 -71.390791)
				  (make-vel 0 0)))


(define default-map-zoom 5)


;; Here's our map.
(define mymap (google-map (latlng-lat (world-latlng initial-world))
			  (latlng-lng (world-latlng initial-world))
			  default-map-zoom
			  '(("id" "mymap"))))



(define (draw-html w)
  (list mymap))


(define (draw-css w)
  '(("mymap" ("width" "100%") ("height" "100%"))))
     


;; tilt: world number number number -> world
;; Changes velocity based on tilt.
(define (tilt w azimuth pitch roll)
  (make-world (world-latlng w)
              (make-vel (/ roll 90) (/ (- pitch) 90))))


;; tick: world -> world
(define (tick w)
  (make-world (make-latlng (+ (vel-y (world-vel w))
			      (latlng-lat (world-latlng w)))
			   (+ (vel-x (world-vel w))
			      (latlng-lng (world-latlng w))))
	      (world-vel w)))



;; recenter-map: world -> effect
;; Recenter mymap to show where we are.
;;
;; TODO: can we show an arrow showing our direction?
;; Since we have our previous location and the next location
(define (recenter-map w)
  (make-effect:map:location mymap
			    (latlng-lat (world-latlng w))
			    (latlng-lng (world-latlng w))))



(js-big-bang initial-world
	     (on-draw draw-html draw-css)
	     (on-tilt tilt)
	     (on-tick! tick recenter-map))