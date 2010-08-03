#lang s-exp "../moby-lang.ss"

(require jsworld/phonegap)
(require jsworld/google-maps)



;; The world is a latitude/longitude pair
(define-struct world (lat lng))

(define initial-world (make-world 0 0))

(define default-map-zoom 5)

;; move: world number number -> world
;; Respond to a move by recording the new location
(define (move w lat lng)
  (make-world lat lng))


;; Here's our map.
(define mymap (google-map (world-lat initial-world)
			  (world-lng initial-world)
			  default-map-zoom
			  '(("id" "mymap"))))



(define (draw-html w)
  (list mymap))


(define (draw-css w)
  '(("mymap" ("width" "100%") ("height" "100%"))))
     


;; recenter-map: world number number -> effect
;; Recenter mymap to show where we are.
;;
;; TODO: can we show an arrow showing our direction?
;; Since we have our previous location and the next location
(define (recenter-map w lat lng)
  (make-effect:map:location mymap lat lng))
  


(js-big-bang initial-world
	     (on-draw draw-html draw-css)
	     (on-location-change! move recenter-map))