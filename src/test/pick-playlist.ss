#lang s-exp "../moby-lang.ss"

(define (update-playlist w playlist)
  playlist)

(define (draw w)
  (list (js-div)
	(list (js-text (format "Current playlist: ~s"
			       w)))

	(list (js-button*
	       (lambda (w)
		 w)
	       (lambda (w)
		 (make-effect:pick-playlist update-playlist)))
	      (list (js-text "Pick playlist")))))

(define (draw-css w)
  '())
		 
     
(js-big-bang false
	     (on-draw draw draw-css))
