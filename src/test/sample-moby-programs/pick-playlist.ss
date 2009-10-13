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
	      (list (js-text "Pick playlist")))


	(list (js-button* identity play) (list (js-text "Play")))
	(list (js-button* identity pause) (list (js-text "Pause")))
	(list (js-button* identity stop) (list (js-text "Stop")))))


(define (play w)
  (cond
    [(boolean? w)
     empty]
    [else
     (make-effect:play-sound w)]))


(define (pause w)
  (cond
    [(boolean? w)
     empty]
    [else
     (make-effect:pause-sound w)]))

(define (stop w)
  (cond
    [(boolean? w)
     empty]
    [else
     (make-effect:stop-sound w)]))



(define (draw-css w)
  '())
		 
     
(js-big-bang false
	     (on-draw draw draw-css))
