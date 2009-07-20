;; The world is a latitude and longitude.
(define-struct world (lat long))

(define initial-world (make-world 0 0))

;; update: world number number -> world
(define (update w lat long)
  (make-world lat long))


;; draw: world -> DOM-sexp
(define (draw w)
  (list (js-text (string-append (number->string lat)
                                ", "
                                (number->string long)))))


;; draw-css: world -> CSS-sexp
(define (draw-css w)
  '())


(js-big-bang initial-world
             '()
             (on-draw draw draw-css)
             (on-location-change update))