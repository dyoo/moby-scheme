;; The first three lines of this file were inserted by DrScheme. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname location) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ())))
;; The world is a latitude and longitude.
(define-struct world (lat long))

(define initial-world (make-world 0 0))

;; update: world number number -> world
(define (update w lat long)
  (make-world lat long))


;; draw: world -> DOM-sexp
(define (draw w)
  (list (js-text (string-append (number->string (world-lat w))
                                ", "
                                (number->string (world-long w))))))


;; draw-css: world -> CSS-sexp
(define (draw-css w)
  '())


(js-big-bang initial-world
             '()
             (on-draw draw draw-css)
             (on-location-change update)
             )