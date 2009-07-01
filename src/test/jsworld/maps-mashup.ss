;; The first three lines of this file were inserted by DrScheme. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname maps-mashup) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ())))
;; mashup example.
;;
;; Google maps mashup: the map that's displayed and the latitude/longitude coordinates shown
;; should match up.


;; The world is a latitude/longitude.
(define-struct loc (lat long))


;; draw: world -> (sexpof dom)
(define (draw w)
  (local [(define a-div (js-div))
          (define lat-text (js-text (number->string (loc-lat w))))
          (define long-text (js-text (number->string (loc-long w))))]
    (list a-div 
          (list lat-text)
          (list (js-text " "))
          (list long-text))))


;; draw-css: world -> (sexpof css)
(define (draw-css w)
  '())


(js-big-bang 0
             '()
             (on-draw draw draw-css))