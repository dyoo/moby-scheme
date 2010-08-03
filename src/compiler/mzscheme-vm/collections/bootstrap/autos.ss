(provide make-auto
	 auto?
	 auto-model
	 auto-hp
	 auto-rims
	 auto-color
	 auto-cost
	 car1
	 paint-job
	 turbo-charge
	 draw-auto)



; an auto is a (make-auto String Number Number String Number)
(define-struct auto (model hp rims color cost))

; define your cars here
(define car1 (make-auto "M5" 480 22 "black" 50000))

; expensive?: Auto -> Boolean
; does the car cost more than $30,000?

; cool? : Auto -> Boolean
; is the car the right color, with enough horsepower?

; paint-job : Auto String -> Auto
; produce an identical car with a new color
(define (paint-job a color)
  (make-auto (auto-model a)
             (auto-hp a)
             (auto-rims a)
             color
             (auto-cost a)))

; turbo-charge: Auto -> Auto
; add another 20HP to the Auto
(define (turbo-charge a)
  (make-auto (auto-model a)
             (+ (auto-hp a) 20)
             (auto-rims a)
             (auto-color a)
             (auto-cost a)))

;; test
(EXAMPLE (paint-job car1 "purple") (make-auto "M5" 480 22 "purple" 50000))








;;-----------------------
; draw-car: Auto -> Scene
(define (draw-auto a)
  (place-image (rectangle 130 50 "solid" (auto-color a))
               130 50
               (place-image (circle (auto-rims a) "solid" "silver")
                            210 110
                            (place-image (circle (auto-rims a) "solid" "silver")
                                         90 110
                                         (place-image (circle 30 "solid" "black")
                                                      210 110
                                                      (place-image (circle 30 "solid" "black")
                                                                   90 110
                                                                   (place-image (rectangle 220 60 "solid" (auto-color a))
                                                                                150 90
                                                                                (empty-scene 300 150))))))))
