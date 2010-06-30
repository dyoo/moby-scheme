#lang s-exp "../../moby-lang.ss"

#;(require (lib "world.ss" "htdp")
         (lib "prim.ss" "lang")
         lang/prim
         htdp/world
         (except-in htdp/testing test)
         (for-syntax scheme/base))
#;(provide circle triangle rectangle ellipse star line text #;EXAMPLE)
#;(provide-higher-order-primitive start (rocket-height))

(provide start)

; dimensions: 
(define WIDTH  200)
(define HEIGHT 600)
(define IMAGE0 (empty-scene WIDTH HEIGHT))

(define source
  (open-image-url "http://www.wescheme.org/images/teachpacks/rocket.png"))

(define ROCKET (put-pinhole source (/ (image-width source) 2) (image-height source)))

(define-struct world (current-height rocket-height))


;; -----------------------------------------------------------------------------
;; draw-world: Number -> Image 
;; create an image that represents the world 
(define (draw-world w)
  (text-add ((world-rocket-height w) (world-current-height w)) (rocket-add w IMAGE0)))

;; text-add : Number Image -> Image
;; add the height of the rocket to the image
(define (text-add height IMAGE0)
  (place-image (text (string-append "Height: " (number->string height)) 14 'black)
               60 
               0 
               IMAGE0))

;; rocket-add : Number Image -> Image 
;; add the satellite to the image assuming w seconds since start of simulation
(define (rocket-add w IMAGE0)
  (cond
    [(>= (image-height ROCKET) (- HEIGHT ((world-rocket-height w) (world-current-height w))))
     (place-image ROCKET 100 200 IMAGE0)]
    [else (place-image ROCKET 100 (- HEIGHT ((world-rocket-height w) (world-current-height w))) IMAGE0)]))


;; (Number (Number -> Number)) Symbol -> (Number (Number -> Number))
;; add 1 to the current time
(define (tock w ke) 
  (make-world (+ 1 (world-current-height w))
              (world-rocket-height w)))


(define (start rocket-height)
  (js-big-bang (make-world 0 rocket-height)
               (on-key tock)
               (on-redraw draw-world)))


;; a `test' macro that is a synonym for `check-expect', catches expansion
;; errors and pretends that they come from `test'.
#;(require (for-syntax syntax/kerncase))
#;(define-syntax (EXAMPLE stx)
  (syntax-case stx ()
    [(_ x ...)
     (with-handlers ([exn? (lambda (e)
                             (raise (make-exn
                                     (regexp-replace*
                                      #rx"check-expect"
                                      (exn-message e)
                                      "test")
                                     (exn-continuation-marks e))))])
       (local-expand (syntax/loc stx (check-expect x ...))
                     (syntax-local-context)
                     (kernel-form-identifier-list)))]))