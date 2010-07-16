;;#lang s-exp "../../moby-lang.ss"

#;(require (lib "world.ss" "htdp")
         (lib "prim.ss" "lang")
         lang/prim
         htdp/world
         (except-in htdp/testing test)
         (for-syntax scheme/base))

#;(provide-higher-order-primitive start (offscreen?))
(provide start)

#;(provide EXAMPLE)

(define WIDTH  400)
(define HEIGHT 200)

(define source 
  (open-image-url "http://www.wescheme.org/images/teachpacks/butterfly.png"))

(define butterfly (put-pinhole source (/ (image-width source) 2) (/ (image-height source) 2)))

; a make-world is a (Number Number)
; each world has an x and y coordinate
(define-struct world [x y])

;; move: World Key -> Number 
;; did the object move? 
(define (move w key)
  (cond
    [(key=? key "left") (make-world (- (world-x w) 10) (world-y w))]
    [(key=? key "right") (make-world (+ (world-x w) 10) (world-y w))]
    [(key=? key "down") (make-world (world-x w) (- (world-y w) 10))]
    [(key=? key "up") (make-world (world-x w) (+ (world-y w) 10))]
    [else w]))


;; ----------------------------------------------------------------------------
;; draw-world: World -> Image 
;; create an image that represents the world 
(define (draw-world w)
  (let* ((draw-butterfly (lambda (w scene)
                           (place-image butterfly (world-x w) (- HEIGHT (world-y w)) scene)))
         (draw-text (lambda (w scene)
                      (place-image (text (string-append "x-coordinate: " (number->string (world-x w))
                                                        "   y-coordinate: " (number->string (world-y w)))
                                         14 'black)
                                   60 
                                   0 
                                   scene))))
    (draw-butterfly w (draw-text w (empty-scene WIDTH HEIGHT)))))


(define (start offscreen?)
  (let* ((update (lambda (w k) 
                   (cond 
                     [(char? k) w]
                     [(offscreen? (world-x (move w k)) 
                                  (world-y (move w k))) w]
                     [else (move w k)]))))
    (js-big-bang (make-world 200 100)
                 (on-key update)
                 (on-redraw draw-world))))

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