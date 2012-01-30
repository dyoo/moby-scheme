;#lang scheme/gui
;
;(require (lib "world.ss" "htdp")
;         (lib "prim.ss" "lang")
;         lang/prim
;         htdp/world
;         htdp/image
;         (except-in htdp/testing test)
;         (for-syntax scheme/base))
;(provide circle triangle rectangle ellipse star line text place-image empty-scene EXAMPLE)
;
;(provide-higher-order-primitive start (rocket-height))

(provide start)


; dimensions: 
(define WIDTH  200)
(define HEIGHT 600)
(define IMAGE0 (empty-scene WIDTH HEIGHT))

(define source (bitmap/url "http://www.wescheme.org/images/teachpacks2011/rocket.png"))
(define ROCKET (put-pinhole source (/ (image-width source) 2) (image-height source)))

;; -----------------------------------------------------------------------------
;; draw-world: Number -> Image 
;; create an image that represents the world 
(define (draw-world w)
  (text-add (car w) ((cdr w) (car w)) (rocket-add w IMAGE0)))

;; text-add : Number Image -> Image
;; add the height of the rocket and the time elapsed to the image
(define (text-add time height IMAGE0)
  (place-image (text (string-append "Time: " (number->string time)) 14 (make-color 41 128 38))
               67 
               0 
               (place-image (text (string-append "Height: " (number->string height)) 14 (make-color 38 38 128))
                            60
                            20
                            IMAGE0)))

;; rocket-add : Number Image -> Image 
;; add the satellite to the image assuming w seconds since start of simulation
(define (rocket-add w IMAGE0)
  (cond
    [(>= (image-height ROCKET) (- HEIGHT ((cdr w) (car w))))
     (place-image ROCKET 100 200 IMAGE0)]
    [else (place-image ROCKET 100 (- HEIGHT ((cdr w) (car w))) IMAGE0)]))


;; (Number (Number -> Number)) Symbol -> (Number (Number -> Number))
;; add 1 to the current time
(define (tock w ke)
  (cond
    [(key=? ke #\space) (cons (+ 1 (car w)) (cdr w))]
    [else w]))

(define (start rocket-height)
  (big-bang (cons 0 rocket-height)
	    (on-key tock)
	    (on-redraw draw-world)))


;; a `test' macro that is a synonym for `check-expect', catches expansion
;; errors and pretends that they come from `test'.
;(require (for-syntax syntax/kerncase))
;(define-syntax (EXAMPLE stx)
;  (syntax-case stx ()
;    [(_ x ...)
;     (with-handlers ([exn? (lambda (e)
;                             (raise (make-exn
;                                     (regexp-replace*
;                                      #rx"check-expect"
;                                      (exn-message e)
;                                      "test")
;                                     (exn-continuation-marks e))))])
;       (local-expand (syntax/loc stx (check-expect x ...))
;                     (syntax-local-context)
;                     (kernel-form-identifier-list)))]))















(provide   sq sine cosine tangent
           pick subset? in?
           type #;warn number->image string->image put-image overlay-at)
  
  ;; warn : any* -> any, and a side effect.
  ;; display all arguments and return the last one.
  #;(define (warn . args)
    (begin
      (map display args)
      (newline)
      (last args)))
  
  ;; type : any -> String
  (define (type obj)
    (cond
      [(procedure? obj) "Function"]
      [(number? obj) "Number"]
      [(string? obj) "String"]
      [(image? obj) "Image"]
      [(boolean? obj) "Boolean"]
      [(posn? obj) "Position"]
      [(symbol? obj) "Symbol"]
      [else "I don't know."]))
  
  ;; string->image : String -> Image
  ;; convert the given string to an image.
  (define (string->image s)
    (text s 14 'black))
  
  ;; number->image : Number -> Image
  ;; convert the given number to an image.
  (define (number->image n)
    (string->image (number->string n)))

  ;; overlay-at : Image Number Number Image -> Image
  ;; Place the foreground on the background at x y 
  ;; (in positive-y point space) relative to the center
  (define (overlay-at background x y foreground)
    (overlay/xy background x (- 0 y) foreground))

  ;; put-image : Image Number Number Image -> Image
  ;; Place the foreground on the background at x y
  ;; (in positive-y point space) relative to the lower left
  (define (put-image foreground x y background)
    (place-image foreground x (- (image-height background) y) background))

  ; sq : Number -> Number
  (define (sq x) (* x x))
  ;; sine : Degrees -> Number
  ;; For a right triangle with non-right angle x in degrees,
  ;; find the ratio of the length of the opposite leg to the 
  ;; length of the hypotenuse.      sin = opposite / hypotenuse
  (define (sine x) (sin (* x (/ pi 180))))
  ;; cosine : Degrees -> Number
  ;; For a right triangle with non-right angle x in degrees,
  ;; find the ratio of the length of the adjacent leg to the 
  ;; length of the hypotenuse.      cos = adjacent / hypotenuse
  (define (cosine x) (cos (* x (/ pi 180))))
  ;; tangent : Degrees -> Number
  ;; For a right triangle with non-right angle x in degrees,
  ;; find the ratio of the length of the opposite leg to the
  ;; length of the adjacent leg.    tan = opposite / adjacent
  (define (tangent x) (tan (* x (/ pi 180))))

  ;; pick : List -> Element
  ;; pick a random element from the list
  (define (pick lst)
    (list-ref lst (random (length lst))))
  
  ;; subset? : List List -> Boolean
  ;; return true if list a is a (proper or improper) subset of b
  (define (subset? a b) 
    (andmap
     (lambda (ele) (member ele b))
     a))
  
  (define (in? a b)
    (if (list? a) (subset? a b) (member a b)))
  

