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
  

