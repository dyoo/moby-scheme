;; My own personal set of small exercises on the teachpacks, just to
;; make sure they're doing something reasonable.


(require bootstrap2012/bootstrap-common)

(check-expect (sq 3) 9)
(check-expect (sine 3) #i0.052335956242943835)
(check-expect (cosine 3) #i0.9986295347545738)
(check-expect (tangent 3) #i0.05240777928304121)

(check-expect (pick '(0)) 0)
(check-expect (pick '(0 0)) 0)
(check-expect (member? (pick '(red blue green)) '(red blue green)) #t)
(check-expect (member? (pick '(red blue green)) '(red blue green)) #t)
(check-expect (member? (pick '(red blue green)) '(red blue green)) #t)
(check-expect (member? (pick '(red blue green)) '(red blue green)) #t)
(check-expect (member? (pick '(red blue green)) '(red blue green)) #t)
(check-expect (member? (pick '(red blue green)) '(red blue green)) #t)

(check-expect (subset? '(a b) '(a x b y)) #t)
(check-expect (subset? '(a b z) '(a x b y)) #f)

(check-expect (in? '(a b) '(a x b y)) #t)
(check-expect (in? '(a b z) '(a x b y)) #f)
(check-expect (in? 'a '(a x b y)) #t)
(check-expect (in? 'z '(a x b y)) #f)

(check-expect (type "hello") "String")
(check-expect (type 42) "Number")
(check-expect (type type) "Function")
(check-expect (type (circle 20 "solid" "green")) "Image")
(check-expect (type true) "Boolean")
(check-expect (type false) "Boolean")
(check-expect (type (make-posn 3 4)) "Position")
(check-expect (type 'sym) "Symbol")
(check-expect (type '(a b c)) "List")
(check-expect (type (cons 'a 'c)) "Pair")
(check-expect (type (make-color 1 2 3 4)) "Structure")


(check-expect (image? (number->image 42)) #t)
(check-expect (image? (number->image 0)) #t)
(check-expect (image? (number->image -42)) #t)


(check-expect (image? (string->image "")) #t)
(check-expect (image? (string->image "hello world")) #t)

(check-expect (image? (boolean->image true)) #t)
(check-expect (image? (boolean->image false))#t)


"should be a circle bottom left:"
(put-image (circle 20 'solid 'green) 0 0 (empty-scene 100 100))
(check-expect (image? (put-image (circle 20 'solid 'green) 0 0 (empty-scene 100 100)))
              #t)

"should be a circle bottom right:"
(put-image (circle 20 'solid 'green) 100 0 (empty-scene 100 100))
(check-expect (image? (put-image (circle 20 'solid 'green) 0 0 (empty-scene 100 100)))
              #t)

"should be a circle top left:"
(put-image (circle 20 'solid 'green) 0 100 (empty-scene 100 100))
(check-expect (image? (put-image (circle 20 'solid 'green) 0 0 (empty-scene 100 100)))
              #t)

"should be a circle top right:"
(put-image (circle 20 'solid 'green) 100 100 (empty-scene 100 100))
(check-expect (image? (put-image (circle 20 'solid 'green) 0 0 (empty-scene 100 100)))
              #t)



"should be a circle bottom left:"
(overlay-at (circle 20 'solid 'green) 0 0 (empty-scene 100 100))
(check-expect (image? (overlay-at (circle 20 'solid 'green) 0 0 (empty-scene 100 100)))
              #t)


"should be a man on a circle, transparency preserved"
(define p (bitmap/url "http://localhost:8000/run.png"))
(overlay (color-list->bitmap (image->color-list p) (image-width p) (image-height p)) (circle 30 'solid 'blue))


