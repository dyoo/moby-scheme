;; The first three lines of this file were inserted by DrScheme. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname jsworld-fading) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ())))
(define (tick w)
  (add1 w))


(define hello-div (js-div))
(define separator-div (js-div))
(define hello-text-1 (js-text "hello"))
(define hello-text-2 (js-text "world"))


(define (get-color r g b)
  (string-append "#" (hex r)
                     (hex g) (hex b)))

(define (hex-digit n)
  (string (list-ref (string->list "0123456789ABCDEF") n)))

(define (hex n)
  (string-append 
    (hex-digit (quotient n 16))
    (hex-digit (modulo n 16))))
                 

;; redraw: world -> sexp
(define (redraw w)
  (list hello-div (list hello-text-1)
                  (list separator-div)
                  (list hello-text-2)))


(define (redraw-css w)
  (local ((define n (floor (abs (* (sin (/ w 10)) 255)))))
	 (list (list hello-div (list "color" 
				     (get-color n n n))))))



(js-big-bang 0
             '()
	     (on-draw redraw redraw-css)
             (on-tick (/ 1 10) tick))