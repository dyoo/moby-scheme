#lang scheme/base

(require (planet untyped/dispatch:1:5/dispatch)
         (planet untyped/instaservlet/instaservlet))

(define-site application
  ([(url "") myindex]
   [(url "/view/" (integer-arg)) view]))

(define-controller (myindex request)
  '(html (head (title "testing"))
         (body (p "hello world"))))

(define-controller (view request project-num)
  `(html (head (title "testing"))
         (body (p "Going to view " ,(number->string project-num)))))

(go! (lambda (request)
       (dispatch request application)))