#lang scheme/base

(require scheme/contract
         net/url
         "../program-resources.ss")

(define server-url (make-parameter "http://go.cs.brown.edu/package"))

(define (build-android-package name program/resources)
  (let ([url (encode-parameters-in-url name program/resources)])
    (void)))

(define (encode-parameters-in-url name program/resources)
  (alist->form-urlencoded 
   (list* (cons 'name name)
          (cons 'program-stx
                (format "~s" (stx->sexp (program/resources-program program/resources))))

          #;(map (lambda (a-resource)
                   ...)
                 program/resources))))


(provide/contract [build-android-packager
                   (string? program/resources? . -> . bytes?)])