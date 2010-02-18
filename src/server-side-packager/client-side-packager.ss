#lang scheme/base

(require scheme/contract
         scheme/port
         net/url
         net/uri-codec
         "../collects/moby/runtime/stx.ss"
         "../program-resources.ss")

(define current-server-url (make-parameter "http://go.cs.brown.edu/package"))


;; build-android-package: string program/resources -> bytes
;; Calls out to the compiler web service to get back the android package.
(define (build-android-package name program/resources)
  (let ([url (encode-parameters-in-url name program/resources)])
    (port->bytes (get-pure-port url))))



;; encode-parameters-in-url: string program/resources -> url
;; Encodes the parameters we need to pass in to get the
(define (encode-parameters-in-url name program/resources)
  (string->url
   (string-append (current-server-url) 
                  "?"
                  (alist->form-urlencoded 
                   (list (cons 'name name)
                         (cons 'program-stx
                               (format "~s" (map stx->sexp 
                                                 (program/resources-program 
                                                  program/resources))))
                         #;(map (lambda (a-resource)
                                  (cons 'resource ...))
                                program/resources))))))


(provide/contract [build-android-package
                   (string? program/resources? . -> . bytes?)])