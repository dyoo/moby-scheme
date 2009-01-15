#lang web-server/insta
(require "model.ss"
         "xmlrpc/server-core.ss")


(define model (make-model "data"))

(no-web-browser)


(define (moby-compile platform-name bytes)
  (let ([bin
         (compile-source (match platform-name
                           ['j2me
                            (make-platform:j2me)
                            'android
                            (make-platform:android)])
                         bytes)])
    (list (binary-name bin)  (binary-bytes bin))))




(add-handler 'moby.compile moby-compile)

(define (start request)
  (handle-xmlrpc-servlet-request* request))
