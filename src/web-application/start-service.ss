#lang web-server/insta
(require "model.ss"
         "../utils.ss"
         "xmlrpc/server-core.ss")


(define model (make-model "data"))

(no-web-browser)

(define (make-id)
  ;; fixme
  "id")


(define (moby-compile platform-name name bytes)
  (printf "I see ~s ~s ~s~n" platform-name name bytes)
  (let ([bin
         (compile-source model
                         (make-source (make-id)
                                      name
                                      bytes
                                      (now)
                                      #f)
                         (match platform-name
                           ['"j2me"
                            (make-platform:j2me)]
                           ['"android"
                            (make-platform:android)]))])
    (list (binary-name bin)  (binary-bytes bin))))




(add-handler 'moby.compile moby-compile)

(define (start request)
  (handle-xmlrpc-servlet-request* request))
