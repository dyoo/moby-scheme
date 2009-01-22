#lang scheme/base
(require "model.ss"
         "../utils.ss"
         "xmlrpc/server-core.ss"
         scheme/match
         web-server/servlet
         web-server/servlet-env)


(define model (make-model "data"))


(define (make-id)
  ;; fixme
  "id")


;; moby-compile: string bytes string -> void
(define (moby-compile name bytes platform-name)
  (let ([bin
         (model-compile-source! model
                                (make-source (make-id)
                                             name
                                             bytes
                                             (now-date-string)
                                             #f)
                                (match platform-name
                                  ['"j2me"
                                   (make-platform:j2me)]
                                  ['"android"
                                   (make-platform:android)]))])
    (list (binary-name bin)  (binary-package bin))))




(add-handler 'moby.compile moby-compile)

(define (start request)
  (handle-xmlrpc-servlet-request* request))


(serve/servlet start 
               #:listen-ip #f
               #:launch-browser? #f)