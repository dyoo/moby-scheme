#lang scheme/base

(require "xmlrpc/xmlrpc.ss"
         net/url)

(define current-server-url (make-parameter (string->url "http://localhost:8000/servlets/standalone.ss")))

(define (get-moby-compile)
  (define server (xmlrpc-server (current-server-url)))
  (define moby-compile (server "moby.compile"))
  moby-compile)


(define (test)
  (define c (get-moby-compile))
  (c "android" "Hiya" #"\n\n\n(big-bang 100 100 0 false)"))