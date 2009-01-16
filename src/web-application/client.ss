#lang scheme/base

(require "xmlrpc/xmlrpc.ss"
         scheme/contract
         net/url)


(define current-moby-server-url (make-parameter (string->url "http://localhost:8000/servlets/standalone.ss")))

(define (get-moby-compile server-url-string)
  (define server (xmlrpc-server (string->url server-url-string)))
  (define moby-compile (server "moby.compile"))
  moby-compile)


(define (test)
  (define c (get-moby-compile (current-moby-server-url)))
  (c "android" "Hiya" #"\n\n\n(big-bang 100 100 0 false)"))


(provide/contract [get-moby-compile (string? . -> . (string? string? bytes? . -> . bytes?))])