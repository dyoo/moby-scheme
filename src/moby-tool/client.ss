#lang scheme/base

(require "xmlrpc/xmlrpc.ss"
         scheme/contract
         net/url)


(define (get-moby-compile server-url-string)
  (define server (xmlrpc-server (string->url server-url-string)))
  (define moby.compile (server "moby.compile"))
  (lambda (username email program-name source platform)
    (moby.compile username email program-name source platform)))


(define (test)
  (define c (get-moby-compile "http://localhost:8000/compile/"))
  (c "Danny Yoo" "dyoo@cs.wpi.edu" 
     "Hiya"
     #"\n\n\n(big-bang 100 100 0 false)"
     "android"))


(provide/contract [get-moby-compile (string? . -> . (string? string? bytes? . -> . (list/c string? bytes?)))])