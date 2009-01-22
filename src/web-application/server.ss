#lang scheme
(require (planet untyped/dispatch:1:10)
         web-server/http/request-structs
         web-server/http/response-structs
         web-server/servlet-env
         net/url
         (prefix-in xmlrpc: "xmlrpc/server-core.ss"))

(provide/contract [main (request? . -> . response?)])

(define-site moby-site
  ([(url "/moby/list") list-binaries]
   [(url "/moby/download/" (integer-arg) "/" (string-arg))
    download]
   [(url "/moby/ping/")
    handle-xmlrpc]
   [(url "/moby/compile/")
    handle-xmlrpc])
  #:rule-not-found (lambda (request)
                     '(html (head (title "File not Found"))
                            (body (p "File not found")))))


(define-controller (list-binaries request)
  'fill-me-in)

(define-controller (download request id filename)
  'fill-me-in)

(define-controller (handle-xmlrpc request)
  (xmlrpc:handle-xmlrpc-servlet-request* request))



(define (moby.ping)
  'hello-world)

(xmlrpc:add-handler 'moby.ping moby.ping)


(define (main request)
  (printf "I'm in main: ~s" (url->string (request-uri request)))
  (newline)
  (dispatch request moby-site))


(serve/servlet main 
               #:listen-ip #f
               #:launch-browser? #f
               #:file-not-found-responder main
               #:servlet-path "/moby/")