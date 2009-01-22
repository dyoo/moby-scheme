#lang scheme
(require (planet untyped/dispatch:1:10)
         (planet untyped/dispatch:1:10/response)
         web-server/http/request-structs
         web-server/http/response-structs
         web-server/servlet-env
         net/url
         scheme/runtime-path
         (prefix-in xmlrpc: "xmlrpc/server-core.ss"))


(provide/contract [main (request? . -> . response?)])


(define-runtime-path htdocs (build-path "htdocs"))


(define-site moby-site
  ([(url "/moby/list") list-binaries]
   [(url "/moby/download/" (integer-arg) "/" (string-arg))
    download]
   [(url "/moby/ping/")
    handle-xmlrpc]
   [(url "/moby/compile/")
    handle-xmlrpc])
  #:rule-not-found make-not-found-response)

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
  (dispatch request moby-site))


(define (run-server)
  (serve/servlet main 
                 #:listen-ip #f
                 #:launch-browser? #f
                 #:extra-files-paths (list htdocs)
                 
                 ;; using file-not-found-responder as a workaround
                 ;; a weirdness in the treatment of servlet-path.
                 #:file-not-found-responder main
                 #:servlet-path "/"))