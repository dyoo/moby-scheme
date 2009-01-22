#lang scheme
(require (planet untyped/dispatch:1:10)
         (planet untyped/dispatch:1:10/response)
         web-server/http/request-structs
         web-server/http/response-structs
         web-server/servlet-env
         net/url
         scheme/runtime-path
         (prefix-in xmlrpc: "xmlrpc/server-core.ss")
         "model.ss")


(provide/contract [main (request? . -> . response?)])


(define-runtime-path htdocs (build-path "htdocs"))
(define-runtime-path data (build-path "htdocs"))



(define model (make-model data))


(define-site moby-site
  ([(url "/list/") list-binaries]
   [(url "/" (integer-arg) "/" (string-arg))
    download]
   [(url "/ping/")
    handle-xmlrpc]
   [(url "/compile/")
    handle-xmlrpc])
  #:rule-not-found make-not-found-response)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-controller (list-binaries request)
  #"fill-me-in")

(define-controller (download request id filename)
  (define CHUNK-SIZE (expt 2 16))
  (let* ([binary
          (model-find-binary model id)]
         [package (binary-package binary)]
         [ip (open-input-bytes package)]
         [headers
          (list (make-header #"Content-Disposition"
                             (bytes-append
                              #"attachment; filename=\""
                              ;; fixme: we need to escape filename
                              (string->bytes/utf-8 filename)
                              #"\"")))])
    ;; fixme: Check that the filename matches what we've got?
    (make-response/incremental 200
                               "Okay"
                               (current-seconds)
                               #"application/octet-stream"
                               headers
                               (lambda (send/bytes)
                                 (let loop ()
                                   (let ([chunk
                                          (read-bytes CHUNK-SIZE ip)])
                                     (cond
                                       [(eof-object? chunk)
                                        (void)]
                                       [else
                                        (send/bytes chunk)
                                        (loop)])))))))


(define-controller (handle-xmlrpc request)
  (xmlrpc:handle-xmlrpc-servlet-request* request))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; XMLRPC Services.

(define (moby.ping)
  #"Ping")


;; moby.compile: string string string bytes string -> string
;; Compiles a program for a given username / email,
;; and produces the subpath needed to download the application.
(define (moby.compile username email program-name program-source platform-name)
  (let* ([user (model-find-or-add-user! model username email)]
         [source (model-add-source! model program-name program-source user)]
         [binary (model-compile-source! model source 
                                        (platform-name->platform platform-name))])
    (binary-server-subpath binary)))



;; binary-server-subpath: binary -> string
;; Given a binary, returns the subpath to download the binary.
(define (binary-server-subpath a-binary)
  (format "~a/~a"
          (number->string (binary-id a-binary))
          (binary-name a-binary)))


(define (platform-name->platform platform-name)
  (match platform-name
    ['"j2me"
     j2me]
    ['"android"
     android]))


(xmlrpc:add-handler 'moby.ping moby.ping)
(xmlrpc:add-handler 'moby.compile moby.compile)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;




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