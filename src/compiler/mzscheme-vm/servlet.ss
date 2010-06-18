#lang scheme/base

(require web-server/servlet
         web-server/servlet-env
         scheme/runtime-path
         "write-support.ss"
         "compile.ss")

(define-runtime-path htdocs "servlet-htdocs")

;; make-port-response: (values output-port response/incremental)
(define (make-port-response #:mime-type (mime-type #"application/octet-stream"))
  (let-values ([(in out) (make-pipe)]
               [(CHUNK-SIZE) 1024])
    (values (make-response/incremental
             200 #"OK" (current-seconds)
             mime-type
             (list #;(make-header #"Content-Disposition"
                                  #"attachment; filename=\"file\""))
             (lambda (output-response)
               (let loop ()
                 (let ([some-bytes (read-bytes CHUNK-SIZE in)])
                   (unless (eof-object? some-bytes)
                     (output-response some-bytes)
                     (loop))))))
            out)))




;; Web service consuming programs and producing bytecode.
(define (start request)
  (let*-values ([(program-text) (extract-binding/single 'program (request-bindings request))]
                [(program-input-port) (open-input-string program-text)]
                [(response output-port) (make-port-response #:mime-type #"text/plain")])
    (compile program-input-port output-port)
    (close-output-port output-port)
    response))



;; Write out a fresh copy of the support library.
(call-with-output-file (build-path htdocs "support.js")
                       (lambda (op)
                         (write-support "browser" op))
                       #:exists 'replace)

(serve/servlet start 
               #:port 8000
               #:extra-files-paths (list htdocs)
               #:launch-browser? #f)

