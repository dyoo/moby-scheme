#lang scheme/base

(require web-server/servlet
         web-server/servlet-env
         scheme/runtime-path
         "write-support.ss"
         "compile.ss")

(define-runtime-path htdocs "servlet-htdocs")

;; make-port-response: (values response/incremental output-port)
;; Creates a response that's coupled to an output-port: whatever you
;; write into the output will be pushed into the response.
(define (make-port-response #:mime-type (mime-type #"application/octet-stream")
                            #:headers (headers '()))
  (let-values ([(in out) (make-pipe)]
               [(CHUNK-SIZE) 1024])
    (values (make-response/incremental
             200 #"OK" (current-seconds)
             mime-type
             headers
             (lambda (output-response)
               (let loop ()
                 (let ([some-bytes (read-bytes CHUNK-SIZE in)])
                   (unless (eof-object? some-bytes)
                     (output-response some-bytes)
                     (loop))))))
            out)))




;; Web service consuming programs and producing bytecode.
(define (start request)
  (with-handlers ([void handle-exception-response])
    (let*-values ([(program-text) (extract-binding/single 'program (request-bindings request))]
                  [(program-input-port) (open-input-string program-text)]
                  [(response output-port) (make-port-response #:mime-type #"text/plain")])
      ;; To support JSONP:
      (cond [(exists-binding? 'callback (request-bindings request))
             (fprintf output-port "~a(" 
                      (extract-binding/single 'callback (request-bindings request)))
             (compile program-input-port output-port)
             (fprintf output-port ")")
             (close-output-port output-port)
             response]
            [else
             (compile program-input-port output-port)
             (close-output-port output-port)
             response]))))


(define (handle-exception-response exn)
  (void))

    



;; Write out a fresh copy of the support library.
(call-with-output-file (build-path htdocs "support.js")
                       (lambda (op)
                         (write-support "browser" op))
                       #:exists 'replace)

(serve/servlet start 
               #:port 8000
               #:extra-files-paths (list htdocs)
               #:launch-browser? #f
               #:listen-ip #f)

