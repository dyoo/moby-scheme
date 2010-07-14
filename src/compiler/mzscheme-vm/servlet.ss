#lang scheme/base

(require web-server/servlet
         web-server/servlet-env
         scheme/runtime-path
         "write-support.ss"
         "compile.ss"
         "../../../support/externals/mzscheme-vm/src/sexp.ss")

(define-runtime-path htdocs "servlet-htdocs")
(define-runtime-path compat 
  "../../../support/externals/mzscheme-vm/lib/compat")


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
  
  
  (with-handlers ([void 
                   (lambda (exn)
                     (cond
                       [(jsonp-request? request)
                        (handle-json-exception-response request exn)]
                       [else 
                        (handle-exception-response request exn)]))])
    (let*-values ([(program-name)
                   (string->symbol
                    (extract-binding/single 'name (request-bindings request)))]
                  [(program-text) 
                   (extract-binding/single 'program (request-bindings request))]
                  [(program-input-port) (open-input-string program-text)])
      ;; To support JSONP:
      (cond [(jsonp-request? request)
             (handle-json-response request program-name program-input-port)]
            [else
             (handle-response request program-name program-input-port)
             ]))))




;; jsonp-request?: request -> boolean
;; Does the request look like a jsonp request?
(define (jsonp-request? request)
  (exists-binding? 'callback (request-bindings request)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; jsonp stuff

;; handle-json-response: -> response
(define (handle-json-response request program-name program-input-port)
  (let-values ([(response output-port) (make-port-response #:mime-type #"text/plain")])
    (fprintf output-port "~a((" 
             (extract-binding/single 'callback (request-bindings request)))
    (compile program-input-port output-port #:name program-name)
    (fprintf output-port "));\n")
    (close-output-port output-port)
    response))


;; handle-json-exception-response: exn -> response
(define (handle-json-exception-response request exn)
  (raise exn)
  (let-values ([(response output-port) (make-port-response #:mime-type #"text/plain")])
    (let ([payload
           (format "~a(~a);\n" (extract-binding/single 'on-error (request-bindings request))
                   (sexp->js (exn-message exn)))])
      (fprintf output-port "~a" payload)
      (close-output-port output-port)
      response)))




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; non jsonp stuff: use with xmlhttprequest
(define (handle-response request program-name program-input-port)
  (let-values  ([(response output-port) (make-port-response #:mime-type #"text/plain")])
    (display "(" output-port)
    (compile program-input-port output-port #:name program-name)
    (display ")" output-port)
    (close-output-port output-port)
    response))

;; handle-exception-response: exn -> response
(define (handle-exception-response request exn)
  (raise exn)
  (make-response/full 500 
                      #"Internal Server Error"
                      (current-seconds)
                      #"application/octet-stream"
                      (list)
                      (list (string->bytes/utf-8 (exn-message exn)))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;; Write out a fresh copy of the support library.
(call-with-output-file (build-path htdocs "support.js")
  (lambda (op)
    (write-support "browser" op))
  #:exists 'replace)

(serve/servlet start 
               #:port 8000
               #:extra-files-paths (list htdocs compat)
               #:launch-browser? #f
               #:listen-ip #f)

