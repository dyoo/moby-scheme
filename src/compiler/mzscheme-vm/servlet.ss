#lang scheme/base

(require web-server/servlet
         web-server/servlet-env
         scheme/runtime-path
         scheme/match
         scheme/list
         "write-support.ss"
         "compile.ss"
         "private/json.ss"
         "../moby-failure.ss"
         "../../collects/moby/runtime/error-struct.ss"
         "../../collects/moby/runtime/error-struct-to-dom.ss"
         "../../collects/moby/runtime/stx.ss"
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
    (compile/port program-input-port output-port #:name program-name)
    (fprintf output-port "));\n")
    (close-output-port output-port)
    response))


;; handle-json-exception-response: exn -> response
(define (handle-json-exception-response request exn)
  (case (compiler-version request)
    [(0)
     (let-values ([(response output-port) (make-port-response #:mime-type #"text/plain")])
       (let ([payload
              (format "~a(~a);\n" (extract-binding/single 'on-error (request-bindings request))
                      (sexp->js (exn-message exn)))])
         (fprintf output-port "~a" payload)
         (close-output-port output-port)
         response))]
    [(1)
     (let-values ([(response output-port) (make-port-response #:mime-type #"text/plain")])
       (let ([payload
              (format "~a(~a);\n" (extract-binding/single 'on-error (request-bindings request))
                      (jsexpr->json (exn->json-structured-output exn)))])
         (fprintf output-port "~a" payload)
         (close-output-port output-port)
         response))]))
     


;; exn->structured-output: exception -> jsexpr
;; Given an exception, tries to get back a jsexpr-structured value that can be passed back to
;; the user.
(define (exn->json-structured-output an-exn)
  (define (on-moby-failure-val failure-val)
    (make-hash `(("type" . "moby-failure")
                 ("dom-message" . 
                                ,(dom->jsexpr 
                                  (error-struct->dom-sexp failure-val #f))))))
  (cond
    [(exn:fail:read? an-exn)
     (let ([translated-srclocs 
            (map srcloc->Loc (exn:fail:read-srclocs an-exn))])
       (on-moby-failure-val
        (make-moby-error (if (empty? translated-srclocs)
                             ;; Defensive: translated-srclocs should not be empty, but
                             ;; if read doesn't give us any useful location to point to,
                             ;; we'd better not die here.
                             (make-Loc 0 1 0 0 "")
                             (first translated-srclocs))
                         (make-moby-error-type:generic-read-error
                          (exn-message an-exn)
                          (if (empty? translated-srclocs) 
                              empty
                              (rest translated-srclocs))))))]
    
    [(moby-failure? an-exn)
     (on-moby-failure-val (moby-failure-val an-exn))]
    
    [else
     (exn-message an-exn)]))


;; dom->jsexpr: dom -> jsexpr
;; Translate a dom structure to one that can pass through.  The dom is treated as a nested list.
(define (dom->jsexpr a-dom)
  (match a-dom
    [(list head-name attribs body ...)
     `(,(symbol->string head-name)
       ,(map (lambda (k+v)
               (list (symbol->string (first k+v))
                     (second k+v))) 
             attribs)
       ,@(map dom->jsexpr body))]
    [else
     a-dom]))



;; srcloc->Loc: srcloc -> jsexp
;; Converts a source location (as stored in exceptions) into one that we can
;; store in error structure values.
(define (srcloc->Loc a-srcloc)
  (make-Loc (srcloc-position a-srcloc)
            (srcloc-line a-srcloc)
            (srcloc-column a-srcloc)
            (srcloc-span a-srcloc)
            (format "~a" (srcloc-source a-srcloc))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; non jsonp stuff: use with xmlhttprequest
(define (handle-response request program-name program-input-port)
  (let-values  ([(response output-port) (make-port-response #:mime-type #"text/plain")])
    (display "(" output-port)
    (compile/port program-input-port output-port #:name program-name)
    (display ")" output-port)
    (close-output-port output-port)
    response))


;; handle-exception-response: exn -> response
(define (handle-exception-response request exn)
  (case (compiler-version request)
    [(0)
     (make-response/full 500 
                         #"Internal Server Error"
                         (current-seconds)
                         #"application/octet-stream"
                         (list)
                         (list (string->bytes/utf-8 (exn-message exn))))]
    [(1)
     (make-response/full 500 
                         #"Internal Server Error"
                         (current-seconds)
                         #"application/octet-stream"
                         (list)
                         (list (string->bytes/utf-8 
                                (jsexpr->json (exn->json-structured-output exn)))))]))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Helpers


;; compiler-version: request -> number
;; versions should be interpreted as
;;
;; 0: no support for structured error messages.
;; 1: support for structured error messages.
(define (compiler-version request)
  (cond
    [(exists-binding? 'compiler-version (request-bindings request))
     (string->number (extract-binding/single 'compiler-version (request-bindings request)))]
    [else
     0]))




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;; Write out a fresh copy of the support library.
(call-with-output-file (build-path htdocs "support.js")
  (lambda (op)
    (write-support "browser" op))
  #:exists 'replace)


;; Also, write out the collections
(call-with-output-file (build-path htdocs "collections.js")
  (lambda (op)
    (write-collections op))
  #:exists 'replace)


(serve/servlet start 
               #:port 8000
               #:servlet-path "/servlets/standalone.ss"
               #:extra-files-paths (list htdocs compat)
               #:launch-browser? #f
               #:listen-ip #f)

