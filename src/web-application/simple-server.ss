#lang scheme
(require "model.ss"
         "server-helper.ss"
         web-server/servlet-env
         web-server/servlet
         scheme/runtime-path)

;; simple hardcoded server

(define-runtime-path data (build-path "data"))

(define model (make-model data))


(define a-form
`(form ((enctype "multipart/form-data")
        (method "post"))
       (input ((type "file")
               (name "datafile")
               (size "40")))
       (input ((type "submit")
               (value "Send")))))


(define (start a-request)
  (cond
    [(bindings-assq #"datafile"
                    (request-bindings/raw a-request))
     =>
     (lambda (a-file-binding)
       (cond
         [(binding:file? a-file-binding)
          (match a-file-binding
            [(struct binding:file (id filename headers content))
             (let ([result-package 
                    (compile-file filename content)])
               (form-response result-package))]
            [else
             (error 'start "Not a file: ~s" a-file-binding)])]))]
    [else
     a-form]))


(define (compile-file filename content)
  #"ok")


(define (form-response some-bytes)
  (make-input-port-response 
   "foo.apk" 
   (open-input-bytes some-bytes)))



(serve/servlet start
               #:command-line? #t
               #:listen-ip #f
               #:servlet-regexp #rx"")