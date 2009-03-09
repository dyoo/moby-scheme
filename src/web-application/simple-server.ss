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
             (let ([result-binary 
                    (compile-file (bytes->string/utf-8 filename)
                                  content)])
               (make-binary-response result-binary))]
            [else
             (error 'start "Not a file: ~s" a-file-binding)])]))]
    [else
     a-form]))



(define (compile-file filename content)
  (let* ([user (model-find-or-add-user! 
                model
                "anonymous"
                "nobody@nobody.com")]
         [source (model-add-source! model
                                    filename 
                                    content 
                                    user)]
         [binary (model-compile-source! model source android)])
    binary))




(define (make-binary-response a-binary)
  (make-input-port-response 
   (binary-name a-binary)
   (open-input-bytes (binary-package a-binary))))



(serve/servlet start
               #:port 8888
               #:command-line? #t
               #:listen-ip #f
               #:servlet-regexp #rx"")