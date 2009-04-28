#lang scheme
(require "model.ss"
         "server-helper.ss"
         "sendmail.ss"
         "../compile-world.ss"
         (prefix-in javascript: "../beginner-to-javascript.ss")
         web-server/servlet-env
         web-server/servlet
         scheme/runtime-path
         net/url)

;; simple hardcoded server

;; Change these two values if you want to move the server somewhere
;; else.
(define server-name "kfisler-ra1.wpi.edu")
(define port-number 8888)
(define request-base-path 
  (format "http://~a:~a/" server-name port-number))



(define-runtime-path data (build-path "data"))
(define-runtime-path htdocs-path (build-path "htdocs"))
(define model (make-model data))


(define a-form
`(form ((enctype "multipart/form-data")
        (method "post"))
       (input ((type "file")
               (name "datafile")
               (size "40")))
       (input ((type "submit")
               (value "Send")))))


;; start: request -> response
(define (start a-request)
  (let* ([url (request-uri a-request)]
         [first-part (path/param-path (first (url-path url)))])
    (cond
      [(not (string? first-part))
       (error 'start "Must be /compile/ or /get/ or /js-compile/")]
      [(string=? first-part "compile")
       (handle-compile a-request)]
      [(string=? first-part "get")
       (handle-get (path/param-path (second  (url-path url))))]
      [(string=? first-part "js-compile")
       (handle-js-compile a-request)])))


;; good-email-address
(define (good-email-address? an-email)
  (regexp-match #rx"^.*@.*$" an-email))


;; handle-compile: request -> response
(define (handle-compile a-request)
  (cond
    [(bindings-assq #"datafile"
                    (request-bindings/raw a-request))
     =>
     (lambda (a-file-binding)
       (match a-file-binding
         [(struct binding:file (id filename headers content))
          (let ([email-address
                 (extract-binding/single 'email
                                         (request-bindings a-request))])
            (cond 
              [(and (sendmail-available?) 
                    (good-email-address? email-address))
               (thread (lambda ()
                         (let ([result-binary 
                                (compile-file (bytes->string/utf-8 filename)
                                              content
                                              email-address)])
                           (send-email-notification email-address
                                                    (binary-id result-binary)))))
               (we-will-call-you-response a-request email-address)]
              [else
               (let* ([a-request (redirect/get/forget)]
                      [result-binary 
                      (compile-file (bytes->string/utf-8 filename)
                                    content
                                    email-address)])
                 (here-is-the-download-link-response 
                  a-request
                  (binary-id result-binary)))]))]
         [else
          (error 'start "Not a file: ~s" a-file-binding)]))]
    [else
     (error 'handle-compile)]))




;; handle-js-compile: request -> response
;; Generates a response where the user's program is translated to a world program.
;; VERY HACKY.
(define (handle-js-compile a-request)
  (let* ([a-program (read* (open-input-string
                            (extract-binding/single 'program
                                                    (request-bindings a-request))))]
         [a-compiled-program
          (javascript:program->compiled-program a-program)]
         [a-main.js (compiled-program->main.js a-compiled-program '())])
    (void)))




;; read*: input-port -> (listof sexp)
(define (read* inp)
  (let ([next-sexp (read inp)])
    (cond
      [(eof-object? next-sexp)
       '()]
      [else
       (cons next-sexp (read* inp))])))




;; we-will-call-you-response: string -> response
(define (we-will-call-you-response a-request email-address)
  (make-bootstrap-response 
   (list
    `(p "We are currently compiling your application; as soon as we are done, we will send a "
        "notification email to " ,email-address " with the download URL."))))


;; here-is-the-download-link-response: string -> response
(define (here-is-the-download-link-response a-request binary-id)
  (make-bootstrap-response
   (list `(p "We've finished compilation.  You can download the application at "
             (a ((href ,(string-append request-base-path
                                       "get/" 
                                       binary-id)))
                ,(string-append request-base-path
                                "get/"
                                binary-id))
             "."))))


;; send-email-notification: string string -> void
;; Sends out an email.
(define (send-email-notification email-address binary-id)
  (let ([op (send-mail-message/port #:from "Danny Yoo <dyoo@cs.wpi.edu>"
                                    #:subject "Your application has been compiled"
                                    #:to (list email-address))])
    (display "Your download is available at: " op)
    (display (string-append request-base-path "get/" binary-id)
           op)
    (close-output-port op)))



;; handle-get: string -> response
(define (handle-get id)
  (let ([a-binary
         (model-find-binary model id)])
    (make-binary-response a-binary)))




;; compile-file: string bytes string -> binary
(define (compile-file filename content email-address)
  (let* ([program-name 
          (regexp-replace #rx".(ss|scm)$" filename "")]
         [user (model-find-or-add-user! 
                model
                email-address
                email-address)]
         [source (model-add-source! model
                                    program-name
                                    content 
                                    user)]
         [binary (model-compile-source! model source android)])
    binary))



;; make-binary-response: binary -> response
(define (make-binary-response a-binary)
  (make-input-port-response 
   (binary-name a-binary)
   (open-input-bytes (binary-package a-binary))))



;; make-boostrap-response: sexp -> response
(define (make-bootstrap-response body)
  `(html (head (title "Bootstrap")
               (link ((rel "stylesheet")
                      (type "text/css")
                      (href "/styles.css"))))
         (body (div ((id "top"))
                    
                    (img ((src "/images/icon.gif")
                          (id "icon")
                          (style "margin: 0px 35px 0px 20px")))
                    (img ((src "/images/logo.png")
                          (id "logo"))))

               (div ((id "sidebar"))
                    (ul
                     (li (a ((href "/")) "Compiler"))
                     (li (a ((href "http://www.bootstrapworld.org")) "Back to Bootstrap"))))
               
               (div ((id "body"))
                    (div ((class "item"))
                         ,@body)))))






(serve/servlet start
               #:port port-number
               #:command-line? #t
               #:listen-ip #f
               #:servlet-regexp #rx"^/(compile|get|js-compile)/"
               #:extra-files-paths (list htdocs-path))