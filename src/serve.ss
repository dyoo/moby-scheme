#lang scheme/base


(require scheme/local
         scheme/class
         scheme/runtime-path
         scheme/tcp
         scheme/contract
         scheme/file
         scheme/list
         web-server/servlet
         web-server/servlet-env
         web-server/dispatch 
         file/zip
         "stub/net.ss"
         "utils.ss"
         "compiler/beginner-to-javascript.ss"
         "compiler/pinfo.ss"
         "image-lift.ss"
         "compiler/stx.ss"
         "generate-application.ss")

(require (for-syntax scheme/base))

(define-runtime-path javascript-support "../support/js")
(define-runtime-path javascript-main-template "../support/js/main.js.template")


;; Evaluate some function that uses a directory, and put it in the context of a temporary
;; directory that'll be deleted on exit.
(define (with-temporary-directory f)
  (let ([dir #f])
    (dynamic-wind 
     (lambda ()
       (set! dir (make-temporary-file))
       (delete-file dir)
       (make-directory dir))     
     (lambda ()
       (f dir))
     (lambda ()
       (delete-directory/files dir)))))

(define-syntax (define/cached stx)
  (syntax-case stx ()
    [(_ (name args ...) body ...)
     (syntax/loc stx
       (define name 
         (let ([cached-result #f])
           (lambda (args ...)
             (cond
               [cached-result
                (unbox cached-result)]
               [else
                (set! cached-result (box (begin body ...)))
                (unbox cached-result)])))))]))




;; compile-and-serve: (listof stx) -> void
;; Generate a web site that compiles and evaluates the program.
(define (compile-and-serve source-code [program-name "unknown"])
  (with-temporary-directory
   (lambda (dir)
     (local [(define-values (lifted-source-code named-bitmaps)
               (lift-images/stxs source-code))
             
             (define program+resources (make-program/resources lifted-source-code named-bitmaps))
             
             (define-values (dispatcher url)
               (dispatch-rules
                [("choose") choose-option]
                [("networkProxy") network-proxy]
                [("generate-js-zip" (string-arg)) generate-js-zip]
                [("generate-apk" (string-arg)) generate-apk]))
             
             (define (choose-option req)
               `(html
                 (head
                  (title "Moby")
                  (link ((rel "stylesheet")
                         (href "css/choose.css")
                         (type "text/css"))))
                 (body
                  (h1 "Moby")
                  (h2 ((class "programTitle")) ,program-name)
                  
                  (a ((class "linkbutton")
                      (href "index.html")
                      #;(style "display:none"))
                     (div ((class "big"))
                          "Run program"))
                  " "
                  (a ((class "linkbutton")
                      (href ,(url generate-js-zip
                                  (string-append program-name
                                                 ".zip"))))
                     (div ((class "big"))
                          "Get Javascript .zip"))
                  " "
                  (a ((class "linkbutton")
                      (href ,(url generate-apk 
                                  (string-append program-name "-debug.apk")))) 
                     (div ((class "big")) "Get Android .apk")))))
             
             (define (network-proxy req)
               (list #"text/plain"
                     (get-url (extract-binding/single 'url (request-bindings req)))))

             
             (define/cached (generate-js-zip req filename)
               (with-temporary-directory
                (lambda (dir)
                  (let ([dest (build-path dir program-name)])
                    (generate-javascript-application program-name
                                                     program+resources
                                                     dest)
                    (parameterize ([current-directory dir])
                      (zip (build-path dir (string-append program-name ".zip"))
                           program-name))
                    (list #"application/zip"
                          (get-file-bytes (build-path 
                                           dir 
                                           (string-append program-name
                                                          ".zip"))))))))
             
             (define/cached (generate-apk req filename)
               (with-temporary-directory
                (lambda (dir)
                  (let ([dest (build-path dir program-name)])
                    (generate-javascript+android-phonegap-application program-name
                                                                      program+resources
                                                                      dest)
                    (list #"application/vnd.android.package-archive"
                          (get-file-bytes (build-path dest "bin" 
                                                      (string-append
                                                       (upper-camel-case program-name)
                                                       "-debug.apk"))))))))]
       
       
       (generate-javascript-application program-name
                                        program+resources
                                        dir)
       (let* ([T 84]
              [portno
               (let loop (;; Numerology at work  (P = 80, L = 76, T=84).
                          [portno 8076]
                          [attempts 0]) 
                 (with-handlers ((exn:fail:network? (lambda (exn)
                                                      (cond [(< attempts T)
                                                             (loop (add1 portno)
                                                                   (add1 attempts))]
                                                            [else
                                                             (raise exn)]))))
                   ;; There's still a race condition here... Not sure how to do this right.
                   (let ([port (tcp-listen portno 4 #t #f)])
                     (tcp-close port)
                     portno)))])
         (serve/servlet dispatcher
                        #:port portno
                        #:listen-ip #f
                        #:servlet-path "/choose"
                        #:servlet-regexp
                        #rx"(^/networkProxy)|(^/choose)|(^/generate-apk)|(^/generate-js-zip)"
                        #:extra-files-paths (list javascript-support dir)))))))




;;; FIXME: A lot of this is just copy-and-pasted from generate-application.  FIXME!

(define (do-compilation program)
  (program->compiled-program/pinfo program (get-base-pinfo 'moby)))



(provide/contract [compile-and-serve (((listof stx?)) (string?) . ->* . any)])
