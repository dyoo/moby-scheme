#lang scheme/base


(require scheme/local
         scheme/runtime-path
         scheme/tcp
         scheme/string
         scheme/contract
         web-server/servlet
         web-server/servlet-env
         web-server/dispatch 
         "stub/net.ss"
         "compiler/beginner-to-javascript.ss"
         "compiler/pinfo.ss"
         "template.ss"
         "compiler/permission.ss"
         "compiler/stx.ss")


(define-runtime-path javascript-support "../support/js")
(define-runtime-path javascript-main-template "../support/js/main.js.template")


;; compile-and-serve: (listof stx) -> void
;; Generate a web site that compiles and evaluates the program.
(define (compile-and-serve source-code [program-name ""])
  (local [(define main.js 
            (compiled-program->main.js (do-compilation source-code)))
          
          (define-values (dispatcher url)
            (dispatch-rules
             [("choose") choose-option]
             [("main.js") main-js]
             [("networkProxy") network-proxy]
             [("generate-js-tarball") generate-js-tarball]
             [("generate-apk") generate-apk]))
          
          
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
                   (href "index.html"))
                  "Run program")
               " "
               (a ((class "linkbutton")
                   (href "index.html"))
                  "Get Javascript .zip")
               " "
               (a ((class "linkbutton")
                   (href ,(url generate-apk)))
                  "Get Android .apk"))))
              
          
          (define (main-js req)
            (list #"text/javascript"
                  main.js))
          
          
          (define (network-proxy req)
            (list #"text/plain"
                  (get-url (extract-binding/single 'url (request-bindings req)))))
          
          
          (define (generate-js-tarball req)
            (list #"application/x-tar-gz"
                  "fill-me-in"))
          
          
          (define (generate-apk req)
            (list #"application/vnd.android.package-archive"
                  "fill-me-in"))]
    
    
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
                     #rx"(^/main.js$)|(^/networkProxy)|(^/choose)|(^/generate-apk)"
                     #:extra-files-paths (list javascript-support)))))


;;; FIXME: A lot of this is just copy-and-pasted from generate-application.  FIXME!

(define (do-compilation program)
  (program->compiled-program/pinfo program (get-base-pinfo 'moby)))

;; compiled-program->main.js: compiled-program -> string
(define (compiled-program->main.js compiled-program)
  (let*-values ([(defns pinfo)
                 (values (compiled-program-defns compiled-program)
                         (compiled-program-pinfo compiled-program))]
                [(output-port) (open-output-string)]
                [(mappings) 
                 (build-mappings 
                  (PROGRAM-DEFINITIONS defns)
                  (IMAGES (string-append "[" "]"))
                  (PROGRAM-TOPLEVEL-EXPRESSIONS
                   (compiled-program-toplevel-exprs
                    compiled-program))
                  (PERMISSIONS (get-permission-js-array (pinfo-permissions pinfo))))])
    (fill-template-port (open-input-file javascript-main-template)
                        output-port
                        mappings)
    (get-output-string output-port)))

;; get-permission-js-array: (listof permission) -> string
(define (get-permission-js-array perms) 
  (string-append "["
                 (string-join (map (lambda (x)
                                     (format "string_dash__greaterthan_permission(~s)" (permission->string x)))
                                   perms)
                              ", ")
                 "]"))




(provide/contract [compile-and-serve ((listof stx?) . -> . any)])
