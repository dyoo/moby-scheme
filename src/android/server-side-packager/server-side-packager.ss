#lang scheme/base

(require scheme/runtime-path
         scheme/cmdline
         scheme/class
         scheme/list
         web-server/servlet
         web-server/servlet-env
         xml
         "../../collects/moby/runtime/stx.ss"
         "../../resource.ss"
         "../../program-resources.ss"
         "../../compile-helpers.ss"
         "../local-android-packager.ss")

;; This is a servlet that compiles packages to Android.

;; Expected input: a program.
;;
;; Expected output: the android package.
;; Should throw an error page.  If error during compilation, should include the domified
;; error structure.
;;
;; FIXME: add support for image resources
;; FIXME: add support for gzipping.


(define-runtime-path HTDOCS-PATH "htdocs")


;; start: request -> response
(define (start req)
  (with-handlers ([exn:fail?
                   (lambda (exn)
                     (handle-unexpected-error exn))])
    (let ([name (parse-program-name req)]
          [program/resources (parse-program/resources req)])
      
      (cond
        [(and name program/resources)
         (make-package-response name (build-android-package name 
                                                            program/resources))]
        [else
         (error-no-program)]))))



;; parse-name: request -> string
;; Extracts the name from the request
(define (parse-program-name req)
  (extract-binding/single 'name (request-bindings req)))



;; parse-program/resources: request -> (or/c program/resource #f)
;; Try to parse the program and its resources, or return false otherwise.
(define (parse-program/resources req)
  (let ([name (parse-program-name req)])
    (cond
      [(and name (exists-binding? 'program (request-bindings req)))
       (let ([program (extract-binding/single 'program (request-bindings req))])
         (make-program/resources (parse-string-as-program program name)
                                 (parse-resources req)))]
      
      [(and name (exists-binding? 'program-stx (request-bindings req)))
       (let ([program-stx (extract-binding/single 'program-stx (request-bindings req))])
         (make-program/resources (map sexp->stx (read (open-input-string program-stx)))
                                 (parse-resources req)))]
      
      [else #f])))



;; parse-resources: request -> (listof resource<%>)
(define (parse-resources req)
  (cond [(exists-binding? 'resource (request-bindings req))
         (map (lambda (val)
                (let ([name&bytes (read (open-input-string val))])
                  (log-debug (format "Reading resource ~s" (first name&bytes)))
                  (new named-bytes-resource% 
                       [name (first name&bytes)]
                       [bytes (second name&bytes)])))
              (extract-bindings 'resource (request-bindings req)))]
        [else
         empty]))



;; make-package-response: string bytes -> response
;; Produces the proper HTTP response for the android package.
;; Headers also include the filename in the content-disposition field, so the
;; user gets a useful file name.
(define (make-package-response program-name package-bytes)
  (make-response/full
     200
     #"OK"(current-seconds)
     #"application/vnd.android.package-archive"
     (list (make-header #"content-disposition"
                        (string->bytes/utf-8 
                         (format "attachment; filename=~a.apk" 
                                 (normalize-name-as-filename program-name)))))
     (list package-bytes)))
    



;; normalize-name-as-filename: string -> string
(define (normalize-name-as-filename a-name)
  (let ([a-name
         (regexp-replace* #px"[^\\w]" a-name "")])
    (cond
      [(string=? a-name "")
       "program"]
      [else
       a-name])))



;; error-no-program: -> response
(define (error-no-program)
  (make-response/full
     400
     #"Bad Request"
     (current-seconds)
     #"text/html"
     (list)
     (list (string->bytes/utf-8 
            (xexpr->string 
             `(html (head (title error))
                    (body
                     "The expected program is missing from the request.")))))))


;; handle-unexpected-error: exn:fail -> response
(define (handle-unexpected-error exn)
   (make-response/full
     400
     #"Bad Request"
     (current-seconds)
     #"text/html"
     (list)
     (list (string->bytes/utf-8 
            (xexpr->string 
             `(html (head (title error))
                    (body
                     "Moby was unable to build your program due to an unexpected error.\n"
                     "Please contact the Moby developers, and include the following content:\n"
                     ,(exn-message exn))))))))




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define PORT (make-parameter 8080))

(command-line #:once-each 
              [("-p" "--port") port "Use port for web server"
                               (PORT (string->number port))])
(serve/servlet start
               #:launch-browser? #f
               #:quit? #f
               #:listen-ip #f
               #:port (PORT)
               #:extra-files-paths (list HTDOCS-PATH)                 
               #:servlet-regexp (regexp
                                 "^/package/.*$"))