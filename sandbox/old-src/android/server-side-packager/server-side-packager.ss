#lang scheme/base

(require scheme/runtime-path
         scheme/cmdline
         scheme/class
         scheme/list
         web-server/servlet
         web-server/servlet-env
         net/uri-codec
         xml
         "logger.ss"
         "../../collects/moby/runtime/stx.ss"
         "../../resource.ss"
         "../../program-resources.ss"
         "../local-android-packager.ss"
         "../helpers.ss")

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

(define current-access-logger (make-parameter #f))


;; start: request -> response
(define (start req)
  (with-handlers ([exn:fail?
                   (lambda (exn)
                     (handle-unexpected-error exn))])
    (let* ([bindings (get-bindings req)]
           [name (parse-program-name bindings)]
           [program/resources (parse-program/resources bindings)])
      (write-to-access-log! req program/resources)
      (cond
        [(and name program/resources)
         (make-package-response name (build-android-package name program/resources))]
        [else
         (error-no-program)]))))


;; write-to-access-log!: request program/resources -> void
(define (write-to-access-log! req program/resources)
  (when (current-access-logger)
    (with-handlers ([void (lambda (exn)
                           (write (exn-message exn) (current-error-port)))])
      (logger-add! (current-access-logger)
                   (request-client-ip req)
                   program/resources
                   '()))))



;; get-bindings: request -> bindings
;; Return the bindings we get from the request.
;; Note that this call might be expensive since we're doing a gunzip-bytes.
(define (get-bindings req)
  (form-urlencoded->alist 
   (bytes->string/utf-8
    (request-post-data/raw req)
    #;(gunzip-bytes (request-post-data/raw req)))))



;; parse-name: bindings -> string
;; Extracts the name from the request
(define (parse-program-name bindings)
  (extract-binding/single 'name bindings))


;; parse-program/resources: bindings -> (or/c program/resource #f)
;; Try to parse the program and its resources, or return false otherwise.
(define (parse-program/resources bindings)
  (let ([name (parse-program-name bindings)])
    (cond      
      [(and name (exists-binding? 'program-stx bindings))
       (let ([program-stx (extract-binding/single 'program-stx bindings)])
         (make-program/resources (sexp->program 
                                  (read (open-input-string program-stx)))
                                 (parse-resources bindings)))]
      
      [else #f])))


;; parse-resources: bindings -> (listof resource<%>)
(define (parse-resources bindings)
  (cond [(exists-binding? 'resource bindings)
         (map (lambda (val)
                (let ([name&bytes (read (open-input-string val))])
                  (log-debug (format "Reading resource ~s" (first name&bytes)))
                  (new named-bytes-resource% 
                       [name (first name&bytes)]
                       [bytes (second name&bytes)])))
              (extract-bindings 'resource bindings))]
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
                     (br)
                     "Please contact the Moby developers, and include the following content:\n"
                     (br)
                     ,(exn-message exn))))))))




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define PORT (make-parameter 8080))
(define LOGFILE-PATH (make-parameter (build-path (current-directory) "access.log")))
(command-line #:once-each 
              [("-p" "--port") port "Use port for web server"
                               (PORT (string->number port))]
              [("-L" "--logfile-dir") logfile-dir "Use the directory to write access.log"
                                      (LOGFILE-PATH (build-path logfile-dir "access.log"))])

(current-access-logger (make-logger (LOGFILE-PATH)))
(serve/servlet start
               #:launch-browser? #f
               #:quit? #f
               #:listen-ip #f
               #:port (PORT)
               #:extra-files-paths (list HTDOCS-PATH)                 
               #:servlet-regexp (regexp
                                 "^.*$"  #;"^/package/.*$"))