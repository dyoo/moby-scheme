#lang racket/base

(require racket/runtime-path
         racket/cmdline
         racket/port
         racket/file
         racket/list
         web-server/servlet
         web-server/servlet-env
         xml
         "logger.ss"
         "../../compile-helpers.ss"
         (prefix-in lap: "../local-android-packager.ss")
         (planet dyoo/pack-directory:1/pack-directory))

;; This is a servlet that compiles packages to Android.
;;
;; Expected input: a program.
;;
;; Parameters: none.
;; POST data should be:
;; An s-expression describing the input, followed by bytes representing the assets in zip format.
;;
;; example:
;; POST whose data is:
;; '(name "My sample program)
;; <bytes for a zip file>
;;
;; Expected output: the android package.
;;
;; Should throw an error page.  If error during compilation, should include the domified
;; error structure.


(define-runtime-path HTDOCS-PATH "htdocs")

(define current-access-logger (make-parameter #f))


;; start: request -> response
(define (start req)
  (with-handlers ([exn:fail?
                   (lambda (exn)
                     (handle-unexpected-error exn))])
    (let-values ([(metadata asset-zip-bytes) 
                  (parse-request req)])
      (write-to-access-log! req asset-zip-bytes)
      
      ;; within temporary directory
      (let ([tmpdir
             (make-temporary-file "mztmp~a" 'directory #f)])
      
        (dynamic-wind (lambda ()
                        ;; create zip file in the temporary directory
                        (void))
                      (lambda ()
                        ;; create the build structure
                        ;; unzip contents of assets
                        ;; build the package
                        ;; read the package bytes
                        (cond
                          [(and metadata asset-zip-bytes)
                           ;; Lots of file system stuff here.
                           ;; Creates the basic structure,
                           (lap:prepare-android-package-src-structure tmpdir)
                           ;; sets up compilation parameters,
                           (lap:write-local.properties tmpdir)
                           ;; and customizes the assets by what's been provided
                           ;; by the client.
                           (parameterize ([current-directory 
                                           (build-path tmpdir "assets")])
                             (unpack-into-current-directory asset-zip-bytes))

                           (run-ant-build.xml tmpdir "debug")
                           
                           (let ([apk-bytes (lap:get-apk-in-dest tmpdir)])
                             (make-package-response metadata
                                                    apk-bytes))]
                          [else
                           (error-no-program)]))
                      
                      (lambda ()
                        (delete-directory/files tmpdir)))))))


;; write-to-access-log!: request program/resources -> void
(define (write-to-access-log! req program/resources)
  (void)
  #;(when (current-access-logger)
      (with-handlers ([void (lambda (exn)
                              (write (exn-message exn) (current-error-port)))])
        (logger-add! (current-access-logger)
                     (request-client-ip req)
                     program/resources
                     '()))))


;; parse-request: request -> (values (or/c #f s-exp) (or/c #f bytes))
(define (parse-request req)
  (let ([post-bytes (request-post-data/raw req)])
    (cond
      [post-bytes 
       (let ([ip (open-input-bytes post-bytes)]
             [op (open-output-bytes)])
         (let ([metadata (read ip)])
           (copy-port ip op)
           (values metadata (get-output-bytes op))))]
      [else
       (values #f #f)])))





;; make-package-response: string bytes -> response
;; Produces the proper HTTP response for the android package.
;; Headers also include the filename in the content-disposition field, so the
;; user gets a useful file name.
(define (make-package-response metadata package-bytes)
  (make-response/full
     200
     #"OK"(current-seconds)
     #"application/vnd.android.package-archive"
     (list (make-header #"content-disposition"
                        (string->bytes/utf-8 
                         (format "attachment; filename=~a.apk" 
                                 (normalize-name-as-filename 
                                  (metadata-name metadata))))))
     (list package-bytes)))
    

;; metadata-name: metadata -> string
;; Gets the name given in the metadata.
(define (metadata-name a-metadata)
  (second (assoc 'name a-metadata)))



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
(define PORT (make-parameter 8888))
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
               #:servlet-regexp (regexp "^/package$"))