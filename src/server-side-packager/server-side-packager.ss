#lang scheme/base

(require scheme/runtime-path
         scheme/cmdline
         web-server/servlet
         web-server/servlet-env
         "../program-resources.ss"
         "../compile-helpers.ss"
         "../android-packager.ss")

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
  (cond
    [(and (exists-binding? 'program (request-bindings req))
          (exists-binding? 'name (request-bindings req)))
     (try-to-produce-package (extract-binding/single 'program (request-bindings req))
                             (extract-binding/single 'name (request-bindings req)))]
    [else
     (error-no-program req)]))


;; try-to-produce-package: string -> response
(define (try-to-produce-package program-text program-name)
  (let ([program (parse-string-as-program program-text
                                          program-name)])
    (make-response/full
     200
     #"OK"(current-seconds)
     #"application/vnd.android.package-archive"
     (list (make-header #"content-disposition"
                        (string->bytes/utf-8 
                         (format "attachment; filename=~a.apk" 
                                 (normalize-name-as-filename program-name)))))
     (list (build-android-package program-name
                                  (make-program/resources program '()))))))


;; normalize-name-as-filename: string -> string
(define (normalize-name-as-filename a-name)
  (let ([a-name
         (regexp-replace* #px"[^\\w]" a-name "")])
    (cond
      [(string=? a-name "")
       "program"]
      [else
       a-name])))

(define (error-no-program req)
  "Missing program")



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