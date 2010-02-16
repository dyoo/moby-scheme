#lang scheme/base

(require scheme/match
         scheme/runtime-path
         file/gzip
         web-server/servlet
         web-server/servlet-env
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


(define (start req)
  "hello world"
  #;(void))




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define PORT 8080)
(define-runtime-path HTDOCS-PATH "htdocs")
(serve/servlet start
               #:launch-browser? #f
               #:quit? #f
               #:listen-ip #f
               #:port PORT
               #:extra-files-paths (list HTDOCS-PATH)                 
               #:servlet-regexp (regexp
                                 "^/package$"))
                                  