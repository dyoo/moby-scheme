#lang scheme/base

(require scheme/match
         scheme/runtime-path
         file/gzip
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
    (list #"application/vnd.android.package-archive"
          (build-android-package program-name
                                 (make-program/resources program '())))))


(define (error-no-program req)
  "Missing program")




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
                                 "^/package/.*$"))
                                  