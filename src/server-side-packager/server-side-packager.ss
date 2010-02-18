#lang scheme/base

(require scheme/runtime-path
         scheme/cmdline
         web-server/servlet
         web-server/servlet-env
         "../collects/moby/runtime/stx.ss"
         "../program-resources.ss"
         "../compile-helpers.ss"
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
  (cond
    [(and (exists-binding? 'program (request-bindings req))
          (exists-binding? 'name (request-bindings req)))
     (let ([name (extract-binding/single 'name (request-bindings req))]
           [program (extract-binding/single 'program (request-bindings req))])
     (make-package-response name
                            (program->package name program)))]

    [(and (exists-binding? 'program-stx (request-bindings req))
          (exists-binding? 'name (request-bindings req)))
     (let ([name (extract-binding/single 'name (request-bindings req))]
           [program-stx (extract-binding/single 'program-stx (request-bindings req))])
     (make-package-response name
                            (program-stx->package name program-stx)))]

    [else
     (error-no-program req)]))


;; program->package: string string -> bytes 
(define (program->package program-name program-text)
  (let* ([program (parse-string-as-program program-text
                                           program-name)]
         [package-bytes (build-android-package program-name
                                               (make-program/resources program '()))])
    package-bytes))

;; program-stx->package: string string -> bytes
(define (program-stx->package program-name program-stx-sexp)
  (let* ([program (map sexp->stx (read (open-input-string program-stx-sexp)))]
         [package-bytes (build-android-package program-name
                                               (make-program/resources program '()))])
    package-bytes))



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