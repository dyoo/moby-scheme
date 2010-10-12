#lang scheme/base

(require scheme/contract
         scheme/port
         scheme/class
         scheme/list
         net/url
         net/uri-codec
         "../../compiler/version.ss"
         "../../collects/moby/runtime/stx.ss"
         "../../program-resources.ss"
         "../helpers.ss")

(define current-server-url (make-parameter #;"http://localhost:8080/package/"
                                           "http://go.cs.brown.edu/package/"))


;; build-android-package: string program/resources -> bytes
;; Calls out to the compiler web service to get back the android package.
(define (build-android-package name program/resources)
  (let ([data (encode-parameters-in-data name program/resources)])
    ;; FIXME: get errors here and do something reasonable.
    (with-handlers ([exn:fail:network?
                     (lambda (exn)
                       (handle-network-failure exn))])
      (log-debug (format "Sending ~s ~s" (current-server-url) data))
      (let* ([ip (post-impure-port (string->url (current-server-url)) 
                                   data)]
             [headers (purify-port ip)]
             [status-code (get-status-code headers)])
        (cond
          [(= status-code 200)
           (port->bytes ip)]
          [else
           (raise (make-exn:fail (bytes->string/utf-8 (port->bytes ip))
                                 (current-continuation-marks)))])))))



;; handle-network-failure: request exn -> response
(define (handle-network-failure exn)
  (raise (make-exn:fail 
          (string-append "We are unable to build your Android package; the web service appears to be offline.\n"
                         "Please contact the Moby developers; in your response, include the following:\n"
                         (exn-message exn))
          (current-continuation-marks))))


;; get-status-code: string -> number
;; Given the 
(define (get-status-code response-headers)
  (log-debug (format "Response headers are ~s~n" response-headers))
  (string->number (second (regexp-match #px"^[^\\s]+\\s([^\\s]+)" response-headers))))


;; encode-parameters-in-data: string program/resources -> bytes
;; Encodes the parameters we need to pass in to get a program.
;; TODO: GZIP the data, as soon as the web server can support it.
(define (encode-parameters-in-data name program/resources)
  (string->bytes/utf-8
   (alist->form-urlencoded 
    (list* (cons 'name name)
           (cons 'compiler-version VERSION)
           (cons 'program-stx
                 (format "~s" (program->sexp 
                               (program/resources-program program/resources))))
           (map (lambda (a-resource)
                  (log-debug (format "Sending resource ~s~n" (send a-resource get-name)))
                  (cons 'resource (format "~s"
                                          (list (send a-resource get-name)
                                                (send a-resource get-bytes)))))
                (program/resources-resources program/resources))))))


(provide/contract [build-android-package
                   (string? program/resources? . -> . bytes?)])