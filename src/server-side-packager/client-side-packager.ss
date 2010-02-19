#lang scheme/base

(require scheme/contract
         scheme/port
         scheme/class
         scheme/list
         net/url
         net/uri-codec
         "../collects/moby/runtime/stx.ss"
         "../program-resources.ss")

(define current-server-url (make-parameter "http://localhost:8080/package/"
                                           #;"http://go.cs.brown.edu/package/"))


;; build-android-package: string program/resources -> bytes
;; Calls out to the compiler web service to get back the android package.
(define (build-android-package name program/resources)
  (let ([url (encode-parameters-in-url name program/resources)])
    (log-debug (format "Sending: ~s"  (url->string url)))
    ;; FIXME: get errors here and do something reasonable.
    (let* ([ip (get-impure-port url)]
           [headers (purify-port ip)]
           [status-code (get-status-code headers)])
      (cond
        [(= status-code 200)
         (port->bytes ip)]
        [else
         (raise (make-exn:fail (port->bytes ip)
                               (current-continuation-marks)))]))))


;; get-status-code: string -> number
;; Given the 
(define (get-status-code response-headers)
  (string->number (second (regexp-match #px"^[^\\s]+\\s([^\\s]+)" response-headers))))


;; encode-parameters-in-url: string program/resources -> url
;; Encodes the parameters we need to pass in to get the
(define (encode-parameters-in-url name program/resources)
  (string->url
   (string-append (current-server-url) 
                  "?"
                  (alist->form-urlencoded 
                   (list* (cons 'name name)
                          (cons 'program-stx
                                (format "~s" (map stx->sexp 
                                                  (program/resources-program 
                                                   program/resources))))
                          (map (lambda (a-resource)
                                 (log-debug (format "Sending resource ~s~n" (send a-resource get-name)))
                                 (cons 'resource (format "~s"
                                                         (list (send a-resource get-name)
                                                               (send a-resource get-bytes)))))
                               (program/resources-resources program/resources)))))))


(provide/contract [build-android-package
                   (string? program/resources? . -> . bytes?)])