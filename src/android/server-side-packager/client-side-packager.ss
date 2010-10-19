#lang racket/base

(require racket/contract
         racket/port
         racket/list
         racket/file
         net/url
         (prefix-in lap: "../local-android-packager.ss")
         (planet dyoo/pack-directory:1/pack-directory))

(define current-server-url (make-parameter 
                            "http://localhost:8888/package"
                            #;"http://go.cs.brown.edu:8888/package/"))


;; build-android-package: string program-path -> bytes
;; Calls out to the compiler web service to get back the android package.
(define (build-android-package name program-path)
  (let ([data (encode-parameters-in-data name program-path)])
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


;; encode-parameters-in-data: string path -> bytes
;; Encodes the parameters we need to pass in to get a program.
;; TODO: GZIP the data, as soon as the web server can support it.
(define (encode-parameters-in-data name program-path)
  (let* ([tmpdir (make-temporary-file "mztmp~a" 'directory #f)]
         [packed-dir-bytes
          (dynamic-wind
           (lambda () (void))
           (lambda ()
             (lap:write-assets name program-path tmpdir)
             (parameterize ([current-directory tmpdir])
               (pack-current-directory)))
           (lambda ()
             (delete-directory/files tmpdir)))])

    (let ([op (open-output-bytes)])
      (write `((name ,name)) op)
      (write-bytes packed-dir-bytes op)
      (get-output-bytes op))))
  




(provide/contract [build-android-package
                   (string? path? . -> . bytes?)])