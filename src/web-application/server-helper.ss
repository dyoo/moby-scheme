#lang scheme

(require web-server/http/request-structs
         web-server/http/response-structs
         scheme/contract)



;; make-input-port-response: string input-port -> response
(define (make-input-port-response filename ip)
  (let* ([CHUNK-SIZE (expt 2 16)]
         [headers
          (list (make-header #"Content-Disposition"
                             (bytes-append
                              #"attachment; filename=\""
                              ;; fixme: we need to escape filename
                              (string->bytes/utf-8 filename)
                              #"\"")))])
    (make-response/incremental 200
                               "Okay"
                               (current-seconds)
                               #"application/octet-stream"
                               headers
                               (lambda (send/bytes)
                                 (let loop ()
                                   (let ([chunk
                                          (read-bytes CHUNK-SIZE ip)])
                                     (cond
                                       [(eof-object? chunk)
                                        (void)]
                                       [else
                                        (send/bytes chunk)
                                        (loop)])))))))

(provide/contract [make-input-port-response 
                   (string? input-port? . -> . response/basic?)])