#lang scheme/base


(define sendmail-available? (make-parameter #t))

(define -send-mail-message/port
  (with-handlers ((void (lambda (exn)
                          (printf "Sendmail not available.~n")
                          (sendmail-available? #f)
                          (lambda (from subject to cc bcc extra-header ...)
                            (raise exn)))))
    (let ([f
           (dynamic-require 'net/sendmail 'send-mail-message/port)])
      (lambda (#:from from 
                      #:subject subject
                      #:to to 
                      #:cc (cc '()) 
                      #:bcc (bcc '()) 
                      #:extra-header (extra-header ""))
        (f from 
           subject 
           to
           cc
           bcc
           extra-header)))))


(provide (rename-out [-send-mail-message/port send-mail-message/port])
         sendmail-available?)
           