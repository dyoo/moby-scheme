#lang racket/base

(require racket/contract
         racket/path)
;; Logger module; record programs that have been sent to us.


(define-struct logger (semaphore logfile-path))


;; -make-logger: path-string -> logger
;; Creates a new logger.
(define (-make-logger logfile-path)
  (make-logger (make-semaphore 1)
               (normalize-path logfile-path)))


;; logger-add!: logger string bytes any -> void
;; Write a new entry into the log.
(define (logger-add! a-logger client-ip asset-zip-bytes other-attributes)
  (call-with-semaphore 
   (logger-semaphore a-logger)
   (lambda ()
     (call-with-output-file (logger-logfile-path a-logger)
       (lambda (op)
         (write `((time ,(current-inexact-milliseconds))
                  (client-ip ,client-ip)
                  (asset-zip-bytes ,asset-zip-bytes)
                  (other-attributes ,other-attributes))
                op)
         (newline op))
       #:exists 'append))))




(provide/contract
 [logger? (any/c . -> . boolean?)]
 [rename -make-logger make-logger (path-string? . -> . logger?)]
 [logger-add! (logger? string? bytes? any/c . -> . any)])