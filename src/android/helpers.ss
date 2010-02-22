#lang scheme/base

(require scheme/contract)

(require file/gzip)

(define (gzip-bytes bytes)
  (let ([op (open-output-bytes)])
    (gzip-through-ports (open-input-bytes bytes) op #f 0)
    (get-output-bytes op)))

(define (gunzip-bytes bytes)
  (let ([op (open-output-bytes)])
    (deflate (open-input-bytes bytes) op)
    (get-output-bytes op)))


;; gzip-string: string -> string
(define (gzip-string a-string)
  (format "~s" (gzip-bytes (string->bytes/utf-8 a-string))))

;; gunzip-string: string -> string
(define (gunzip-string a-string)
  (bytes->string/utf-8 (gunzip-bytes (read (open-input-string a-string)))))



                      
(provide/contract [gzip-bytes (bytes? . -> . bytes?)]
                  [gunzip-bytes (bytes? . -> . bytes?)]
                  [gzip-string (string? . -> . string?)]
                  [gunzip-string (string? . ->  . string?)])