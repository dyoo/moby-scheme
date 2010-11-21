#lang racket


(define files-to-change
  (find-files (lambda (p) 
		(and (file-exists? p)
		     (bytes=? (filename-extension p)
			      #"rkt")))))

(define (change-version b from to)
  (regexp-replace* (format ":~a" from)
		   b
		   (format ":~a" to)))
		   

(define from (vector-ref (current-command-line-arguments) 0))
(define to (vector-ref (current-command-line-arguments) 1))
(printf "Changing ~a to ~a\n"
	from to)
(printf "Press enter to continue...")
(read-line)
(for ([f files-to-change])
  (let ([b (file->bytes f)])
    (call-with-output-file f
      (lambda (op)
	(write-bytes (change-version b) op))
      #:exists 'replace)))
