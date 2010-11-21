#lang racket


(define files-to-change
  (find-files (lambda (p) 
		(and (file-exists? p)
		     (bytes=? (filename-extension p)
			      #"rkt")))))

(for ([f files-to-change])
  (let ([b (file->bytes f)])
    (call-with-output-file f
      (lambda (op)
	(write-bytes b op))
      #:exists replace)))