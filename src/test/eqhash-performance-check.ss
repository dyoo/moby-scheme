#lang s-exp "../moby-lang.ss"

;; A little performance test.

(define-struct blah ())

(define elements (buid-list 10000 (lambda (x) (make-blah))))
  
(define ht (make-hasheq))
(for-each (lambda (x) (hash-set! ht x true)) elements)
          
(for-each (lambda (x)
            (cond
              [(hash-ref ht x false) 'ok]
              [else
               (error 'broke)]))
