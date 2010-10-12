#lang s-exp "../moby-lang.ss"

;; A little performance test.

;; On my Macbook pro, (OS X 10.5.8, 2.33Ghz)
;; Moby 2.26: takes about 1300 milliseconds


(define BIG-NUMBER 40000)

(define-struct blah ())

(define elements (build-list BIG-NUMBER (lambda (x) (make-blah))))
  
(define ht (make-hasheq))

"starting"

(for-each (lambda (x) (hash-set! ht x true)) elements)
          
(for-each (lambda (x)
            (cond
              [(hash-ref ht x false) 'ok]
              [else
               (error 'broke "It broke.")]))
          elements)
                    
(for-each (lambda (x)
            (cond
              [(hash-ref ht x false)
               (error 'broke "It broke.")]
              [else
               'ok]))
          (build-list BIG-NUMBER (lambda (x) (make-hash))))

"done"
