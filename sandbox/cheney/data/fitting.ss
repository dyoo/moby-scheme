#lang scheme/gui
(require plot)

;; read-data: path -> (listof (vectorof number number number))
(define (read-data path)
  (for/list ([line (in-lines (open-input-file path))])
    (list->vector (append (map string->number (regexp-split #px"\\s+" line)) (list 1)))))


(define safari-sum-using-exception
  (read-data "safari/sumUsingException.txt"))

(define safari-sum-iter-using-exception
  (read-data "safari/sumIterUsingException.txt"))

(define safari-sum-using-timeout
  (read-data "safari/sumUsingTimeout.txt"))

(define safari-sum-iter-using-timeout
  (read-data "safari/sumIterUsingTimeout.txt"))


(define firefox-sum-using-exception
  (read-data "firefox-3.1/sumUsingException.txt"))

(define firefox-sum-iter-using-exception
  (read-data "firefox-3.1/sumIterUsingException.txt"))

(define firefox-sum-using-timeout
  (read-data "firefox-3.1/sumUsingTimeout.txt"))

(define firefox-sum-iter-using-timeout
  (read-data "firefox-3.1/sumIterUsingTimeout.txt"))




(define (sequence->list a-seq)
  (for/list ((x a-seq))
    x))

(define (generate-table data)
  (let ([ht (make-hash)]
        [counts (make-hash)])
    (for ([row data])
      (let ([key (vector-ref row 0)]
            [value (vector-ref row 1)])
        (hash-set! ht  
                   key 
                   (+ value (hash-ref ht key 0)))
        (hash-set! counts key (add1 (hash-ref counts key 0)))))
    (for/list ([key (sort (sequence->list (in-hash-keys ht)) <)])
      (vector key (exact->inexact (/ (hash-ref ht key)
                                     (hash-ref counts key)))))))

(define (print-table a-table)
  (for ([row a-table])
    (printf "~a\t~a~n" (vector-ref row 0) (vector-ref row 1))))



;; Assumption: the function is linear?
(define (fit-fun number-of-trampolines cost-of-summation-work cost-of-single-trampoline)
  (+ cost-of-summation-work (* number-of-trampolines cost-of-single-trampoline)))


(define (find-fit data)
  (fit fit-fun '((cost-of-summation-work 1)
                                (cost-of-single-trampoline 100))
                      data))


(define (plot-fit data)
  (plot (points data)
        #;(mix (points data)
               (line (fit-result-function (find-fit data))))
        #:x-min 0
        #:x-max 100
        #:y-min 1000
        #:y-max 20000))