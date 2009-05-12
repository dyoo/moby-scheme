#lang scheme/gui
(require plot)

;; read-data: path -> (listof (vectorof number number number))
(define (read-data path)
  (for/list ([line (in-lines (open-input-file path))])
    (list->vector (append (map string->number (regexp-split #px"\\s+" line)) (list 1)))))

(define sum-using-exception
  (read-data "safari/sumUsingException.txt"))
(define sum-iter-using-exception
  (read-data "safari/sumIterUsingException.txt"))
(define sum-using-timeout
  (read-data "safari/sumUsingTimeout.txt"))

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