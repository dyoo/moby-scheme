#lang scheme/gui
(require plot)

;; read-data: path -> (listof (vectorof number number))
(define (read-data path)
  (for/list ([line (in-lines (open-input-file path))])
    (list->vector (map string->number (regexp-split #px"\\s+" line)))))

(define sum-using-exception
  (read-data "safari/sumUsingException.txt"))
(define sum-iter-using-exception
  (read-data "safari/sumIterUsingException.txt"))