#lang scheme

(require syntax/docprovide)


;; Get the list of symbols in intermediate level.
(define docs
  (lookup-documentation '(lib "htdp-intermediate-lambda.ss" "lang") 'procedures))

(define (process-category cat)
  (map (lambda (item)
         (first item))
       (rest cat)))


(define ids 
  (apply append
         (map (lambda (cat)
                (process-category cat))
              docs)))
