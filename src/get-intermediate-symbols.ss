#lang scheme
(require syntax/docprovide
         "compiler/toplevel.ss"
         "compiler/env.ss"
         (only-in srfi/1 lset-union lset-difference lset-intersection))


;; Get the list of symbols in intermediate level.
(define docs
  (lookup-documentation '(lib "htdp-intermediate-lambda.ss" "lang") 'procedures))

(define (process-category cat)
  (map (lambda (item)
         (first item))
       (rest cat)))

(define all-ids 
  (apply append
         (map (lambda (cat)
                (process-category cat))
              docs)))


(define implemented-ids
  (env-keys (get-toplevel-env)))


(define (union s1 s2)
  (lset-union symbol=? s1 s2))

(define (intersect s1 s2)
  (lset-intersection symbol=? s1 s2))

(define (subtract s1 s2)
  (lset-difference symbol=? s1 s2))
