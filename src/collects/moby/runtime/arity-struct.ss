#lang s-exp "../../../private/restricted-runtime-scheme.ss"

;; Represents the arities available to a function.

(define-struct arity:fixed (n))
(define-struct arity:variable (min max))
(define-struct arity:mixed (arities))


;; arity?: any -> boolean
;; Produces true if X is a procedure arity.
(define (arity? x)
  (or (arity:fixed? x)
      (arity:variable? x)
      (arity:mixed? x)))


;; Translates an arity to an s-expression.
;; arity->sexp: arity -> s-exp
(define (arity->sexp an-arity)
  (cond
    [(arity:fixed? an-arity)
     (list 'arity:fixed (arity:fixed-n an-arity))]
    [(arity:variable? an-arity)
     (list 'arity:variable (arity:variable-min an-arity) (arity:variable-max an-arity))]
    [(arity:mixed? an-arity)
     (list 'arity:mixed (map arity->sexp (arity:mixed-arities an-arity)))]))


;; Translates an s-expression back to an arity.
(define (sexp->arity an-sexp)
  (cond
    [(list? an-sexp)
     (cond
       [(and (symbol=? (first an-sexp) 'arity:fixed?)
             (= 2 (length an-sexp))
             (number? (second an-sexp)))
        (make-arity:fixed (second an-sexp))]

       [(and (symbol=? (first an-sexp) 'arity:variable?)
             (= 3 (length an-sexp))
             (or (number? (second an-sexp)) (false? (second an-sexp)))
             (or (number? (third an-sexp)) (false? (third an-sexp))))
        (make-arity:variable (second an-sexp) (third an-sexp))]

       [(and (symbol=? (first an-sexp) 'arity:mixed?)
             (list (second an-sexp)))
        (local [(define inner-arities (map sexp->arity (second an-sexp)))]
          (cond
            [(andmap (lambda (x) (or (arity:fixed? x)
                                     (arity:variable? x)))
                     inner-arities)
             (make-arity:mixed inner-arities)]
            [else
             (error 'sexp->arity 
                    (format "Does not look like an arity structure: ~s" an-sexp))]))])]


    [else
     (error 'sexp->arity (format "Does not look like an arity structure: ~s" an-sexp))]))



(provide/contract
 [struct arity:fixed 
         ([n number?])]
 
 [struct arity:variable 
         ([min (or/c number? false)]
          [max (or/c number? false)])]
 
 [struct arity:mixed
         ([arities 
           (listof (or/c arity:fixed? arity:variable?))])]
 
 [arity? (any/c . -> . boolean?)])