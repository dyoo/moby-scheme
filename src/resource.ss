#lang scheme/base

(require scheme/class
         scheme/contract)

(define resource<%>
  (interface () 
    save!   ;; path -> void    
    ))


(define named-bytes-resource%
  (class* object% (resource<%>)
    (define/public (save! a-path)
      ;; fixme
      (void))))



(provide/contract [resource<%> interface?]
                  [named-bytes-resource% class?])
