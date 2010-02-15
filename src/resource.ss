#lang scheme/base

(require scheme/class
         scheme/contract)

;; A resource is something that should be associated to a program.  Examples include images
;; and music files.

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
