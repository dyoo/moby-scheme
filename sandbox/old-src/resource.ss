#lang scheme/base

(require scheme/class
         scheme/contract)

;; A resource is something that should be associated to a program.  Examples include images
;; and music files.

(define resource<%>
  (interface () 
    save!      ;; path -> void    
    get-name   ;; -> string
    get-bytes  ;; -> bytes
    ))


(define named-bytes-resource%
  (class* object% (resource<%>)
    (super-new)
    (init-field name)
    (init-field bytes)
    
    (define/public (save! a-path)
      (call-with-output-file (build-path a-path name)
        (lambda (op)
          (write-bytes bytes op))))

    
    (define/public (get-name)
      name)
    
    (define/public (get-bytes)
      bytes)))



(provide/contract [resource<%> interface?]
                  [named-bytes-resource% class?])
