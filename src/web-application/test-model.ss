#lang scheme/base
(require "model.ss"
         scheme/file
         (planet schematics/schemeunit:3)
         (planet schematics/schemeunit:3/text-ui)
         scheme/stxparam)
(require (for-syntax scheme/base))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Small helpers to make the test cases easier to write.
;; Creates an identifier M.
(define-syntax-parameter M
  (lambda (stx)
    (raise-syntax-error #f "M not bound yet and must be used in the context of a with-test-model form.")))

(define-syntax (with-test-model stx)
  (syntax-case stx ()
    [(_ body ...)
     (with-syntax ([model 'model])
       (syntax/loc stx
          (let ([model #f])
            (syntax-parameterize 
             ([M (make-rename-transformer #'model)])             
             (dynamic-wind (lambda ()
                             (let ([filename (make-temporary-file)])
                               (delete-file filename)
                               (set! model (make-model filename))))
                           (lambda ()
                             body ...)
                           (lambda ()
                             (delete-model! model)))))))]))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

   
(define test-module
  (test-suite
   "test-model.ss"   
   (test-case
    "Create and delete"
    (with-test-model
     (void)))))



(run-tests test-module)