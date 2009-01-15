#lang scheme/base
(require "model.ss"
         scheme/file
         "../utils.ss"
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
                             (set! model (make-model "tmp-model")))
                           (lambda ()
                             body ...)
                           (lambda ()
                             (void)
                             #;(delete-model! model)))))))]))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

   
(define test-module
  (test-suite
   "test-model.ss"

   #;(test-case
    "Create and delete"
    (with-test-model
     (void)))
   
   (test-case
    "compilation"
    (with-test-model
     (let ([binary
            (compile-source M
                            (make-source "id" "hello" #"\n\n\n\"hello world\" (big-bang 0 0 1 false)" (now) #f)
                            (make-platform:j2me))])
       (check-true (binary? binary)))))))


(run-tests test-module)