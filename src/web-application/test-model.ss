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
(define (call-with-test-model f)
  (let ([model #f])
    (dynamic-wind (lambda ()
                    (set! model (make-model "tmp-model")))
                  (lambda ()
                    (f model))
                  (lambda ()
                    #;(void)
                    (delete-model! model)))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

   
(define test-module
  (test-suite
   "test-model.ss"

   (test-case
    "Unknown user"
    (call-with-test-model
     (lambda (a-model)
       (check-false (model-find-user a-model "no-such-user@invisible.com")))))
   
   (test-case
    "add a user."
    (call-with-test-model
     (lambda (a-model)
       (model-add-user! a-model "Danny Yoo" "dyoo@cs.wpi.edu")
       (let ([user
              (model-find-user a-model "dyoo@cs.wpi.edu")])
         (check-equal? (user-name user) "Danny Yoo")))))

   (test-case
    "add a few more users."
    (call-with-test-model
     (lambda (a-model)
       (model-add-user! a-model "Danny Yoo" "dyoo@cs.wpi.edu")
       (model-add-user! a-model "Daniel Yoo" "dyoo@hkn.eecs.berkeley.edu")
       (check-equal?
        (user-name (model-find-user a-model "dyoo@cs.wpi.edu"))
        "Danny Yoo")
       (check-equal?
        (user-name (model-find-user a-model "dyoo@cs.wpi.edu"))
        "Daniel Yoo")
       (check-false (model-find-user a-model "dyoo@hashcollision.org")))))
       
   

   
   (test-case
    "compilation"
    (call-with-test-model
     (lambda (a-model)
       (let* ([a-user (model-add-user! a-model "Danny Yoo" "dyoo@cs.wpi.edu")]
              [binary
               (model-compile-source a-model
                                     (make-source 1 "hello" #"\n\n\n\"hello world\" (big-bang 0 0 1 false)" (now) a-user)
                                     (make-platform:j2me))])
         (check-true (binary? binary))))))))


(run-tests test-module)