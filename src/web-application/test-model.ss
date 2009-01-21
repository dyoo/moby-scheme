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
       (check-false (model-find-user a-model #:email "no-such-user@invisible.com")))))
   
   (test-case
    "add a user."
    (call-with-test-model
     (lambda (a-model)
       (model-add-user! a-model "Danny Yoo" "dyoo@cs.wpi.edu")
       (let ([user
              (model-find-user a-model #:email "dyoo@cs.wpi.edu")])
         (check-equal? (user-name user) "Danny Yoo")))))

   (test-case
    "add a few more users."
    (call-with-test-model
     (lambda (a-model)
       (model-add-user! a-model "Danny Yoo" "dyoo@cs.wpi.edu")
       (model-add-user! a-model "Daniel Yoo" "dyoo@hkn.eecs.berkeley.edu")
       (check-equal?
        (user-name (model-find-user a-model #:email "dyoo@cs.wpi.edu"))
        "Danny Yoo")
       (check-equal?
        (user-name (model-find-user a-model #:email "dyoo@hkn.eecs.berkeley.edu"))
        "Daniel Yoo")
       (check-false (model-find-user a-model #:email "dyoo@hashcollision.org")))))
       
   
   (test-case
    "Add a user and a source."
    (call-with-test-model
     (lambda (a-model)
       (let* ([user (model-add-user! a-model "Danny Yoo" "dyoo@cs.wpi.edu")]
              [source (model-add-source! a-model 
                                         "Happy Days"
                                         #"\n\n\nBlah" user)])
         (check-equal? (source-name source) "Happy Days")
         (check-true (bytes=? (source-code source) #"\n\n\nBlah"))
         (check-equal? (source-user source) user)))))
   
   

   
   (test-case
    "compilation"
    (call-with-test-model
     (lambda (a-model)
       (let* ([a-user 
               (model-add-user! a-model "Danny Yoo" "dyoo@cs.wpi.edu")]
              [a-source 
               (model-add-source! 
                a-model 
                "hello" 
                #"\n\n\n \"hello world\" (big-bang 0 0 1 false)"
                a-user)]
              [binary
               (model-compile-source! a-model a-source j2me)])
         (check-true (binary? binary))))))))


(run-tests test-module)