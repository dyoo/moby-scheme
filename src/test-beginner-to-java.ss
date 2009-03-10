#lang scheme/base
(require "beginner-to-java.ss"
         "pinfo.ss"
         "env.ss"
         scheme/list
         (planet schematics/schemeunit:3:3)
         (planet schematics/schemeunit:3:3/text-ui))

(define tests (test-suite
               "test-beginner-to-java.ss"

               (test-case
                "empty program has empty analysis"
                (check-equal? (program-analyze empty (make-pinfo empty-env))
                              (make-pinfo empty-env)))
               (test-case
                "simple definition"
                (check-equal? (program-analyze (list '(define pi 3.1415))
                                               (make-pinfo empty-env))
                              (make-pinfo (env-extend empty-env (make-binding:constant 'pi "pi")))))))
                                          



(run-tests tests)