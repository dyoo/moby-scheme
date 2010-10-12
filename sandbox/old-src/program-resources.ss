#lang scheme/base


(require scheme/contract
         scheme/class
         (only-in scheme/list first second empty)
         "collects/moby/runtime/stx.ss"
         "compiler/helpers.ss"
         "resource.ss")

;; A program/resources consists of program source, and the set of named bitmaps
;; associated to it.
;; TODO: We may expand this definition to handle other resource types like music
;; and other media.
(define-struct program/resources (program resources))
  

;; program/resources->sexp: program/resources -> sexp
(define (program/resources->sexp a-program/resources)
  (list (program->sexp (program/resources-program a-program/resources))
        (map (lambda (a-resource)
               (list (send a-resource get-name)
                     (send a-resource get-bytes)))
             (program/resources-resources a-program/resources))))


;; sexp->program/resources: sexp -> program/resources
(define (sexp->program/resources->sexp an-sexp)
  (make-program/resources
   (sexp->program (first an-sexp))
   (map (lambda (resource-sexp)
          (new named-bytes-resource% 
               [name (first resource-sexp)]
               [bytes (second resource-sexp)]))
        (second an-sexp))))



;; program/resources-write-bitmaps!: program/resources path -> void
(define (program/resources-write-resources! a-program/resources dest-dir)
  (for ([a-resource  (program/resources-resources a-program/resources)])
    (send a-resource save! dest-dir)))


(provide/contract [struct program/resources 
                          ([program program?]
                           [resources (listof (is-a?/c resource<%>))])]

                  [program/resources->sexp
                   (program/resources? . -> . any)]
                  
                  [program/resources-write-resources!
                   (program/resources? path-string? . -> . any)])