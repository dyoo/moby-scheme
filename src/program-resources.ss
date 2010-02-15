#lang scheme/base


(require scheme/contract
         scheme/class
         "image-lift.ss"
         "compiler/helpers.ss")

;; A program/resources consists of program source, and the set of named bitmaps
;; associated to it.
;; TODO: We may expand this definition to handle other resource types like music
;; and other media.
(define-struct program/resources (program named-bitmaps))
  

               


;; program/resources-write-bitmaps!: program/resources path -> void
(define (program/resources-write-resources! a-program/resources dest-dir)
  (for ([a-resource  (program/resources-named-bitmaps a-program/resources)])
    (send a-resource save! dest-dir)))


(provide/contract [struct program/resources 
                          ([program program?]
                           [named-bitmaps (listof named-bitmap?)])]

                  
                  [program/resources-write-resources!
                   (program/resources? path-string? . -> . (listof named-bitmap?))])