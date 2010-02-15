#lang scheme/base


(require scheme/contract
         scheme/local
         "compile-helpers.ss"
         "image-lift.ss"
         "compiler/helpers.ss")

;; A program/resources consists of program source, and the set of named bitmaps
;; associated to it.
;; TODO: We may expand this definition to handle other resource types like music
;; and other media.
(define-struct program/resources (program named-bitmaps))
  

;; open-program/resources: path -> program/resources
(define (open-program/resources a-path)
  (local [(define source-code (open-beginner-program a-path))
          (define named-bitmaps (lift-images! source-code))]
    (make-program/resources (parse-text-as-program source-code 
                                                   (if (string? a-path)
                                                       a-path
                                                       (path->string a-path)))
                            named-bitmaps)))
               


;; program/resources-write-bitmaps!: program/resources path -> (listof named-bitmap)
(define (program/resources-write-named-bitmaps! a-program/resources dest-dir)
  (local [(define named-bitmaps (program/resources-named-bitmaps a-program/resources))]
    (for ([bm named-bitmaps])
      (named-bitmap-save bm dest-dir))
    named-bitmaps))


(provide/contract [struct program/resources 
                          ([program program?]
                           [named-bitmaps (listof named-bitmap?)])]

                  [open-program/resources 
                   (path-string? . -> . program/resources?)]
                  
                  [program/resources-write-named-bitmaps!
                   (program/resources? path-string? . -> . (listof named-bitmap?))])