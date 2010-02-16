#lang scheme/base

(require scheme/gui/base
         scheme/file
         scheme/class
         scheme/contract
         scheme/local
         "compile-helpers.ss"
         "program-resources.ss"
         "image-lift.ss")


;; lift-images: text path -> (listof named-bitmap)
;; Lifts up the image snips in the text, writing them into the resource directory.
;; The snips in the text will be replaced with the expression (create-image <path>)
;; where path refers to the file saves in the resource directory.
(define (lift-images-to-directory a-text resource-dir)
  (make-directory* resource-dir)
  (let ([named-bitmaps (lift-images! a-text)])
    (for ([nb named-bitmaps])
      (named-bitmap-save nb resource-dir))
    named-bitmaps))



;; open-program/resources: path -> program/resources
(define (open-program/resources a-path)
  (local [(define source-code (open-beginner-program a-path))
          (define named-bitmaps (map named-bitmap->resource (lift-images! source-code)))]
    (make-program/resources (parse-text-as-program source-code 
                                                   (if (string? a-path)
                                                       a-path
                                                       (path->string a-path)))
                            named-bitmaps)))


(provide/contract 
 [lift-images-to-directory ((is-a?/c text%) path? . -> . (listof named-bitmap?))]
 [open-program/resources 
  (path-string? . -> . program/resources?)])
 