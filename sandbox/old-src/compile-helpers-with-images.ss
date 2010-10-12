#lang scheme/base

(require scheme/gui/base
         scheme/file
         scheme/class
         scheme/contract
         scheme/local
         "collects/moby/runtime/stx.ss"
         "stx-helpers.ss"
         "compile-helpers.ss"
         "program-resources.ss"
         "image-lift.ss")



;; parse-text-as-program: text -> program
;; Given a text, returns a program as well.
(define (parse-text-as-program a-text [source-name "<unknown>"])
  (let* ([ip (open-input-text-editor a-text)])
    (port-count-lines! ip)
    (parameterize ([read-accept-reader #t]
		   [read-decimal-as-inexact #f])
      (let ([stx (read-syntax source-name ip)])
        (syntax-case stx ()
          [(module name lang (#%module-begin body ...))
           (map syntax->stx (syntax->list #'(body ...)))]
          [(module name lang body ...)
           (map syntax->stx (syntax->list #'(body ...)))]
          [else
           (error 'moby
                  (string-append "The input does not appear to be a Moby module; "
                                 "I don't see a \"#lang moby\" at the top of the file."))])))))


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


;; open-beginner-program: path-string -> text%
;; Opens up the beginner-level program.
(define (open-beginner-program path)
  (define text (new text%))
  (send text insert-file (if (path? path) 
                             (path->string path)
                             path))
  text)


(provide/contract 
 [parse-text-as-program (((is-a?/c text%)) ((or/c string? false/c)) . ->* .  (listof stx?))]
 [lift-images-to-directory ((is-a?/c text%) path? . -> . (listof named-bitmap?))]
 [open-beginner-program (path-string? . -> . (is-a?/c text%))]
 [open-program/resources (path-string? . -> . program/resources?)])
 