#lang scheme/base
(require scheme/class
         scheme/gui/base 
         scheme/list
         mrlib/cache-image-snip
         scheme/contract)

(define-struct named-bitmap (name bitmap))

;; Image lifting


(provide/contract [struct named-bitmap [(name string?)
                                        (bitmap (is-a?/c bitmap%))]]
                  [named-bitmap-save (named-bitmap? path-string? . -> . any)]
                  [lift-images! ((is-a?/c text%)
                                 . -> . (listof named-bitmap?))])

;; lift-images!: text -> (listof named-bitmap)
;; Lifts up the image snips in the text, writing them into the resource directory.
;; The snips in the text will be replaced with the expression (create-image <path>)
;; where path refers to the file saves in the resource directory.
;; Mutates the text, and produces a list of bitmap objects that should be saved.
(define (lift-images! a-text)
  (let loop ([a-snip (send a-text find-first-snip)])
    (cond
      [(not a-snip)
       empty]
      [(image-snip? a-snip)
       (let* ([file-name (make-image-name)]
              [bitmap (send a-snip get-bitmap)]
              [replacement-snip (make-object string-snip%
                                  (format "(-kernel-create-image ~s)" 
                                          (string-append "/" file-name)))])
         (send a-text set-position 
               (send a-text get-snip-position a-snip)
               (+ (send a-text get-snip-position a-snip) 
                  (send a-snip get-count)))
         (send a-text insert replacement-snip)
         (cons (make-named-bitmap file-name bitmap)
               (loop (send replacement-snip next))))]
      [else
       (loop (send a-snip next))])))


;; named-bitmap-save: named-bitmap path-string -> void
;; Saves the named bitmap under the given directory.
(define (named-bitmap-save a-named-bitmap a-dir)
  (let ([a-path
         (build-path a-dir (named-bitmap-name a-named-bitmap))])
    (send (named-bitmap-bitmap a-named-bitmap) save-file (path->string a-path) 
          'png)))


;; make-image-name: -> string
;; Makes a new image name.
(define make-image-name
  (let ([i 0])
    (lambda ()
      (begin0 (string-append "image-" (number->string i) ".png")
              (set! i (add1 i))))))


;; image-snip?: snip ->  boolean
;; Returns true if this looks like an image snip.
(define (image-snip? a-snip)
  (or (is-a? a-snip image-snip%)
      (is-a? a-snip cache-image-snip%)))