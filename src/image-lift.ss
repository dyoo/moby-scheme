#lang scheme/base
(require scheme/class
         scheme/gui/base 
         scheme/list
         scheme/port
         mrlib/cache-image-snip
         scheme/contract
         "utils.ss"
         "resource.ss"
         "collects/moby/runtime/stx.ss")

(define-struct named-bitmap (name bitmap))

;; Image lifting





(provide/contract [struct named-bitmap [(name string?)
                                        (bitmap (is-a?/c bitmap%))]]
                  [named-bitmap->resource (named-bitmap? . -> . (is-a?/c resource<%>))]
                  [named-bitmap-save (named-bitmap? path-string? . -> . any)]
                  [lift-images! ((is-a?/c text%)
                                 . -> . (listof named-bitmap?))]
                  
                  [lift-images/stx (stx? . -> . (values stx? (listof named-bitmap?)))]
                  [lift-images/stxs ((listof stx?) . -> . (values (listof stx?) (listof named-bitmap?)))])


(define named-bitmap-resource%
  (class* object% (resource<%>)
    (init-field named-bitmap)
    (super-new)
    
    (define/public (save! a-path)
      (named-bitmap-save named-bitmap a-path))
    
    (define/public (get-name)
      (named-bitmap-name named-bitmap))
    
    (define/public (get-bytes)
      (with-temporary-directory
       (lambda (a-dir)
         (save! a-dir)
         (call-with-input-file (build-path a-dir (get-name))
           (lambda (ip)
             (port->bytes ip))))))))


;; Turns a named bitmap into a resource.
(define (named-bitmap->resource a-named-bitmap)
  (new named-bitmap-resource% [named-bitmap a-named-bitmap]))





;; lift-images!: text -> (listof named-bitmap)
;; Lifts up the image snips in the text.
;; The snips in the text will be replaced with the expression (open-image-url <path>)
;; where path refers to the file name of the named bitmap.
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
                                  (format "(open-image-url ~s)" 
                                          file-name))])
         (send a-text set-position 
               (send a-text get-snip-position a-snip)
               (+ (send a-text get-snip-position a-snip) 
                  (send a-snip get-count)))
         (send a-text insert replacement-snip)
         (cons (make-named-bitmap file-name bitmap)
               (loop (send replacement-snip next))))]
      [else
       (loop (send a-snip next))])))


;; lift-images/stx: stx -> (values stx (listof named-bitmap))
;; Lift out the image snips in an stx.
(define (lift-images/stx a-stx)
  (cond
    [(stx:list? a-stx)
     (let-values ([(lifted-elts named-bitmaps)
                   (lift-images/stxs (stx-e a-stx))])
       (values (datum->stx #f lifted-elts (stx-loc a-stx))
               named-bitmaps))]
    
    [(stx:atom? a-stx)
     (cond [(image-snip? (stx-e a-stx))
            (let* ([filename (make-image-name)]
                   [bitmap (send (stx-e a-stx) get-bitmap)]
                   [replacement-stx (datum->stx #f `(open-image-url ,filename)
                                                (stx-loc a-stx))])
              (values replacement-stx (list (make-named-bitmap filename bitmap))))]
           [else
            (values a-stx empty)])]))


;; lift-images/stxs: (listof stx) -> (values (listof stx) (listof named-bitmap))
(define (lift-images/stxs stxs)
  (cond
    [(empty? stxs)
     (values empty empty)]
    [else
     (let-values ([(lifted-stx named-bitmaps)
                   (lift-images/stx (first stxs))]
                  [(rest-lifted-stxs rest-named-bitmaps)
                   (lift-images/stxs (rest stxs))])
       (values (cons lifted-stx rest-lifted-stxs)
               (append named-bitmaps rest-named-bitmaps)))]))



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