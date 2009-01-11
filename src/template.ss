#lang scheme/base
(require (for-syntax scheme/base)
         scheme/file
         scheme/path
         scheme/contract)

(provide/contract [fill-template (string? hash? . -> . string?)]
                  [fill-template-file (path-string? path-string? hash? . -> . any)])

(provide build-mappings)


;; fill-template: string hashtable -> string
;; Given a template, fills in all of the holes in the template
;; with values from the mappings.  Each hole is of the form
;; <<[-A-Z]>>
;;
;; Example:
;;
;; (fill-template "Hello <<name>>" #hash(("name" . "Danny")))
;;
;; should produce "Hello Danny".
(define (fill-template a-template mappings)
  (regexp-replace* #px"\\<\\<([-A-Za-z]+)\\>\\>" 
                   a-template
                   (lambda (_ hole-name)
                     (stringify
                      (hash-ref mappings hole-name)))))


;; stringify: X -> string
;; Just make sure we turn something into a string.
(define (stringify thing)
  (cond
    [(string? thing)
     thing]
    [(path? thing)
     (path->string thing)]
    [else
     (format "~a" thing)]))


;; fill-template-file: path path mappings -> void
(define (fill-template-file a-path-in a-path-out mappings)
  (make-directory* (path-only a-path-out))
  ;; fixme: validate that a-path-in and a-path-out are different.
  (call-with-output-file a-path-out
    (lambda (op)
      (call-with-input-file a-path-in 
        (lambda (ip)
          (for ([line (in-lines ip)])
            (display (fill-template line mappings) op)
            (newline op)))))
    #:exists 'replace))

               

(define-syntax (build-mappings stx)
  (syntax-case stx ()
    [(_ (k v) ...)
     (andmap identifier? (syntax->list #'(k ...)))
     (with-syntax ([(k ...) (map (lambda (s) 
                                   (symbol->string (syntax-e s)))
                                 (syntax->list #'(k ...)))])
       (syntax/loc stx 
         (let ([ht (make-hash)])
           ;; fixme: make sure v is a string value
           (hash-set! ht k v) ...
           ht)))]
    [(_ (k v) ...)
     (not (andmap identifier? (syntax->list #'(k ...))))
     (let ([bad-non-identifier-stx
            (findf (lambda (stx) (not (identifier? stx))) (syntax->list #'(k ...)))])
       (raise-syntax-error #f "Not an identifier" stx bad-non-identifier-stx))]))
