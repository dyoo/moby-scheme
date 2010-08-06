#lang scheme/base
(require (for-syntax scheme/base)
         scheme/file
         scheme/path
         scheme/contract)

(provide/contract [fill-template (string? hash? . -> . string?)]
                  [fill-template-port (input-port? output-port? hash? . -> . any)]
                  [fill-template-file (path-string? path-string? hash? . -> . any)]
                  [replace-template-file (path-string? path-string? hash? . -> . any)])

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
  (let ([pattern #px"\\<\\<([-A-Za-z]+)\\>\\>"])
    (cond
      [(regexp-match pattern a-template)
       =>
       (lambda (a-match)
         ;; I want to use regexp-replace*, but there's a bug in
         ;; Racket 5.0 that prevents me from doing so: the type
         ;; signature of regexp-replace* is incompatible with
         ;; previous versions of plt-scheme.
         (fill-template (regexp-replace pattern
                                        a-template
                                        (lambda (_ hole-name)
                                          (stringify
                                           (hash-ref mappings hole-name))))
                        mappings))]
      [else
       a-template])))


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



;; fill-template-port: input-port output-port hashtable -> void
(define (fill-template-port inp outp mappings)
  (for ([line (in-lines inp)])
    (display (fill-template line mappings) outp)
    (newline outp)))



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



(define (replace-template-file dest-dir a-path mappings)
  (fill-template-file (build-path dest-dir (string-append a-path ".template"))
                      (build-path dest-dir a-path)
                        mappings)
  (delete-file (build-path dest-dir (string-append a-path ".template"))))



(define-syntax (build-mappings stx)
  (syntax-case stx ()
    [(_ (k v) ...)
     (andmap identifier? (syntax->list #'(k ...)))
     (with-syntax ([(k ...) (map (lambda (s) 
                                   (symbol->string (syntax-e s)))
                                 (syntax->list #'(k ...)))])
       (syntax/loc stx 
         (let ([ht (make-hash)])
           (hash-set! ht k v) ...
           ht)))]
    [(_ (k v) ...)
     (not (andmap identifier? (syntax->list #'(k ...))))
     (let ([bad-non-identifier-stx
            (findf (lambda (stx) (not (identifier? stx))) (syntax->list #'(k ...)))])
       (raise-syntax-error #f "Not an identifier" stx bad-non-identifier-stx))]))
