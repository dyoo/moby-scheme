#lang scheme/base

;; The restricted language we're writing our compiler in.
;; Part of the self-hosting.  The Javascript compiler will be written
;; in a small superset of intermediate-level scheme, so we can then
;; bootstrap things.

(require (prefix-in base: scheme/base)
         scheme/contract
         scheme/class
         scheme/gui/base
         lang/htdp-advanced
         (for-syntax scheme/base)
         "stx.ss"
         "error-struct.ss")

                     
(provide (except-out (all-from-out lang/htdp-advanced)
                     define-struct
                     define
                     quote
                     quasiquote
                     unquote
                     unquote-splicing
                     let
                     letrec
                     let*
                     image?
                     image=?
                     set!
                     not))


(define-syntax (my-define-struct stx)
  (syntax-case stx ()
    [(_ id (fields ...))
     (syntax/loc stx 
       (base:define-struct id (fields ...)
                           #:prefab 
                           #:mutable))]))
    

(base:define (syntax-error msg . stx)
  (raise (make-exn:fail:moby-syntax-error msg (current-continuation-marks) stx)))








;; BIG HACK.
(define (open-input-stx a-path)
  (local [;; open-beginner-program: path-string -> text%
          ;; Opens up the beginner-level program.
          (define (open-beginner-program path)
            (local [(define text (new text%))]
              (begin (send text insert-file (path->string path))
                     text)))
          
          ;; syntax->stx: syntax -> stx
          ;; Go from Scheme's syntax objects to our own.
          (define (syntax->stx a-syntax)
            (cond
              [(pair? (syntax-e a-syntax))
               (let ([elts
                      (map syntax->stx (syntax->list a-syntax))])
                 (datum->stx elts
                             (make-Loc (syntax-position a-syntax)
                                       (syntax-line a-syntax)
                                       (syntax-span a-syntax)
                                       (format "~a" (syntax-source a-syntax)))))]
              [else
               (datum->stx (syntax-e a-syntax)
                           (make-Loc (syntax-position a-syntax)
                                     (syntax-line a-syntax)
                                     (syntax-span a-syntax)
                                     (format "~a" (syntax-source a-syntax))))]))
          
          ; parse-text-as-program: text -> program
          ;; Given a text, returns a program as well.
          (define (parse-text-as-program a-text source-name)
            (let* ([ip (open-input-text-editor a-text)])
              (begin
              (port-count-lines! ip)
              (parameterize ([read-accept-reader #t])
                (let ([stx (read-syntax source-name ip)])
                  (syntax-case stx ()
                    [(module name lang body ...)
                     (map syntax->stx (syntax->list #'(body ...)))]
                    [else
                     (error 'moby
                            (string-append "The input does not appear to be a Moby module; "
                                           "I don't see a \"#lang moby\" at the top of the file."))]))))))]
    (parse-text-as-program (open-beginner-program a-path)
                           a-path)))



;; The following primitives will need support in the runtime,
;; or be handled specially by the preprocessor.
(provide (rename-out (base:provide provide)
                     (base:quote quote)
                     (base:quasiquote quasiquote)
                     (base:unquote unquote)
                     (base:unquote-splicing unquote-splicing)
                     (my-define-struct define-struct)
                     (base:define define)
                     (base:set! set!)
                     (base:not not))

         (all-from-out "stx.ss")
         
         ;; Contract-related stuff: the following will be erased on 
         ;; javascript bootstrapping time.
         provide/contract -> any/c listof list/c or/c false/c natural-number/c hash?

         begin
         void
         
         build-vector
         make-vector
         vector
         vector-length
         vector-ref
         vector-set!
         vector?
         
         case
         
         let let* letrec
         
         ;; The rest of these primitives will be implemented for the kernel.
         ;; Hash stuff
         ;; FIXME: the hash in javascript only accepts strings as keys.
         ;; We should use contracts here.
         hash-set hash-ref hash-remove make-immutable-hasheq hash-map
         ;; To support include and require
         open-input-stx

	 syntax-error

         ;path->string normalize-path path? resolve-module-path build-path
         )