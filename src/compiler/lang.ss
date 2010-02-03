#lang scheme/base

;; The restricted language we're writing our compiler in.
;; Part of the self-hosting.  The Javascript compiler will be written
;; in a small superset of intermediate-level scheme, so we can then
;; bootstrap things.

(require (prefix-in base: scheme/base)
         scheme/contract
         lang/htdp-advanced
         (for-syntax scheme/base)
         "../collects/runtime/stx.ss"
         "../collects/runtime/error-struct.ss"
         "../collects/runtime/error-struct-to-dom.ss")


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
                     not
                     
                     map
                     foldl
                     foldr
                     for-each
                     andmap
                     ormap
                     
                     error
                     ))


(define-syntax (my-define-struct stx)
  (syntax-case stx ()
    [(_ id (fields ...))
     (syntax/loc stx 
       (base:define-struct id (fields ...)
                           #:prefab 
                           #:mutable))]))

(base:define-struct (exn:fail:moby-syntax-error exn:fail) (stxs))


#;(base:define (syntax-error msg . stx)
               (raise (make-exn:fail:moby-syntax-error 
                       (format "~a: ~s" msg (map stx->datum stx))
                       (current-continuation-marks)
                       stx)))








;; BIG HACK.
;; open-input-stx: string -> (listof stx)
#;(define (open-input-stx a-path-string)
  (local [;; open-beginner-program: path-string -> text%
          ;; Opens up the beginner-level program.
          (define (open-beginner-program path)
            (local [(define text (new text%))]
              (begin (send text insert-file path)
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
              (parameterize ([read-accept-reader #t]
                             [read-decimal-as-inexact #f])
                (let loop ()
                  (let ([stx (read-syntax source-name ip)])
                    (cond [(not (eof-object? stx))
                           (cons (syntax->stx stx) (loop))]
                          [else
                           empty])))))))]
    (parse-text-as-program (open-beginner-program a-path-string)
                           a-path-string)))


(base:define-struct (moby-failure exn:fail) (val))



  
;; dom-string-content: dom -> string
;; Gets the string content of the dom.
(define (dom-string-content a-dom)
  (cond
    [(string? a-dom)
     a-dom]
    [(ormap (lambda (an-attrib-pair)
              (and (symbol=? (first an-attrib-pair)
                             'style)
                   (string=? (second an-attrib-pair)
                             "display:none")))
            (second a-dom))
     ""]
    [else
     (foldl (lambda (a-dom rest)
              (string-append rest (dom-string-content a-dom)))
            ""
            (cdr (cdr a-dom)))]))



(define (Loc->string a-loc)
  (format "Location: line ~a, column ~a, span ~a, offset ~a, id ~s" 
          (Loc-line a-loc)
          (Loc-column a-loc)
          (Loc-span a-loc)
          (Loc-offset a-loc)
          (Loc-id a-loc)))


(define-syntax (my-raise stx)
  (syntax-case stx ()
    [(_ val)
     (syntax/loc stx
       (base:raise (make-moby-failure (base:format "~a" 
                                                   (if (moby-error? val)
                                                       (string-append (dom-string-content (moby-error-struct-to-dom-sexp val))
                                                                      "\n"
                                                                      (Loc->string (moby-error-location val)))
                                                       val))
                                      (base:current-continuation-marks) 
                                      val)))]))



(define (my-hash-ref a-hash key default-val)
  (base:hash-ref a-hash key default-val))

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
                     (base:not not)
                     (base:procedure-arity procedure-arity)
                     (base:andmap andmap)
                     (base:ormap ormap)
                     (base:foldl foldl)
                     (base:foldr foldr)
                     (base:map map)
                     (base:for-each for-each)
                     (base:error error)
                     (my-raise raise)
                     )

         ;;(all-from-out "../collects/runtime/stx.ss")

         
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
         
         
         
         ;; WORKAROUND: For some reason, list* isn't in ASL as of 4.2.2.
         ;; We may need to use version-case if this changes in some new version of DrScheme.
         list*
         
         
         ;; The rest of these primitives will be implemented for the kernel.
         ;; Hash stuff
         ;; FIXME: the hash in javascript only accepts strings as keys.
         ;; We should use contracts here.
         ;; disabled: using internal rbtree in the compiler now.
         ;;hash-set hash-ref hash-remove make-immutable-hasheq hash-map
	 make-hash
         make-hasheq
	 hash? hash-set! hash-remove! hash-map hash-for-each
	 (rename-out (my-hash-ref hash-ref))
         
         ;; To support include and require
         #;open-input-stx
         
         ;; syntax-error



	 printf

         ;path->string normalize-path path? resolve-module-path build-path
         )