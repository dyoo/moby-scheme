#lang scheme/base
(require (except-in "compiler/lang.ss")
         "stub/parser.ss"
         "runtime/stx.ss"
         "stub/world-config.ss"
         "stub/jsworld.ss"
         "serve.ss"
         "stub/world.ss"
         "stub/net.ss"
         "compiler/effect-struct.ss"
	 "stub/location.ss"
         scheme/runtime-path
         (for-syntax scheme/base
                     "stx-helpers.ss")
         (prefix-in base: scheme/base))



(define-for-syntax source-code #'not-initialized-yet)


;; Zero out js-big-bang in favor of the js-big-bang/source that'll fire off at the end.
(define-syntax (-js-big-bang stx)
  (syntax-case stx ()
    [(_ world0 handlers ...)
     (syntax/loc stx
       (void world0 handlers ...))]))


(define-syntax (-#%module-begin stx)
  (syntax-case stx ()
    [(mb form ...)
     (begin
       (let ([name (symbol->string (or (syntax-property stx 
                                                        'enclosing-module-name)
                                       'unknown))])
         (set! source-code (map syntax->stx (syntax->list #'(form ...))))
         (with-syntax ([source-code source-code]
                       [module-name name])
           (syntax/loc stx 
             (base:#%module-begin
              form ...
              (compile-and-serve 'source-code module-name)
              )))))]))


;; include: textually include content from another module.
(define-syntax (include stx)
  (syntax-case stx ()
    [(_ path)
     (with-syntax ([(body ...)
                    (let ([result
                           (parameterize ([read-accept-reader #t]
                                          [read-decimal-as-inexact #f])
                             (let* ([pathname (syntax-e #'path)]
                                    [ip (open-input-file pathname)])
                               (let loop ()
                                 (let ([datum (read ip)])
                                   (cond
                                     [(eof-object? datum)
                                      '()]
                                     [else
                                      (cons (datum->syntax stx datum stx) 
                                            (loop))])))))])
                      (cond
                        [(= 1 (length result))
                         (syntax-case (car result) ()
                           [(module a-name a-lang a-body ...)
                            (syntax->list #'(a-body ...))]
                           [else
                            result])]
                        [else
                         result]))])
       (syntax/loc stx
         (base:begin body ...)))]))



;; Require: currently broken.
(define-syntax (hacky-require stx)
  (syntax-case stx ()
    [(_ path)
     (cond
       #;[(and (syntax-e #'path)
               (string? (syntax-e #'path))
               (or (string=? (syntax-e #'path) "moby/bootstrap")
                   (string=? (syntax-e #'path) "moby/bootstrap-teachpack.ss")))
          (with-syntax ([start (datum->syntax stx 'start)])
            (syntax/loc stx 
              (require (file "collects/bootstrap-teachpack.ss"))
              #;(base:begin
                 (define (start title background playerImg targetImgs objectImgs  
                                update-player update-target update-object 
                                collide? offscreen?)
                   (void))
                 (void))))]
       [else
        (raise-syntax-error #f "Not currently implemented" stx)])]
    [else
     (raise-syntax-error #f "Not currently implemented" stx)]))



(provide (except-out (all-from-out "compiler/lang.ss") #%module-begin provide require)
         (rename-out (-#%module-begin #%module-begin))
         (rename-out (hacky-require require))
         
         (all-from-out "stub/parser.ss")
         (all-from-out "stub/world.ss")
         (all-from-out "stub/net.ss")
         (all-from-out "compiler/effect-struct.ss")
	 (all-from-out "stub/location.ss")
 
         include
         
         void
         
	 remove

         provide
         
         ;; Configuration handlers
         on-key on-key!
         on-tick on-tick!
         on-location-change on-location-change!
         on-tilt on-tilt!
         on-acceleration on-acceleration!
         on-shake on-shake!
         on-redraw on-draw
         stop-when
         initial-effect
         
         
         ;; jsworld
         (rename-out (-js-big-bang js-big-bang))
         js-div
         js-p
         js-button
         js-button!
         js-input
         #;js-bidirectional-input
         js-img
         js-text
         js-node
         js-select

         #;get-input-value)