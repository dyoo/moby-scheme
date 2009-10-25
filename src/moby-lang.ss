#lang scheme/base
(require (except-in "compiler/lang.ss")
         "stub/parser.ss"
         "compiler/stx.ss"
         "stub/world-config.ss"
         "stub/jsworld.ss"
         "serve.ss"
         "stub/world.ss"
         "stub/net.ss"
         "compiler/effect-struct.ss"
	 "stub/location.ss"
         (for-syntax scheme/base
                     "stx-helpers.ss"))


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
             (#%module-begin
              (provide (all-defined-out))
              form ...
              (compile-and-serve 'source-code module-name)
              )))))]))



(provide (except-out (all-from-out "compiler/lang.ss") #%module-begin provide require)
         (rename-out (-#%module-begin #%module-begin))
         (all-from-out "stub/parser.ss")
         (all-from-out "stub/world.ss")
         (all-from-out "stub/net.ss")
         (all-from-out "compiler/effect-struct.ss")
	 (all-from-out "stub/location.ss")
         
         load
         
         void
         
	 remove

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
         
         #;get-input-value)