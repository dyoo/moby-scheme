#lang scheme/base
(require (except-in "compiler/lang.ss" require provide)
         "stub/parser.ss"
         "stub/world-config.ss"
         "stub/jsworld.ss"
         (for-syntax scheme/base 
                     scheme/stxparam)
         scheme/stxparam)


(define-for-syntax source-code #'not-initialized-yet)


(define-syntax (-js-big-bang stx)
  (syntax-case stx ()
    [(_ world0 handlers ...)
     (with-syntax ([source-code source-code])
       (syntax/loc stx
         (begin 
           (js-big-bang/source (quote-syntax #'source-code)
                               world0
                               handlers ...))))]))


(define-syntax (-#%module-begin stx)
  (syntax-case stx ()
    [(_ form ...)
     (begin
       (set! source-code #'(form ...))
       (syntax/loc stx 
         (#%module-begin
          form ...)))]))





(provide (except-out (all-from-out "compiler/lang.ss") #%module-begin)
         (all-from-out "stub/parser.ss")
         (rename-out (-#%module-begin #%module-begin))

         ;; Configuration handlers
         on-key on-key*
         on-tick on-tick*
         on-location-change on-location-change*
         on-tilt on-tilt*
         on-acceleration on-acceleration*
         on-shake on-shake*
         on-redraw on-draw
         stop-when
         
         
         ;; jsworld
         (rename-out (-js-big-bang js-big-bang))
         js-div
         js-p
         js-button
         js-button*
         js-input
         js-bidirectional-input
         js-img
         js-text
         js-node)