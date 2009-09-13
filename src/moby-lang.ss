#lang scheme/base
(require (except-in "compiler/lang.ss" require provide)
         "stub/parser.ss"
         "compiler/stx.ss"
         "stub/world-config.ss"
         "stub/jsworld.ss"
         "stub/world.ss"
         "stub/net.ss"
         "stub/private/world-effects.ss"
	 "stub/location.ss"
         (for-syntax scheme/base
                     "stx-helpers.ss"))


(define-for-syntax source-code 'not-initialized-yet)


(define-syntax (-js-big-bang stx)
  (syntax-case stx ()
    [(_ world0 handlers ...)
     (cond
       [(eq? source-code 'not-initialized-yet)
        (syntax/loc stx
          (begin
            (js-big-bang/source (list (datum->stx `(js-big-bang world0 handlers ...)
                                                  (make-Loc 0 0 0 "")))
                                world0 handlers ...)))]
       [else
        (with-syntax ([source-code source-code])
          (syntax/loc stx
            (begin 
              (js-big-bang/source 'source-code
                                  world0
                                  handlers ...))))])]))


(define-syntax (-#%module-begin stx)
  (syntax-case stx ()
    [(_ form ...)
     (begin
       (set! source-code (map syntax->stx (syntax->list #'(form ...))))
       (syntax/loc stx 
         (#%module-begin
          form ...)))]))



(provide (except-out (all-from-out "compiler/lang.ss") #%module-begin)
         (rename-out (-#%module-begin #%module-begin))
         (all-from-out "stub/parser.ss")
         (all-from-out "stub/world.ss")
         (all-from-out "stub/net.ss")
         (all-from-out "stub/private/world-effects.ss")
	 (all-from-out "stub/location.ss")

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