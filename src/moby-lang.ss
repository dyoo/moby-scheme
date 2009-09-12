#lang scheme/base
(require (except-in "compiler/lang.ss" require provide)
         "stub/parser.ss"
         "stub/world-config.ss"
         "stub/jsworld.ss"
         (for-syntax scheme/base))


(define-syntax (-js-big-bang stx)
  (syntax-case stx ()
    [(_ world0 handlers ...)
     (syntax/loc stx
       (js-big-bang world0 handlers ...))]))




(provide (except-out (all-from-out "compiler/lang.ss"))
         (all-from-out "stub/parser.ss")
         

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
         js-big-bang
         js-div
         js-p
         js-button
         js-button*
         js-input
         js-bidirectional-input
         js-img
         js-text
         js-node)