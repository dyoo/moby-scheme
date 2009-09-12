#lang scheme/base
(require "../compiler/stx.ss"
         "../compile-helpers.ss")


;; js-big-bang/source: stx world0 . (listof handler) -> void
(define (js-big-bang/source source-code initWorld . handlers)
  (printf "I see the source code is ~s" source-code)
  ;; FIXME
  (void))


(define-struct jsworld-widget (attrs) #:prefab)
(define-struct (jsworld-widget:div jsworld-widget) () #:prefab)
(define-struct (jsworld-widget:p jsworld-widget) () #:prefab)
(define-struct (jsworld-widget:button jsworld-widget) (f ef) #:prefab)
(define-struct (jsworld-widget:input jsworld-widget) (type) #:prefab)
(define-struct (jsworld-widget:bidirectional-input jsworld-widget) (type val-f update-f) #:prefab)
(define-struct (jsworld-widget:img jsworld-widget) (src) #:prefab)
(define-struct (jsworld-widget:text jsworld-widget) (text) #:prefab)
(define-struct (jsworld-widget:node jsworld-widget) (node) #:prefab)


(define (js-div . attrs)
  (make-jsworld-widget:div attrs))

(define (js-p . attrs)
  (make-jsworld-widget:p attrs))

(define (js-button f . attrs)
  (make-jsworld-widget:button attrs f (lambda (w) '())))

(define (js-button* f ef . attrs)
  (make-jsworld-widget:button attrs f ef))

(define (js-input type . attrs)
  (make-jsworld-widget:input attrs type))

(define (js-bidirectional-input type val-f update-f . attrs)
  (make-jsworld-widget:bidirectional-input attrs type val-f update-f))

(define (js-img src . attrs)
  (make-jsworld-widget:img attrs src))

(define (js-text text . attrs)
  (make-jsworld-widget:text attrs text))

(define (js-node raw-node . attrs)
  (make-jsworld-widget:node attrs raw-node))
  

(provide (all-defined-out))