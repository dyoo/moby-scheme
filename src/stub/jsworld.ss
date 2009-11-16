#lang scheme/base
(require scheme/contract)


(define-struct jsworld-widget (attrs) #:prefab)
(define-struct (jsworld-widget:div jsworld-widget) () #:prefab)
(define-struct (jsworld-widget:p jsworld-widget) () #:prefab)
(define-struct (jsworld-widget:button jsworld-widget) (f ef) #:prefab)
(define-struct (jsworld-widget:input jsworld-widget) (type update-f) #:prefab)
#;(define-struct (jsworld-widget:bidirectional-input jsworld-widget) (type val-f update-f) #:prefab)
(define-struct (jsworld-widget:img jsworld-widget) (src) #:prefab)
(define-struct (jsworld-widget:text jsworld-widget) (text) #:prefab)
(define-struct (jsworld-widget:node jsworld-widget) (node) #:prefab)
(define-struct (jsworld-widget:select jsworld-widget) (optiosn update-f) #:prefab)


(define (js-div (attrs '()))
  (make-jsworld-widget:div attrs))

(define (js-p (attrs '()))
  (make-jsworld-widget:p attrs))

(define (js-button f (attrs '()))
  (make-jsworld-widget:button attrs f (lambda (w) '())))

(define (js-button! f ef (attrs '()))
  (make-jsworld-widget:button attrs f ef))

(define (js-input type update-f (attrs '()))
  (make-jsworld-widget:input attrs update-f type))

#;(define (js-bidirectional-input type val-f update-f (attrs '()))
  (make-jsworld-widget:bidirectional-input attrs type val-f update-f))

(define (js-img src (attrs '()))
  (make-jsworld-widget:img attrs src))

(define (js-text text (attrs '()))
  (make-jsworld-widget:text attrs text))

(define (js-node raw-node (attrs '()))
  (make-jsworld-widget:node attrs raw-node))

(define (js-select options update-f (attrs '()))
  (make-jsworld-widget:select attrs options update-f))

#;(define (get-input-value an-input-node)
  "")


(define attrs/c (listof (list/c string? string?)))

(provide/contract [js-p (() (attrs/c) . ->* . jsworld-widget?)]
                  [js-div (() (attrs/c) . ->* . jsworld-widget?)]
                  [js-button (((any/c . -> . any/c)) 
                              (attrs/c) 
                              . ->* . jsworld-widget?)]
                  [js-button! (((any/c . -> . any/c) 
                                (any/c . -> . any/c))
                               (attrs/c) 
                               . ->* .
                               jsworld-widget?)]
                  [js-input ((string? (any/c  string? . -> . any/c))
                             (attrs/c) . ->* . jsworld-widget?)]
                  #;[js-bidirectional-input ((string? 
                                            (any/c . -> . any/c) 
                                            (any/c . -> . any/c))
                                           (attrs/c) . ->* . jsworld-widget?)]
                  [js-img ((string?) (attrs/c) . ->* . jsworld-widget?)]
                  [js-text (string? . -> . jsworld-widget?)]
                  [js-node ((any/c) (attrs/c) . ->* . jsworld-widget?)]
                  [js-select (((listof string?) (any/c string? . -> . any/c)) (attrs/c) . ->* . jsworld-widget?)]
                  #;[get-input-value ((or/c jsworld-widget:input? 
                                          jsworld-widget:bidirectional-input? 
                                          string?)
                                    . -> . string?)]
                  )