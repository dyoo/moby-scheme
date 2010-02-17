#lang s-exp "../../../private/restricted-runtime-scheme.ss"

;; dom-parameters allows the user to extend the dom-transformers by providing
;; custom functions.

(define-struct dom-parameters
  (scheme-value->dom? 
   scheme-value->dom))



(provide/contract [struct dom-parameters ([scheme-value->dom? (any/c . -> . boolean?)]
                                          [scheme-value->dom (any/c ;; a value
                                                              (any/c . -> . any)  ;; a function to recur on subcomponents
                                                              . -> . any)])])