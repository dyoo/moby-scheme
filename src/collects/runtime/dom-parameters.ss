#lang s-exp "private/restricted-runtime-scheme.ss"

;; dom-parameters allows the user to extend the dom-transformers by providing
;; custom functions.

(define-struct dom-parameters
  (scheme-value->dom? 
   scheme-value->dom
   error-struct->dom?
   error-struct->dom))



(provide/contract [struct dom-parameters ([scheme-value->dom? (any/c . -> . boolean?)]
                                          [scheme-value->dom (any/c ;; a value
                                                              (any/c . -> . any)  ;; a function to recur on subcomponents
                                                              . -> . any)]

                                          [error-struct->dom? (any/c . -> . boolean?)]
                                          [error-struct->dom (any/c ;; an error-struct
                                                              (any/c . -> . any) ;; a function to recur on subcomponents
                                                              . -> . any/c)])])