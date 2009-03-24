#lang scheme/base

(require lang/prim
         xml)

;; parse-xml: string -> xexpr
(define (parse-xml a-str)  
  (xml->xexpr (document-element
                 (read-xml (open-input-string
                            a-str)))))


(provide-primitive parse-xml)