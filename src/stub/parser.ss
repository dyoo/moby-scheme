#lang scheme/base

(require lang/prim
         xml)

;; parse-xml: string -> xexpr
(define (parse-xml a-str)  
  (xml->xexpr (document-element
                 (read-xml (open-input-string
                            a-str)))))

;; split-whitespace: string -> (listof string)
(define (split-whitespace a-str)
  (filter (lambda (x)
            (> (string-length x) 0))
          (regexp-split #rx"[ \n\t]+" a-str)))


(provide-primitives parse-xml
                    split-whitespace)