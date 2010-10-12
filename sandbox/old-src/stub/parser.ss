#lang scheme/base


(require lang/prim
         scheme/list
         xml)

;; xml->s-exp: string -> xexpr
(define (xml->s-exp a-str)  
  (let ([result 
         (convert-cdata&normalize
          (xml->xexpr (document-element
                       (read-xml (open-input-string
                                  a-str)))))])
    result))

(define (convert-cdata&normalize xexpr)
  (cond
    [(cdata? xexpr)
     (cdata-string xexpr)]
    [(pair? xexpr)
     (cond
       [(attrib-list? (second xexpr))
        (cons (first xexpr)
              (cons (cons '@ (second xexpr))
                    (map convert-cdata&normalize (rest (rest xexpr)))))]
       [else
        (cons (first xexpr)
              (cons (cons '@ empty)
                    (map convert-cdata&normalize (rest xexpr))))])]
    [else
     xexpr]))


(define (attrib-list? thing)
  (and (list? thing)
       (andmap (lambda (x)
                 (and (list x)
                      (= (length x) 2)
                      (symbol? (first x))
                      (string? (second x))))
               thing)))


;; split-whitespace: string -> (listof string)
(define (split-whitespace a-str)
  (filter (lambda (x)
            (> (string-length x) 0))
          (regexp-split #rx"[ \n\t]+" a-str)))


(provide-primitives xml->s-exp
                    #;split-whitespace)