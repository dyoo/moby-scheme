#lang s-exp "../../../private/restricted-runtime-scheme.ss"

;; dom-string-content: dom -> string
;; Gets the string content of the dom.
(define (dom-string-content a-dom)
  (cond
    [(string? a-dom)
     a-dom]
    [else
     (foldl (lambda (a-dom rest)
              (cond
                [(and (list? a-dom)
                      (> (length a-dom) 2)
                      (list? (second a-dom))
                      (ormap (lambda (attrib-pair) 
                               (and (symbol? (first attrib-pair))
                                    (symbol=? (first attrib-pair) 'style)
                                    (string? (second attrib-pair))
                                    (string=? (second attrib-pair) "display:none")))
                             (second a-dom)))
                 rest]
                [else
                 (string-append rest (dom-string-content a-dom))]))
            ""
            (rest (rest a-dom)))]))


(provide/contract [dom-string-content (any/c . -> . string?)])