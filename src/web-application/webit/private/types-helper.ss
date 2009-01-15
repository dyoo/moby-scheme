; types-helper.ss

(module types-helper mzscheme
  
  (provide (all-defined))
  
  ; stripped down version of SRFI-13 implementation
  ; SHOULD really use the Schematics port of SRFI-13!
  (define (string-index-right str criterion)
    (let ((end (string-length str)))
      (let lp ((i (- end 1)))
        (and (>= i 0)
             (if (char=? criterion (string-ref str i)) 
                 i
                 (lp (- i 1)))))))
  
  (define (prefix-name sym)
    (let ((nm (symbol->string sym)))
      (cond 
        ((string-index-right nm #\:) 
         => (lambda (pos) 
              (string->symbol 
               (substring nm 0 pos))))
        (else #f))))
  
  (define (local-name sym)
    (let ((nm (symbol->string sym)))
      (cond 
        ((string-index-right nm #\:) 
         => (lambda (pos) 
              (string->symbol 
               (substring nm (+ pos 1) (string-length nm)))))
        (else sym))))
  
  (define (ele-names fun-tag ns-uri stx)
    (let ((local (local-name fun-tag))
          (pre (prefix-name fun-tag)))
      (values (datum->syntax-object stx
                                    (if ns-uri
                                        (string->symbol (string-append (symbol->string ns-uri) ":"
                                                                       (symbol->string local)))
                                        local))
              (datum->syntax-object stx local)
              (datum->syntax-object stx
                                    (string->symbol (string-append (symbol->string fun-tag) "?"))))))
  
  (define (attr-names fun-tag ns-uri stx)
    (let ((local (local-name fun-tag))
          (pre (prefix-name fun-tag)))
      (values (datum->syntax-object stx
                                    (if ns-uri
                                        (string->symbol (string-append (symbol->string ns-uri) ":"
                                                                       (symbol->string local)))
                                        local))
              (datum->syntax-object stx local)
              (datum->syntax-object stx
                                    (string->symbol (string-append (symbol->string fun-tag) ":")))
              (datum->syntax-object stx
                                    (if pre
                                        (string->symbol (string-append (symbol->string pre)
                                                                       ":&"
                                                                       (symbol->string local)))
                                        (string->symbol (string-append "&"
                                                                       (symbol->string local)))))
              (datum->syntax-object stx
                                    (if pre
                                        (string->symbol (string-append (symbol->string pre)
                                                                       ":&"
                                                                       (symbol->string local)
                                                                       "?"))
                                        (string->symbol (string-append "&"
                                                                       (symbol->string local)
                                                                       "?"))))
              (datum->syntax-object stx
                                    (if pre
                                        (string->symbol (string-append (symbol->string pre)
                                                                       ":&"
                                                                       (symbol->string local)
                                                                       "-attr?"))
                                        (string->symbol (string-append "&"
                                                                       (symbol->string local)
                                                                       "-attr?")))))))
  
  )
