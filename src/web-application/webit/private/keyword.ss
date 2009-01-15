; a DSSSL-like keywords

(module keyword mzscheme
  
  (provide (struct dsssl-keyword (tag print-tag ns-uri))
           declare-keyword)
  
  (define-struct dsssl-keyword (tag print-tag ns-uri))
  
  (define-syntax (declare-keyword stx)
    (syntax-case stx ()
      [(declare-dsssl-keyword tag)
       (with-syntax ((kwd (datum->syntax-object (syntax tag)
                                                (string->symbol
                                                 (string-append
                                                  (symbol->string (syntax-object->datum (syntax tag)))
                                                  ":"))))
                     (kwd-syntax (datum->syntax-object (syntax tag)
                                                       (string->symbol
                                                        (string-append
                                                         (symbol->string (syntax-object->datum (syntax tag)))
                                                         ":attribute")))))
         (syntax
          (begin 
            (define-syntax kwd-syntax 'tag)
            (define-syntax (kwd stx)
              (syntax-case stx ()
                (name
                 (identifier? (syntax name))
                 (syntax (make-dsssl-keyword (quote tag)
                                             (quote tag)
                                             #f))))))))]
      [(declare-dsssl-keyword ptag ns-uri tag)
       (with-syntax ((kwd (datum->syntax-object (syntax ptag)
                                                (string->symbol
                                                 (string-append
                                                  (symbol->string (syntax-object->datum (syntax ptag)))
                                                  ":"))))
                     (kwd-syntax (datum->syntax-object (syntax ptag)
                                                       (string->symbol
                                                        (string-append
                                                         (symbol->string (syntax-object->datum (syntax ptag)))
                                                         ":attribute"))))
                     (ename (datum->syntax-object (syntax ptag)
                                                  (let ((ns (syntax-object->datum (syntax ns-uri)))
                                                        (lname (syntax-object->datum (syntax tag))))
                                                    (if ns
                                                        (string->symbol
                                                         (string-append
                                                          (symbol->string ns)
                                                          ":"
                                                          (symbol->string lname)))
                                                        lname)))))
         (syntax
          (begin
            (define-syntax kwd-syntax 'ename)
            (define-syntax (kwd stx)
              (syntax-case stx ()
                (name
                 (identifier? (syntax name))
                 (syntax (make-dsssl-keyword (quote ename)
                                             (quote tag)
                                             (quote ns-uri)))))))))]))
  
  )
