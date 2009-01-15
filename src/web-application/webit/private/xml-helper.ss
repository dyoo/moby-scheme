(module xml-helper mzscheme
  
  (provide (all-defined))
  
  (define strict-typing-state #f)
  (define (strict-type-checking)
    strict-typing-state)
  (define (strict-type-checking! flag)
    (set! strict-typing-state flag))
  
  ; Return the index of the last occurence of a-char in str, or #f
  (define (string-rindex str a-char)
    (let loop ((pos (- (string-length str) 1)))
      (cond
        ((negative? pos) #f) 	; whole string has been searched, in vain
        ((char=? a-char (string-ref str pos)) pos)
        (else (loop (- pos 1))))))
  
  ; splits a tag prefix:short into (prefix . short)
  ; a tag with no colon is returned unchanged
  (define (split-tag tag)
    (let ((nm (symbol->string tag)))
      (cond 
        ((string-rindex nm #\:)
         => (lambda (pos) 
              (values (string->symbol (substring nm 0 pos))
                      (string->symbol (substring nm (+ pos 1) (string-length nm))))))
        (else (values #f tag)))))
  
  (define (split-string-tag nm)
    (cond 
      ((string-rindex nm #\:)
       => (lambda (pos) 
            (values (substring nm 0 pos)
                    (substring nm (+ pos 1) (string-length nm)))))
      (else (values #f nm))))
  
  ; prefix->namespace :: symbol syntax -> symbol
  (define (prefix->namespace pre tag)
    (syntax-local-value (datum->syntax-object
                         tag
                         (string->symbol
                          (string-append
                           (symbol->string pre)
                           "-nspre")))
                        (lambda () (error "xml: namespace prefix not defined" pre))))
  
  (define (tag->ename tag)
    (let-values (((pre name) (split-tag (syntax-object->datum tag))))
      (if pre
          (datum->syntax-object tag
                                (string->symbol
                                 (string-append
                                  (symbol->string (prefix->namespace pre tag))
                                  ":"
                                  (symbol->string name))))
          tag)))
  
  (define (type->tag tag)
    (if (strict-type-checking)
        (syntax-local-value (datum->syntax-object
                             tag
                             (string->symbol
                              (string-append
                               (symbol->string (syntax-object->datum tag))
                               "-type")))
                            (lambda ()
                              (raise-syntax-error #f
                                                  "element type not defined"
                                                  tag)))
        (syntax-local-value (datum->syntax-object
                             tag
                             (string->symbol
                              (string-append
                               (symbol->string (syntax-object->datum tag))
                               "-type")))
                            (lambda () (tag->ename tag)))))
  
  (define (attribute->tag tag)
    (let ((nm (symbol->string (syntax-object->datum tag))))
      (if (strict-type-checking)
          (syntax-local-value (datum->syntax-object
                               tag
                               (string->symbol
                                (string-append nm "attribute")))
                              (lambda ()
                                (raise-syntax-error #f
                                                    "attribute type not defined"
                                                    tag)))
          (syntax-local-value (datum->syntax-object
                               tag
                               (string->symbol
                                (string-append nm "attribute")))
                              (lambda () 
                                (tag->ename 
                                 (datum->syntax-object 
                                  tag
                                  (string->symbol
                                   (substring nm 0 (- (string-length nm) 1))))))))))
  
  (define keyword-identifier? (lambda (s)
                                (and (symbol? s)
                                     (char=? #\:
                                             (let ((st (symbol->string s)))
                                               (string-ref st (- (string-length st) 1)))))))
  
  )
