(module quote mzscheme
  
  (require "xml-core.ss"
           (lib "list.ss"))
  
  (require-for-syntax "xml-helper.ss"
                      (lib "stx.ss" "syntax"))
  
  (provide define-namespace
           quote-xml
           quasi-xml
           strict-webit-xml)
  
  (define-syntax (strict-webit-xml stx)
    (syntax-case stx ()
      ((_)
       #`(quote #,(strict-type-checking)))
      ((_ #t)
       #'(define-syntaxes () (begin (strict-type-checking! #t)
                                    (values))))
      ((_ #t)
       #'(define-syntaxes () (begin (strict-type-checking! #f)
                                    (values))))))
  
  (define-syntax (define-namespace stx)
    (syntax-case stx ()
      ((_ prefix namespace)
       #`(define-syntax #,(datum->syntax-object
                           (syntax prefix)
                           (string->symbol
                            (string-append
                             (symbol->string 
                              (syntax-object->datum (syntax prefix)))
                             "-nspre")))
           (string->symbol namespace)))))
  
  (define-syntax (quote-xml stx)
    (define (construct-attribute full-tag val)
      #`(make-xml-attribute '#,full-tag #,val))
    (define (get-attributes lst-stx acc)
      (syntax-case lst-stx ()
        (() (values (reverse acc) '()))
        ((key val)
         (keyword-identifier? (syntax-object->datum #'key))
         (begin (unless (string? (syntax-object->datum #'val))
                  (raise-syntax-error #f
                                      "attribute value must be a string"
                                      stx
                                      #'val))
                (values (reverse (cons (construct-attribute (attribute->tag #'key) #'val) acc))
                        '())))
        ((key val item ...)
         (keyword-identifier? (syntax-object->datum #'key))
         (begin (unless (string? (syntax-object->datum #'val))
                  (raise-syntax-error #f
                                      "attribute value must be a string"
                                      stx
                                      #'val))
                (get-attributes #'(item ...)
                                (cons (construct-attribute (attribute->tag #'key) #'val)
                                      acc))))
        ((item ...)
         (values (reverse acc) #'(item ...)))))
    (define (get-contents lst-stx)
      (syntax-case lst-stx ()
        (() '())
        ((node)
         (syntax->list #'node)
         (list (translate-xml #'node)))
        ((node item ...)
         (syntax->list #'node)
         (cons (translate-xml #'node) (get-contents #'(item ...))))
        ((item1)
         (begin (unless (string? (syntax-e #'item1))
                  (raise-syntax-error #f
                                      "content item must be a string or element"
                                      stx
                                      #'item1))
                (list #'item1)))
        ((item1 item ...)
         (begin (unless (string? (syntax-e #'item1))
                  (raise-syntax-error #f
                                      "content item must be a string or element"
                                      stx
                                      #'item1))
                (cons #'item1 (get-contents #'(item ...)))))))
    (define (translate-xml top-stx)
      (syntax-case top-stx ()
        ((tag)
         (begin (unless (identifier? #'tag)
                  (raise-syntax-error #f
                                      "element tag must be an identifier"
                                      stx
                                      #'tag))
                (let ((full-tag (type->tag #'tag)))
                  #`(make-xml-element '#,full-tag '() '()))))
        ((tag item ...)
         (begin (unless (identifier? #'tag)
                  (raise-syntax-error #f
                                      "element tag must be an identifier"
                                      stx
                                      #'tag))
                (let*-values (((attrs rest) (get-attributes #'(item ...) '()))
                              ((contents) (get-contents rest))
                              ((full-tag) (type->tag #'tag)))
                  #`(make-xml-element '#,full-tag
                                  (list #,@attrs) (list #,@contents)))))
        (other
         (raise-syntax-error #f
                             "expected an element syntax"
                             stx
                             #'other))))
    (syntax-case stx ()
      ((_ name)
       (identifier? (syntax name))
       #`(quote #,(type->tag (syntax name))))
      ((_ it)
       (translate-xml (syntax it)))))
  
  (define (attrval-mustbe-string name val)
    (if (string? val)
        val
        (error (format "quasi-xml: value for ~a not a string, given ~s" name val))))
  
  (define (content-mustbe-node name val)
    (cond
      ((xml-element? val) val)
      ((string? val) val)
      (else 
       (error 
        (format "quasi-xml: unquoted expression must be an element, pcdata, or a string; in element ~a given ~s"
                name val)))))
  
  (define (content-mustbe-list name val-lst)
    (if (list? val-lst)
        (map (lambda (val)
               (cond
                 ((xml-element? val) val)
                 ((string? val) val)
                 (else 
                  (error 
                   (format "quasi-xml: node must be an element, pcdata, or a string; in element ~a given ~s"
                           name val)))))
             val-lst)
        (error 
         (format "quasi-xml: unquote-splice expression must evaluate to a list; in element ~a given ~s"
                 name val-lst))))
  
  (define-syntax (quasi-xml stx)
    (define (get-attrval tag-stx val-stx)
      (syntax-case* val-stx (unquote unquote-splicing)
        module-or-top-identifier=?
        ((unquote x) #`(attrval-mustbe-string '#,tag-stx x))
        (val
         (begin (unless (string? (syntax-object->datum #'val))
                  (raise-syntax-error #f
                                      "attribute value must be a string"
                                      stx
                                      #'val))
                #'val))))
    (define (construct-attribute full-tag val)
      #`(make-xml-attribute '#,full-tag #,val))
    (define (get-attributes lst-stx acc)
      (syntax-case lst-stx ()
        (() (values (reverse acc) '()))
        ((key val)
         (keyword-identifier? (syntax-object->datum #'key))
         (begin (values (reverse (cons (construct-attribute (attribute->tag #'key)
                                                            (get-attrval #'key #'val))
                                       acc))
                        '())))
        ((key val item ...)
         (keyword-identifier? (syntax-object->datum #'key))
         (begin (get-attributes #'(item ...)
                                (cons (construct-attribute (attribute->tag #'key)
                                                           (get-attrval #'key #'val))
                                      acc))))
        ((item ...)
         (values (reverse acc) #'(item ...)))))
    (define (get-contents tag-stx lst-stx)
      (syntax-case* lst-stx (unquote unquote-splicing)
        module-or-top-identifier=?
        (() '())
        (((unquote x))
         #`(list (content-mustbe-node '#,tag-stx x)))
        (((unquote x) item ...)
         #`(cons (content-mustbe-node '#,tag-stx x) #,(get-contents tag-stx #'(item ...))))
        (((unquote-splicing x))
         #`(content-mustbe-list '#,tag-stx x))
        (((unquote-splicing x) item ...)
         #`(append (content-mustbe-list '#,tag-stx x) #,(get-contents tag-stx #'(item ...))))
        ((node)
         (syntax->list #'node)
         #`(list #,(translate-xml #'node)))
        ((node item ...)
         (syntax->list #'node)
         #`(cons #,(translate-xml #'node) #,(get-contents tag-stx #'(item ...))))
        ((item1)
         (begin (unless (string? (syntax-e #'item1))
                  (raise-syntax-error #f
                                      "content item must be a string or element"
                                      stx
                                      #'item1))
                #'(list item1)))
        ((item1 item ...)
         (begin (unless (string? (syntax-e #'item1))
                  (raise-syntax-error #f
                                      "content item must be a string or element"
                                      stx
                                      #'item1))
                #`(cons item1 #,(get-contents tag-stx #'(item ...)))))))
    (define (translate-xml top-stx)
      (syntax-case top-stx ()
        ((tag)
         (begin (unless (identifier? #'tag)
                  (raise-syntax-error #f
                                      "element tag must be an identifier"
                                      stx
                                      #'tag))
                (let ((full-tag (type->tag #'tag)))
                  #`(make-xml-element '#,full-tag '() '()))))
        ((tag item ...)
         (begin (unless (identifier? #'tag)
                  (raise-syntax-error #f
                                      "element tag must be an identifier"
                                      stx
                                      #'tag))
                (let*-values (((attrs rest) (get-attributes #'(item ...) '()))
                              ((contents) (get-contents #'tag rest))
                              ((full-tag) (type->tag #'tag)))
                  #`(make-xml-element '#,full-tag
                                      (list #,@attrs)
                                      #,contents))))
        (other
         (raise-syntax-error #f
                             "expected an element syntax"
                             stx
                             #'other))))
    (syntax-case stx ()
      ((_ name)
       (identifier? (syntax name))
       #`(quote #,(type->tag (syntax name))))
      ((_ it)
       (translate-xml #'it))))
  
  )
