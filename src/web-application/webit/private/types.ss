; types.ss

(module types mzscheme
  
  (provide define-element
           define-attribute)
  
  (require "xml-core.ss"
           "pattern.ss")
  
  (require-for-syntax "types-helper.ss")
  
  (define-syntax (define-attribute stx)
    (syntax-case stx ()
      ((define-attribute tag)
       (identifier? (syntax tag))
       (let ((ns-uri #f))
         (let-values ([(match-tag print-tag attr-keyword attr-fun attr-pred-name sec-attr-pred-name) 
                       (attr-names (syntax-object->datum (syntax tag))
                                   ns-uri
                                   stx)])
           (with-syntax ((ename match-tag)
                         (lname print-tag)
                         (akey attr-keyword)
                         (acons attr-fun)
                         (atest? attr-pred-name)
                         (atest2? sec-attr-pred-name)
                         (ns ns-uri))
             (syntax
              (begin
                (declare-keyword tag
                                 ns
                                 lname)
                (define atest?
                  (let ((pred (lambda (node)
                                (cond
                                  ((xml-element? node)
                                   (let ((found? (match-xml-attribute (quote ename)
                                                                      (xml-element-attributes node))))
                                     (if found?
                                         (xml-attribute-value found?)
                                         #f)))
                                  (else #f)))))
                    pred))))))))
      ((define-attribute (tag ns-uri))
       (and (identifier? (syntax tag)) 
            (or (identifier? (syntax ns-uri))
                (eq? (syntax-object->datum (syntax ns-uri)) #f)))
       (let-values ([(match-tag print-tag attr-keyword attr-fun attr-pred-name sec-attr-pred-name) 
                     (attr-names (syntax-object->datum (syntax tag))
                                 (syntax-object->datum (syntax ns-uri))
                                 stx)])
         (with-syntax ((ename match-tag)
                       (lname print-tag)
                       (akey attr-keyword)
                       (acons attr-fun)
                       (atest? attr-pred-name)
                       (atest2? sec-attr-pred-name))
           (syntax
            (begin
              (declare-keyword tag
                               ns-uri
                               lname)
              (define atest?
                (let ((pred (lambda (node)
                              (cond
                                ((xml-element? node)
                                 (let ((found? (match-xml-attribute (quote ename)
                                                                    (xml-element-attributes node))))
                                   (if found?
                                       (xml-attribute-value found?)
                                       #f)))
                                (else #f)))))
                  pred)))))))))
  
  (define-syntax (define-element stx)
    (syntax-case stx (validate)
      [(define-element tag)
       (identifier? (syntax tag))
       (let ((ns-uri #f))
         (let-values ([(match-tag print-tag ele-pred-name) (ele-names (syntax-object->datum (syntax tag))
                                                                      ns-uri
                                                                      stx)])
           (with-syntax ((ename match-tag)
                         (lname print-tag)
                         (etest? ele-pred-name)
                         (ns ns-uri)
                         (type-syntax-name
                          (datum->syntax-object (syntax tag)
                                                (string->symbol 
                                                 (string-append 
                                                  (symbol->string 
                                                   (syntax-object->datum (syntax tag)))
                                                  "-type")))))
             (syntax
              (begin (define-syntax type-syntax-name (quote ename))
                     (define tag
                       (let ((cntr (lambda items
                                     (xml-element-function-back-end (quote ename) items))))
                         cntr))
                     (define etest?
                       (let ((pred (lambda (node)
                                     (and (xml-element? node)
                                          (eq? (xml-element-tag node) 
                                               (quote ename))))))
                         pred)))))))]
      [(define-element (tag ns-uri))
       (and (identifier? (syntax tag)) 
            (or (identifier? (syntax ns-uri))
                (eq? (syntax-object->datum (syntax ns-uri)) #f)))
       (let-values ([(match-tag print-tag ele-pred-name) (ele-names (syntax-object->datum (syntax tag))
                                                                    (syntax-object->datum (syntax ns-uri))
                                                                    stx)])
         (with-syntax ((ename match-tag)
                       (lname print-tag)
                       (etest? ele-pred-name)
                       (type-syntax-name
                        (datum->syntax-object (syntax tag)
                                              (string->symbol 
                                               (string-append 
                                                (symbol->string 
                                                 (syntax-object->datum (syntax tag)))
                                                "-type")))))
           (syntax
            (begin (define-syntax type-syntax-name (quote ename))
                   (define tag
                     (let ((cntr (lambda items
                                   (xml-element-function-back-end (quote ename) items))))
                       cntr))
                   (define etest?
                     (let ((pred (lambda (node)
                                   (and (xml-element? node)
                                        (eq? (xml-element-tag node) 
                                             (quote ename))))))
                       pred))))))]
      [(define-element tag (validate pat ...))
       (identifier? (syntax tag))
       (let ((ns-uri #f))
         (let-values ([(match-tag print-tag ele-pred-name) (ele-names (syntax-object->datum (syntax tag))
                                                                      ns-uri
                                                                      stx)])
           (with-syntax ((ename match-tag)
                         (lname print-tag)
                         (etest? ele-pred-name)
                         (ns ns-uri)
                         (type-syntax-name
                          (datum->syntax-object (syntax tag)
                                                (string->symbol 
                                                 (string-append 
                                                  (symbol->string 
                                                   (syntax-object->datum (syntax tag)))
                                                  "-type")))))
             (syntax
              (begin (define-syntax type-syntax-name (quote ename))
                     (define tag
                       (let ((cntr (lambda items
                                     ((lambda (node)
                                        (if (or (match-xml-pat (xml-pat pat) node) ...)
                                            node
                                            (error (format "~a: no validation rule matched ~s~n"
                                                           'tag
                                                           node))))
                                      (xml-element-function-back-end (quote ename) items)))))
                         cntr))
                     (define etest?
                       (let ((pred (lambda (node)
                                     (and (xml-element? node)
                                          (eq? (xml-element-tag node) 
                                               (quote ename))))))
                         pred)))))))]
      [(define-element (tag ns-uri) (validate pat ...))
       (and (identifier? (syntax tag)) 
            (or (identifier? (syntax ns-uri))
                (eq? (syntax-object->datum (syntax ns-uri)) #f)))
       (let-values ([(match-tag print-tag ele-pred-name) (ele-names (syntax-object->datum (syntax tag))
                                                                    (syntax-object->datum (syntax ns-uri))
                                                                    stx)])
         (with-syntax ((ename match-tag)
                       (lname print-tag)
                       (etest? ele-pred-name)
                       (type-syntax-name
                        (datum->syntax-object (syntax tag)
                                              (string->symbol 
                                               (string-append 
                                                (symbol->string 
                                                 (syntax-object->datum (syntax tag)))
                                                "-type")))))
           (syntax
            (begin (define-syntax type-syntax-name (quote ename))
                   (define tag
                     (let ((cntr (lambda items
                                   ((lambda (node)
                                      (if (or (match-xml-pat (xml-pat pat) node) ...)
                                          node
                                          (error (format "~a: no validation rule matched ~s~n"
                                                         'tag
                                                         node))))
                                    (xml-element-function-back-end (quote ename) items)))))
                       cntr))
                   (define etest?
                     (let ((pred (lambda (node)
                                   (and (xml-element? node)
                                        (eq? (xml-element-tag node) 
                                             (quote ename))))))
                       pred))))))]
      ))
  
  )

