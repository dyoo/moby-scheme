; "Excess" stylesheet system, providing 'micro' and 'macro' XML transformers

; more here.
;   Add a *document* expander
;   Get rid of the rest of EPS in the implementation of Excess

#cs
(module excess mzscheme
  
  (require-for-syntax "valid-types-helper.ss")
  
  (require "xml-core.ss"
           (lib "unit.ss"))
  
  (provide stylesheet->expander
           stylesheet
           compound-stylesheet)
  
  (define (install-expander dictionary ele-name expander)
    (hash-table-put! dictionary 
                     ele-name
                     expander)
    (void))
  
  (define (lookup-expander dictionary ele-name)
    (hash-table-get dictionary ele-name (lambda () #f)))
  
  (define (make-expander dictionary)
    (lambda (n e . rest)
      (cond
        ((xml-element? n)
         (let* ((name (xml-element-tag n))
                (expander (or (lookup-expander dictionary name)
                              (lookup-expander dictionary '*element*))))
           (if expander
               (apply expander n e rest)
               (raw-xml-element-function-back-end (xml-element-tag n)
                                                  (xml-element-print-tag n)
                                                  (xml-element-target-ns n)
                                                  (xml-element-ns-list n)
                                                  (xml-element-attributes n)
                                                  (map (lambda (item)
                                                         (apply e item e rest))
                                                       (xml-element-contents n))))))
        ((string? n)
         (let* ((expander (lookup-expander dictionary '*text*)))
           (if expander
               (apply expander n e rest)
               n)))
        ((xml-comment? n)
         (let* ((expander (lookup-expander dictionary '*comment*)))
           (if expander
               (apply expander n e rest)
               n)))
        (else 
         (let* ((expander (lookup-expander dictionary '*data*)))
           (if expander
               (apply expander n e rest)
               (if (and (nodeset? n) (pair? n))
                   (cons (apply e (car n) e rest)
                         (apply e (cdr n) e rest))
                   n)))))))
  
  (define (stylesheet->expander stylesheet-unit)
    (let* ((dictionary (make-hash-table))
           (e (make-expander (invoke-unit stylesheet-unit dictionary))))
      (lambda (n . rest)
        (apply e n e rest))))
  
  (define-syntax (define-xml-macro stx)
    (syntax-case stx ()
      [(_ dictionary tag-exp fun)
       (identifier? (syntax tag-exp))
       (with-syntax ([etag (type-exists/ext? (syntax tag-exp))]
                     [xexp (datum->syntax-object #'fun 'xml-expand)])
         (syntax (install-expander dictionary
                                   'etag
                                   (lambda (n e . rest)
                                     (let ((xexp (lambda (i . r) (apply e i e r))))
                                       (apply e (fun n) e rest))))))]))
  
  (define-syntax (define-xml-micro stx)
    (syntax-case stx ()
      [(_ dictionary tag-exp fun)
       (identifier? (syntax tag-exp))
       (with-syntax ([etag (type-exists/ext? (syntax tag-exp))]
                     [xexp (datum->syntax-object #'fun 'xml-expand)])
         (syntax (install-expander dictionary
                                   'etag
                                   (lambda (n e . rest)
                                     (let ((xexp (lambda (i . r) (apply e i e r))))
                                       (apply fun n rest))))))]))
  
  (define-syntax wrap-ss-exp
    (syntax-rules (xml-micro xml-macro)
      [(_ dictionary (xml-micro . rst))
       (define-xml-micro dictionary . rst)]
      [(_ dictionary (xml-macro . rst))
       (define-xml-macro dictionary . rst)]
      [(_ dictionary other)
       other]))
  
  (define-syntax stylesheet
    (syntax-rules ()
      [(stylesheet exp ...)
       (unit
         (import dictionary) (export)
         (wrap-ss-exp dictionary exp) ...
         dictionary)]))
  
  (define-syntax (compound-stylesheet stx)
    (syntax-case stx ()
      [(_ ss ...)
       (with-syntax ([(tag ...) (generate-temporaries (syntax (ss ...)))])
         (syntax (compound-unit
                   (import dictionary)
                   (link (tag (ss dictionary)) ...)
                   (export))))]))
  
  )
