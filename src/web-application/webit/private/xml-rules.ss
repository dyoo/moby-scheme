
(module xml-rules mzscheme
  
  (require "xml-core.ss"
           "pattern.ss")
  
  (require-for-syntax "rules-helper.ss")
  
  (provide xml-rules
           xml-case)
  
  (define-syntax (xml-case stx)
    (letrec ((rules-clauses 
              (lambda (node stx)
                (syntax-case stx ()
                  (() (syntax ((else (error "xml-rules: no match found")))))
                  (((pat output) . rest)
                   (with-syntax ((new-rest (rules-clauses node (syntax rest))))
                     #`(((match-xml-pat (xml-pat pat) #,node) => 
                         (lambda (bindings) (template-wrapper pat bindings output)))
                        . new-rest)))
                  (((pat guard output) . rest)
                   (with-syntax ((new-rest (rules-clauses node (syntax rest))))
                     #`(((let ((bindings (match-xml-pat (xml-pat pat) #,node)))
                           (if (and bindings
                                    (template-wrapper pat bindings guard))
                               bindings
                               #f))
                         => 
                         (lambda (bindings) (template-wrapper pat bindings output)))
                        . new-rest)))))))
      (syntax-case stx ()
        ((_ node clause ...)
         (with-syntax ((new-clauses (rules-clauses #'node (syntax (clause ...)))))
           (syntax (cond
                     . new-clauses)))))))
  
  (define-syntax (xml-rules stx)
    (syntax-case stx ()
      ((_ clause ...)
       (syntax (lambda (node)
                 (xml-case node clause ...))))))
  
  )
