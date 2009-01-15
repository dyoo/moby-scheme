;; css.ss -- definitions to support CSS in a style more idiomatic to Scheme

#cs
(module css mzscheme
  
  (require "xml.ss"
           "html.ss")
  
  (require-for-syntax (lib "stx.ss" "syntax"))
  
  (define-syntax (css-attr stx)
    (syntax-case stx ()
      ((_ (name1 val1))
       (syntax (string-append (symbol->string 'name1)
                              ":"
                              val1
                              ";")))
      ((_ (name1 val1) (name val) ...)
       (syntax (string-append (string-append (symbol->string 'name1)
                                             ":"
                                             val1
                                             ";")
                              (string-append " "
                                             (symbol->string 'name)
                                             ":"
                                             val
                                             ";")
                              ...)))))
  
  ;; create an inline stylesheet in an html/xhtml document
  (define-syntax (css/html stx)
    (syntax-case stx ()
      ((_ . rest)
       (syntax (h4:style h4:type: "text/css" (css . rest))))))
  
  (define (create-css decls)
    (make-css-node decls))
  
  (define (css-link url)
    (h4:link h4:rel: "stylesheet"
             h4:href: url
             h4:type: "text/css"))
  
  (define (css-link/xml url)
    (make-xml-pi "xml-stylesheet"
                 (list (format "type=\"text/css\" href=~a media=\"screen\""
                               url))))
  
  ;; create a CSS1 stylesheet
  (define-syntax (css stx)
    (syntax-case stx ()
      ((_ . lst)
       (letrec ((parse-avp (lambda (avp)
                             (syntax-case* avp (!)
                               module-or-top-identifier=?
                               ((! (name val))
                                #'(make-em-avp 'name val))
                               ((name val)
                                #'(make-avp 'name val)))))
                (parse-avp-list (lambda (lst)
                                  (syntax-case* lst ()
                                    module-or-top-identifier=?
                                    ((fst . rst)
                                     #`(cons #,(parse-avp #'fst)
                                             #,(parse-avp-list #'rst)))
                                    (()
                                     #'()))))
                (parse-symbol-selector (lambda (sel)
                                         (syntax-case sel ()
                                           (i #'(quote i)))))
                (parse-simple-selector (lambda (sel-item)
                                         (syntax-case* sel-item (// pclass id)
                                           module-or-top-identifier=?
                                           (sel
                                            (identifier? #'sel)
                                            (parse-symbol-selector #'sel))
                                           ((sel)
                                            #`(list #,(parse-simple-selector #'sel)))
                                           ((id sel)
                                            #`(quote #,(string->symbol 
                                                        (string-append "#"
                                                                       (symbol->string (syntax-e #'sel))))))
                                           ((id typ sel)
                                            #`(quote #,(string->symbol 
                                                        (string-append (symbol->string (syntax-e #'typ))
                                                                       "#"
                                                                       (symbol->string (syntax-e #'sel))))))
                                           ((classx sel)
                                            (eq? (syntax-object->datum #'classx) 'class)
                                            #`(quote #,(string->symbol 
                                                        (string-append "."
                                                                       (symbol->string (syntax-e #'sel))))))
                                           ((classx typ sel)
                                            (eq? (syntax-object->datum #'classx) 'class)
                                            #`(quote #,(string->symbol 
                                                        (string-append (symbol->string (syntax-e #'typ))
                                                                       "."
                                                                       (symbol->string (syntax-e #'sel))))))
                                           ((pclass sel)
                                            #`(quote #,(string->symbol 
                                                        (string-append ":"
                                                                       (symbol->string (syntax-e #'sel))))))
                                           ((pclass typ sel)
                                            #`(quote #,(string->symbol 
                                                        (string-append (symbol->string (syntax-e #'typ))
                                                                       ":"
                                                                       (symbol->string 
                                                                        (syntax-e #'sel))))))
                                           ((pclass (classx csel) sel)
                                            (eq? (syntax-object->datum #'classx) 'class)
                                            #`(quote #,(string->symbol 
                                                        (string-append "."
                                                                       (symbol->string 
                                                                        (syntax-e #'csel))
                                                                       ":"
                                                                       (symbol->string 
                                                                        (syntax-e #'sel))))))
                                           ((pclass (classx typ csel) sel)
                                            (eq? (syntax-object->datum #'classx) 'class)
                                            #`(quote #,(string->symbol 
                                                        (string-append (symbol->string 
                                                                        (syntax-e #'typ))
                                                                       "."
                                                                       (symbol->string 
                                                                        (syntax-e #'csel))
                                                                       ":"
                                                                       (symbol->string (syntax-e #'sel))))))
                                           ((sel1 sel ...)
                                            #`(cons #,(parse-symbol-selector #'sel1)
                                                    #,(parse-selector #'(sel ...)))))))
                (parse-steps (lambda (lst)
                               (syntax-case lst ()
                                 (() #'())
                                 ((sel) #`(list #,(parse-simple-selector #'sel)))
                                 ((sel1 . rst) #`(cons #,(parse-simple-selector #'sel1)
                                                       #,(parse-steps #'rst))))))
                (parse-path-selector (lambda (sel-item)
                                       (syntax-case* sel-item (//)
                                         module-or-top-identifier=?
                                         ((// i ...)
                                          #`(make-path-selector #,(parse-steps #'(i ...))))
                                         (a (parse-simple-selector #'a)))))
                (parse-selector (lambda (sel-item)
                                  (syntax-case* sel-item (// pclass id)
                                    module-or-top-identifier=?
                                    (sel
                                     (identifier? #'sel)
                                     (parse-symbol-selector #'sel))
                                    ((// sel ...)
                                     (parse-path-selector #'(// sel ...)))
                                    ((sel)
                                     #`(list #,(parse-path-selector #'sel)))
                                    ((id sel)
                                     (parse-simple-selector #'(id sel)))
                                    ((id typ sel)
                                     (parse-simple-selector #'(id typ sel)))
                                    ((classx sel)
                                     (eq? (syntax-object->datum #'classx) 'class)
                                     (parse-simple-selector #'(classx sel)))
                                    ((classx typ sel)
                                     (eq? (syntax-e #'class) 'class)
                                     (parse-simple-selector #'(classx typ sel)))
                                    ((pclass sel)
                                     (parse-simple-selector #'(pclass sel)))
                                    ((pclass (classx csel) sel)
                                     (eq? (syntax-object->datum #'classx) 'class)
                                     (parse-simple-selector #'(pclass (classx csel) sel)))
                                    ((pclass (classx typ csel) sel)
                                     (eq? (syntax-object->datum #'classx) 'class)
                                     (parse-simple-selector #'(pclass (classx typ csel) sel)))
                                    ((pclass typ sel)
                                     (parse-simple-selector #'(pclass typ sel)))
                                    ((sel1 sel ...)
                                     #`(cons #,(parse-simple-selector #'sel1)
                                             #,(parse-selector #'(sel ...)))))))
                (parse-rule (lambda (r)
                              (syntax-case r ()
                                ((sel avp ...)
                                 #`(make-css-rule #,(parse-selector #'sel)
                                                  #,(parse-avp-list #'(avp ...)))))))
                (parse-rules (lambda (lst)
                               (syntax-case lst ()
                                 ((fst others ...)
                                  #`(cons #,(parse-rule #'fst)
                                          #,(parse-rules #'(others ...))))
                                 ((fst)
                                  #`(list #,(parse-rule #'fst)))
                                 (()
                                  #'()))))
                (parse-clause (lambda (c)
                                (syntax-case* c (import)
                                  module-or-top-identifier=?
                                  ((import url)
                                   #'(make-import-clause url)))))
                (parse-clauses (lambda (lst)
                                 (syntax-case* lst (import)
                                   module-or-top-identifier=?
                                   (((import . a) . rst)
                                    (begin (write "parsing import")
                                           (newline)
                                           #`(cons #,(parse-clause #'(import . a))
                                                   #,(parse-clauses #'rst))))
                                   (rst
                                    (parse-rules #'rst))))))
         #`(make-css-node #,(parse-clauses #'lst))))))
  
  (provide css-attr
           css
           (rename css text/css)
           css/html
           (rename css/html text/css-html)
           css-link
           (rename css-link external-stylesheet/html)
           css-link/xml
           (rename css-link/xml external-stylesheet/xml)
           css-node?)
  
  )

