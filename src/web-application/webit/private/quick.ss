; quick.ss -- implements a "fast" stylesheet ("stylesheet0")
;   limitations: [1] cannot be linked with other stylesheets
;                [2] no support for EPS

#cs
(module quick mzscheme
  
  (require-for-syntax "valid-types-helper.ss"
                      (lib "list.ss"))
  
  (require "xml-core.ss")
  
  (provide stylesheet0)
  
  (define-syntax (stylesheet0 stx)
    (syntax-case stx ()
      [(_ attr-spec exp ...)
       (let* ((attrs #'attr-spec)
              (is? (lambda (exp? val?)
                     (lambda (i)
                       (syntax-case i (xml-macro xml-micro xml-expander)
                         [(xml-macro tag fun)
                          exp?]
                         [(xml-micro tag fun)
                          exp?]
                         [(xml-expander tag fun)
                          (raise-syntax-error 'stylesheet0
                                              "xml-expander form not allowed in stylesheet0 syntax"
                                              stx
                                              i)]
                         [something
                          val?]))))
              (vals (filter (is? #f #t)
                            (syntax->list #'(exp ...))))
              (exps (filter (is? #t #f)
                            (syntax->list #'(exp ...))))
              (default-tags '(*text* *comment*))
              (all-default-tags '(*element* *text* *comment* *data*))
              (default-exps (filter (lambda (i)
                                      (syntax-case i (xml-macro xml-micro)
                                        [(xml-macro tag fun)
                                         (memq (syntax-e #'tag) default-tags)]
                                        [(xml-micro tag fun)
                                         (memq (syntax-e #'tag) default-tags)]))
                                    exps))
              (def-data-exps (filter (lambda (i)
                                       (syntax-case i (xml-macro xml-micro)
                                         [(xml-macro tag fun)
                                          (eq? (syntax-e #'tag) '*data*)]
                                         [(xml-micro tag fun)
                                          (eq? (syntax-e #'tag) '*data*)]))
                                     exps))
              (def-ele-exps (filter (lambda (i)
                                      (syntax-case i (xml-macro xml-micro)
                                        [(xml-macro tag fun)
                                         (eq? (syntax-e #'tag) '*element*)]
                                        [(xml-micro tag fun)
                                         (eq? (syntax-e #'tag) '*element*)]))
                                    exps))
              (ele-exps (filter (lambda (i)
                                  (syntax-case i (xml-macro xml-micro)
                                    [(xml-macro tag fun)
                                     (not (memq (syntax-e #'tag) all-default-tags))]
                                    [(xml-micro tag fun)
                                     (not (memq (syntax-e #'tag) all-default-tags))]))
                                exps))
              (xml-expand-id (datum->syntax-object stx 'xml-expand))
              (expander-tags (map (lambda (exp temp)
                                    (syntax-case exp ()
                                      [(what tag fun)
                                       (cons (syntax-e #'tag) temp)]))
                                  exps
                                  (generate-temporaries #`(#,@exps)))))
         (define (tag->expander tag)
           (let ((binding (assq tag expander-tags)))
             (if binding
                 (cdr binding)
                 (error "stylesheet0: [internal error] tag not found" tag))))
         (define (make-ele-disp dispatch node rest exp)
           (syntax-case exp (xml-macro xml-micro)
             [(xml-macro tag fun)
              #`((#,(type-exists/ext? #'tag))
                 (#,dispatch (#,(tag->expander (syntax-e #'tag)) #,node) #,@attrs))]
             [(xml-micro tag fun)
              #`((#,(type-exists/ext? #'tag))
                 (#,(tag->expander (syntax-e #'tag)) #,node #,@attrs))]))
         (define (make-def-test tag node)
           (case tag
             ((*text*) #`(string? #,node))
             ((*comment*) #`(xml-comment?  #,node))))
         (define (make-def-disp dispatch node rest exp)
           (syntax-case exp (xml-macro xml-micro)
             [(xml-macro tag fun)
              #`(#,(make-def-test (syntax-e #'tag) node)
                  (#,dispatch (#,(tag->expander (syntax-e #'tag)) #,node) #,@attrs))]
             [(xml-micro tag fun)
              #`(#,(make-def-test (syntax-e #'tag) node)
                  (#,(tag->expander (syntax-e #'tag)) #,node #,@attrs))]))
         (define (make-default-ele-exp node dispatch rest)
           (if (pair? def-ele-exps)
               (syntax-case (car def-ele-exps) (xml-macro xml-micro xml-expander)
                 [(xml-macro tag fun)
                  #`(#,dispatch (#,(tag->expander (syntax-e #'tag)) #,node) #,@attrs)]
                 [(xml-micro tag fun)
                  #`(#,(tag->expander (syntax-e #'tag)) #,node #,@attrs)])
               #`((xml-type #,node)
                  (xml-element-attributes #,node)
                  (map (lambda (i) (#,dispatch i #,@attrs))
                       (xml-element-contents #,node)))))
         (define (make-default-data-exp node dispatch rest)
           (if (pair? def-data-exps)
               (syntax-case (car def-data-exps) (xml-macro xml-micro)
                 [(xml-macro tag fun)
                  #`(#,dispatch (#,(tag->expander (syntax-e #'tag)) #,node) #,@attrs)]
                 [(xml-micro tag fun)
                  #`(#,(tag->expander (syntax-e #'tag)) #,node #,@attrs)])
               #`(if (pair? #,node)
                     (cons (#,dispatch (car #,node))
                           (#,dispatch (cdr #,node)))
                     #,node)))
         (define (make-dispatcher next-e ele-exps default-exps)
           (let ((node (car (generate-temporaries #'(node))))
                 (rest (car (generate-temporaries #'(rest)))))
             #`(lambda (#,node #,@attrs)
                 (cond 
                   ((xml-element? #,node)
                    #,(if (null? ele-exps)
                          (make-default-ele-exp node next-e rest)
                          #`(case (xml-element-tag #,node)
                              #,@(map (lambda (i) (make-ele-disp next-e node rest i)) ele-exps)
                              (else #,(make-default-ele-exp node next-e rest)))))
                   #,@(map (lambda (i) (make-def-disp next-e node rest i)) default-exps)
                   (else #,(make-default-data-exp node next-e rest))))))
         (define (make-expanders exps)
           (map (lambda (i)
                  (syntax-case i ()
                    [(what tag fun)
                     (list (tag->expander (syntax-e #'tag))
                           #'fun)]))
                exps))
         (define (make-result)
           (let ((dispatch xml-expand-id))
             (if (null? vals)
                 #`(letrec (#,@(make-expanders exps)
                             (#,dispatch #,(make-dispatcher dispatch ele-exps default-exps)))
                     #,dispatch)
                 #`(let ()
                     #,@vals
                     (letrec (#,@(make-expanders exps)
                               (#,dispatch #,(make-dispatcher dispatch ele-exps default-exps)))
                       #,dispatch)))))
         (make-result))]))
  
  )
