(module xml-match mzscheme
  
  (provide (all-defined))
  
  (require "xml-core.ss")
  
  (require-for-syntax "xml-helper.ss")
  
  (define-syntax compile-term
    (lambda (stx)
      (define (compile-clause term base-pvars base-dotted-vars cata-fun src-stx)
        (letrec
            ([xml-match-syntax-error
              (lambda (msg sub)
                (raise-syntax-error #f msg src-stx sub))]
             [ellipsis?
              (lambda (stx)
                (and (identifier? stx) (eq? '... (syntax-object->datum stx))))]
             [literal?
              (lambda (stx)
                (let ([x (syntax-object->datum stx)])
                  (or (string? x)
                      (char? x)
                      (number? x)
                      (boolean? x))))]
             [keyword?
              (lambda (stx)
                (and (identifier? stx)
                     (let ([str (symbol->string (syntax-object->datum stx))])
                       (char=? #\: (string-ref str (- (string-length str) 1))))))]
             [check-duplicate-pvar
              (lambda (pvar lst)
                (if (null? lst)
                    (void)
                    (if (bound-identifier=? (car lst) pvar)
                        (xml-match-syntax-error "duplicate pattern variable not allowed"
                                                pvar)
                        (check-duplicate-pvar pvar (cdr lst)))))]
             [add-pat-var
              (lambda (pvar pvar-lst)
                (check-duplicate-pvar pvar pvar-lst)
                (check-duplicate-pvar pvar base-pvars)
                (cons pvar pvar-lst))]
             [add-cata-def
              (lambda (depth cvars cfun ctemp cdefs)
                (cons (list depth cvars cfun ctemp) cdefs))]
             [merge-pvars/checks
              (lambda (lst1 lst2)
                (if (null? lst1)
                    lst2
                    (begin (check-duplicate-pvar (car lst1) lst2)
                           (check-duplicate-pvar (car lst1) base-pvars)
                           (cons (car lst1) (merge-pvars/checks (cdr lst1) lst2)))))]
             [process-cata-exp
              (lambda (depth cfun ctemp)
                (if (= depth 0)
                    (with-syntax ([cf cfun]
                                  [ct ctemp])
                      (syntax (cf ct)))
                    (let ([new-ctemp (car (generate-temporaries (list ctemp)))])
                      (with-syntax ([ct ctemp]
                                    [nct new-ctemp]
                                    [body (process-cata-exp (- depth 1) cfun new-ctemp)])
                        (syntax (map (lambda (nct) body) ct))))))]
             [process-cata-defs
              (lambda (cata-defs body)
                (if (null? cata-defs)
                    body
                    (with-syntax ([(cata-binding ...)
                                   (map (lambda (def)
                                          (with-syntax ([bvar (cadr def)]
                                                        [bval (process-cata-exp (car def)
                                                                                (caddr def)
                                                                                (cadddr def))])
                                            (syntax (bvar bval))))
                                        cata-defs)]
                                  [body-stx body])
                      (syntax (let-values (cata-binding ...)
                                body-stx)))))]
             [cata-defs->pvar-lst
              (lambda (lst)
                (if (null? lst)
                    '()
                    (let iter ([items (cadr (car lst))])
                      (syntax-case items ()
                        [() (cata-defs->pvar-lst (cdr lst))]
                        [(fst . rst) (cons (syntax fst) (iter (syntax rst)))]))))]
             [remove-shadowed-vars
              (lambda (lst top-base)
                (if (null? lst)
                    '()
                    (let loop ([base top-base])
                      (if (null? base)
                          (cons (car lst) (remove-shadowed-vars (cdr lst) top-base))
                          (if (bound-identifier=? (car lst) (car base))
                              (remove-shadowed-vars (cdr lst) top-base)
                              (loop (cdr base)))))))]
             [process-output-action
              (lambda (action pvars dotted-vars)
                (with-syntax ([src-term src-stx]
                              [pvars-stx (append base-pvars pvars)]
                              [dvars-stx (append (remove-shadowed-vars base-dotted-vars pvars)
                                                 dotted-vars)]
                              [action-stx action])
                  (syntax (with-match-vars src-term pvars-stx dvars-stx action-stx))))]
             [compile-element-pat
              (lambda (ele exp nextp fail-k pvar-lst depth cata-defs dotted-vars)
                (syntax-case ele ()
                  [(tag kwd . items)
                   (and (identifier? (syntax tag)) (keyword? (syntax kwd)))
                   (let ([attr-exp (car (generate-temporaries (list exp)))]
                         [body-exp (car (generate-temporaries (list exp)))])
                     (let-values ([(tests new-pvar-lst new-cata-defs new-dotted-vars)
                                   (compile-attr-list (syntax (kwd . items))
                                                      attr-exp
                                                      body-exp
                                                      nextp
                                                      fail-k
                                                      pvar-lst
                                                      depth
                                                      cata-defs
                                                      dotted-vars)])
                       (values (with-syntax ([x exp]
                                             [ax attr-exp]
                                             [bx body-exp]
                                             [etag (type->tag (syntax tag))]
                                             [body tests]
                                             [fail-to fail-k])
                                 (syntax (if (and (pair? x) (eq? 'etag (xml-element-tag x)))
                                             (let ([ax (xml-element-attributes x)]
                                                   [bx (xml-element-contents x)])
                                               body)
                                             (fail-to))))
                               new-pvar-lst
                               new-cata-defs
                               new-dotted-vars)))]
                  [(tag . items)
                   (identifier? (syntax tag))
                   (let ([body-exp (car (generate-temporaries (list exp)))])
                     (let-values ([(tests new-pvar-lst new-cata-defs new-dotted-vars)
                                   (compile-item-list (syntax items)
                                                      body-exp
                                                      nextp
                                                      fail-k
                                                      #t
                                                      pvar-lst
                                                      depth
                                                      cata-defs
                                                      dotted-vars)])
                       (values (with-syntax ([x exp]
                                             [bx body-exp]
                                             [etag (type->tag (syntax tag))]
                                             [body tests]
                                             [fail-to fail-k])
                                 (syntax (if (and (pair? x) (eq? 'etag (xml-element-tag x)))
                                             (let ([bx (xml-element-contents x)])
                                               body)
                                             (fail-to))))
                               new-pvar-lst
                               new-cata-defs
                               new-dotted-vars)))]))]
             [compile-end-element
              (lambda (exp nextp fail-k pvar-lst cata-defs dotted-vars)
                (let-values ([(next-tests new-pvar-lst new-cata-defs new-dotted-vars)
                              (nextp pvar-lst cata-defs dotted-vars)])
                  (values (with-syntax ([x exp]
                                        [body next-tests]
                                        [fail-to fail-k])
                            (syntax (if (null? x) body (fail-to))))
                          new-pvar-lst
                          new-cata-defs
                          new-dotted-vars)))]
             [compile-attr-list
              (lambda (lst attr-exp body-exp nextp fail-k pvar-lst depth cata-defs dotted-vars)
;                (syntax-case lst (unquote ->)
                (syntax-case lst (unquote)
                  [(kwd [(unquote [cata arrow cvar ...]) default] . rst)
                   (and (eq? (syntax-e (syntax arrow)) '->) (keyword? (syntax kwd)))
                   (let ([ctemp (car (generate-temporaries (syntax ([cvar ...]))))])
                     (let-values ([(tests new-pvar-lst new-cata-defs new-dotted-vars)
                                   (compile-attr-list (syntax rst)
                                                      attr-exp
                                                      body-exp
                                                      nextp
                                                      fail-k
                                                      (add-pat-var ctemp pvar-lst)
                                                      depth
                                                      (add-cata-def depth
                                                                    (syntax [cvar ...])
                                                                    (syntax cata)
                                                                    ctemp
                                                                    cata-defs)
                                                      dotted-vars)])
                       (values (with-syntax ([atag (attribute->tag (syntax kwd))]
                                             [ax attr-exp]
                                             [ct ctemp]
                                             [body tests])
                                 (syntax (let ([binding (match-xml-attribute 'atag ax)])
                                           (let ([ct (if binding
                                                         (cadr binding)
                                                         default)])
                                             body))))
                               new-pvar-lst
                               new-cata-defs
                               new-dotted-vars)))]
                  [(kwd [(unquote [cvar ...]) default] . rst)
                   (keyword? (syntax kwd))
                   (let ([ctemp (car (generate-temporaries (syntax ([cvar ...]))))])
                     (if (not cata-fun)
                         (xml-match-syntax-error "catamorphism not allowed in this context"
                                                 (syntax [cvar ...])))
                     (let-values ([(tests new-pvar-lst new-cata-defs new-dotted-vars)
                                   (compile-attr-list (syntax rst)
                                                      attr-exp
                                                      body-exp
                                                      nextp
                                                      fail-k
                                                      (add-pat-var ctemp pvar-lst)
                                                      depth
                                                      (add-cata-def depth
                                                                    (syntax [cvar ...])
                                                                    cata-fun
                                                                    ctemp
                                                                    cata-defs)
                                                      dotted-vars)])
                       (values (with-syntax ([atag (attribute->tag (syntax kwd))]
                                             [ax attr-exp]
                                             [ct ctemp]
                                             [body tests])
                                 (syntax (let ([binding (match-xml-attribute 'atag ax)])
                                           (let ([ct (if binding
                                                         (cadr binding)
                                                         default)])
                                             body))))
                               new-pvar-lst
                               new-cata-defs
                               new-dotted-vars)))]
                  [(kwd [(unquote var) default] . rst)
                   (and (keyword? (syntax kwd)) (identifier? (syntax var)))
                   (let-values ([(tests new-pvar-lst new-cata-defs new-dotted-vars)
                                 (compile-attr-list (syntax rst)
                                                    attr-exp
                                                    body-exp
                                                    nextp
                                                    fail-k
                                                    (add-pat-var (syntax var) pvar-lst)
                                                    depth
                                                    cata-defs
                                                    dotted-vars)])
                     (values (with-syntax ([atag (attribute->tag (syntax kwd))]
                                           [ax attr-exp]
                                           [body tests])
                               (syntax (let ([binding (match-xml-attribute 'atag ax)])
                                         (let ([var (if binding
                                                        (cadr binding)
                                                        default)])
                                           body))))
                             new-pvar-lst
                             new-cata-defs
                             new-dotted-vars))]
                  [(kwd (unquote [cata arrow cvar ...]) . rst)
                   (and (eq? (syntax-e (syntax arrow)) '->) (keyword? (syntax kwd)))
                   (let ([ctemp (car (generate-temporaries (syntax ([cvar ...]))))])
                     (let-values ([(tests new-pvar-lst new-cata-defs new-dotted-vars)
                                   (compile-attr-list (syntax rst)
                                                      attr-exp
                                                      body-exp
                                                      nextp
                                                      fail-k
                                                      (add-pat-var ctemp pvar-lst)
                                                      depth
                                                      (add-cata-def depth
                                                                    (syntax [cvar ...])
                                                                    (syntax cata)
                                                                    ctemp
                                                                    cata-defs)
                                                      dotted-vars)])
                       (values (with-syntax ([atag (attribute->tag (syntax kwd))]
                                             [ax attr-exp]
                                             [ct ctemp]
                                             [body tests]
                                             [fail-to fail-k])
                                 (syntax (let ([binding (match-xml-attribute 'atag ax)])
                                           (if binding
                                               (let ([ct (cadr binding)])
                                                 body)
                                               (fail-to)))))
                               new-pvar-lst
                               new-cata-defs
                               new-dotted-vars)))]
                  [(kwd (unquote [cvar ...]) . rst)
                   (keyword? (syntax kwd))
                   (let ([ctemp (car (generate-temporaries (syntax ([cvar ...]))))])
                     (if (not cata-fun)
                         (xml-match-syntax-error "catamorphism not allowed in this context"
                                                 (syntax [cvar ...])))
                     (let-values ([(tests new-pvar-lst new-cata-defs new-dotted-vars)
                                   (compile-attr-list (syntax rst)
                                                      attr-exp
                                                      body-exp
                                                      nextp
                                                      fail-k
                                                      (add-pat-var ctemp pvar-lst)
                                                      depth
                                                      (add-cata-def depth
                                                                    (syntax [cvar ...])
                                                                    cata-fun
                                                                    ctemp
                                                                    cata-defs)
                                                      dotted-vars)])
                       (values (with-syntax ([atag (attribute->tag (syntax kwd))]
                                             [ax attr-exp]
                                             [ct ctemp]
                                             [body tests]
                                             [fail-to fail-k])
                                 (syntax (let ([binding (match-xml-attribute 'atag ax)])
                                           (if binding
                                               (let ([ct (cadr binding)])
                                                 body)
                                               (fail-to)))))
                               new-pvar-lst
                               new-cata-defs
                               new-dotted-vars)))]
                  [(kwd (unquote var) . rst)
                   (and (keyword? (syntax kwd)) (identifier? (syntax var)))
                   (let-values ([(tests new-pvar-lst new-cata-defs new-dotted-vars)
                                 (compile-attr-list (syntax rst)
                                                    attr-exp
                                                    body-exp
                                                    nextp
                                                    fail-k
                                                    (add-pat-var (syntax var) pvar-lst)
                                                    depth
                                                    cata-defs
                                                    dotted-vars)])
                     (values (with-syntax ([atag (attribute->tag (syntax kwd))]
                                           [ax attr-exp]
                                           [body tests]
                                           [fail-to fail-k])
                               (syntax (let ([binding (match-xml-attribute 'atag ax)])
                                         (if binding
                                             (let ([var (cadr binding)])
                                               body)
                                             (fail-to)))))
                             new-pvar-lst
                             new-cata-defs
                             new-dotted-vars))]
                  [(kwd (i ...) . rst)
                   (keyword? (syntax kwd))
                   (xml-match-syntax-error "bad attribute pattern"
                                           (syntax (kwd (i ...))))]
                  [(kwd i . rst)
                   (and (keyword? (syntax kwd)) (identifier? (syntax i)))
                   (xml-match-syntax-error "bad attribute pattern"
                                           (syntax (kwd i)))]
                  [(kwd literal . rst)
                   (and (keyword? (syntax kwd)) (literal? (syntax literal)))
                   (let-values ([(tests new-pvar-lst new-cata-defs new-dotted-vars)
                                 (compile-attr-list (syntax rst)
                                                    attr-exp
                                                    body-exp
                                                    nextp
                                                    fail-k
                                                    pvar-lst
                                                    depth
                                                    cata-defs
                                                    dotted-vars)])
                     (values (with-syntax ([atag (attribute->tag (syntax kwd))]
                                           [ax attr-exp]
                                           [body tests]
                                           [fail-to fail-k])
                               (syntax (let ([binding (match-xml-attribute 'atag ax)])
                                         (if binding
                                             (if (equal? (cadr binding) literal)
                                                 body
                                                 (fail-to))
                                             (fail-to)))))
                             new-pvar-lst
                             new-cata-defs
                             new-dotted-vars))]
                  [rst (compile-item-list (syntax rst)
                                          body-exp
                                          nextp
                                          fail-k
                                          #t
                                          pvar-lst
                                          depth
                                          cata-defs
                                          dotted-vars)]))]
             [compile-item-list
              (lambda (lst exp nextp fail-k ellipsis-allowed? pvar-lst depth cata-defs dotted-vars)
;                (syntax-case lst (unquote ->)
                (syntax-case lst (unquote)
                  [() (compile-end-element exp nextp fail-k pvar-lst cata-defs dotted-vars)]
                  [(unquote var)
                   (identifier? (syntax var))
                   (if (not ellipsis-allowed?)
                       (xml-match-syntax-error "improper list pattern not allowed in this context"
                                               (syntax dots))
                       (let-values ([(next-tests new-pvar-lst new-cata-defs new-dotted-vars)
                                     (nextp (add-pat-var (syntax var) pvar-lst) cata-defs dotted-vars)])
                         (values (with-syntax ([x exp]
                                               [body next-tests])
                                   (syntax (let ([var x]) body)))
                                 new-pvar-lst
                                 new-cata-defs
                                 new-dotted-vars)))]
                  [(unquote [cata arrow cvar ...])
                   (eq? (syntax-e (syntax arrow)) '->)
                   (if (not ellipsis-allowed?)
                       (xml-match-syntax-error "improper list pattern not allowed in this context"
                                               (syntax dots))
                       (let ([ctemp (car (generate-temporaries (syntax ([cvar ...]))))])
                         (let-values ([(next-tests new-pvar-lst new-cata-defs new-dotted-vars)
                                       (nextp (add-pat-var ctemp pvar-lst)
                                              (add-cata-def depth
                                                            (syntax [cvar ...])
                                                            (syntax cata)
                                                            ctemp
                                                            cata-defs)
                                              dotted-vars)])
                           (values (with-syntax ([ct ctemp]
                                                 [x exp]
                                                 [body next-tests])
                                     (syntax (let ([ct x]) body)))
                                   new-pvar-lst
                                   new-cata-defs
                                   new-dotted-vars))))]
                  [(unquote [cvar ...])
                   (let ([ctemp (car (generate-temporaries (syntax ([cvar ...]))))])
                     (if (not cata-fun)
                         (xml-match-syntax-error "catamorphism not allowed in this context"
                                                 (syntax [cvar ...])))
                     (let-values ([(next-tests new-pvar-lst new-cata-defs new-dotted-vars)
                                   (nextp (add-pat-var ctemp pvar-lst)
                                          (add-cata-def depth
                                                        (syntax [cvar ...])
                                                        cata-fun
                                                        ctemp
                                                        cata-defs)
                                          dotted-vars)])
                       (values (with-syntax ([ct ctemp]
                                             [x exp]
                                             [body next-tests])
                                 (syntax (let ([ct x]) body)))
                               new-pvar-lst
                               new-cata-defs
                               new-dotted-vars)))]
                  [(item dots . rst)
                   (ellipsis? (syntax dots))
                   (if (not ellipsis-allowed?)
                       (xml-match-syntax-error "ellipses not allowed in this context"
                                               (syntax dots))
                       (compile-dotted-pattern-list (syntax item)
                                                    (syntax rst)
                                                    exp
                                                    nextp
                                                    fail-k
                                                    pvar-lst
                                                    depth
                                                    cata-defs
                                                    dotted-vars))]
                  [(item . rst)
                   (compile-item (syntax item)
                                 exp
                                 (lambda (new-exp new-pvar-lst new-cata-defs new-dotted-vars)
                                   (compile-item-list (syntax rst)
                                                      new-exp
                                                      nextp
                                                      fail-k
                                                      ellipsis-allowed?
                                                      new-pvar-lst
                                                      depth
                                                      new-cata-defs
                                                      new-dotted-vars))
                                 fail-k
                                 pvar-lst
                                 depth
                                 cata-defs
                                 dotted-vars)]))]
             [compile-dotted-pattern-list
              (lambda (item
                       tail
                       exp
                       nextp
                       fail-k
                       pvar-lst
                       depth
                       cata-defs
                       dotted-vars)
                (let-values ([(tail-tests tail-pvar-lst tail-cata-defs tail-dotted-vars)
                              (compile-item-list tail
                                                 (syntax lst)
                                                 (lambda (new-pvar-lst new-cata-defs new-dotted-vars)
                                                   (values (with-syntax ([(npv ...) new-pvar-lst])
                                                             (syntax (values #t npv ...)))
                                                           new-pvar-lst
                                                           new-cata-defs
                                                           new-dotted-vars))
                                                 (syntax fail)
                                                 #f
                                                 '()
                                                 depth
                                                 '()
                                                 '())]
                             [(item-tests item-pvar-lst item-cata-defs item-dotted-vars)
                              (compile-item item
                                            (syntax lst)
                                            (lambda (new-exp new-pvar-lst new-cata-defs new-dotted-vars)
                                              (values (with-syntax ([(npv ...) new-pvar-lst])
                                                        (syntax (values #t (cdr lst) npv ...)))
                                                      new-pvar-lst
                                                      new-cata-defs
                                                      new-dotted-vars))
                                            (syntax fail)
                                            '()
                                            (+ 1 depth)
                                            '()
                                            '())])
                  ; more here: check for duplicate cata-defs
                  (let-values ([(final-tests final-pvar-lst final-cata-defs final-dotted-vars)
                                (nextp (merge-pvars/checks tail-pvar-lst (merge-pvars/checks item-pvar-lst pvar-lst))
                                       (append tail-cata-defs item-cata-defs cata-defs)
                                       (merge-pvars/checks item-pvar-lst
                                                           (merge-pvars/checks (cata-defs->pvar-lst item-cata-defs)
                                                                               (merge-pvars/checks tail-dotted-vars
                                                                                                   dotted-vars))))])
                    (let ([temp-item-pvar-lst (generate-temporaries item-pvar-lst)])
                      (values
                       (with-syntax
                           ([x exp]
                            [fail-to fail-k]
                            [tail-body tail-tests]
                            [item-body item-tests]
                            [final-body final-tests]
                            [(ipv ...) item-pvar-lst]
                            [(gpv ...) temp-item-pvar-lst]
                            [(tpv ...) tail-pvar-lst]
                            [(item-void ...) (map (lambda (i) (syntax (void))) item-pvar-lst)]
                            [(tail-void ...) (map (lambda (i) (syntax (void))) tail-pvar-lst)]
                            [(item-null ...) (map (lambda (i) (syntax '())) item-pvar-lst)]
                            [(item-cons ...) (map (lambda (a b)
                                                    (with-syntax ([xa a]
                                                                  [xb b])
                                                      (syntax (cons xa xb))))
                                                  item-pvar-lst
                                                  temp-item-pvar-lst)])
                         (syntax (letrec ([match-tail
                                           (lambda (lst fail)
                                             tail-body)]
                                          [match-item
                                           (lambda (lst)
                                             (let ([fail (lambda ()
                                                           (values #f
                                                                   lst
                                                                   item-void ...))])
                                               item-body))]
                                          [match-dotted
                                           (lambda (x)
                                             (let-values ([(tail-res tpv ...)
                                                           (match-tail x
                                                                       (lambda ()
                                                                         (values #f
                                                                                 tail-void ...)))])
                                               (if tail-res
                                                   (values item-null ...
                                                           tpv ...)
                                                   (let-values ([(res new-x ipv ...) (match-item x)])
                                                     (if res
                                                         (let-values ([(gpv ... tpv ...)
                                                                       (match-dotted new-x)])
                                                           (values item-cons ... tpv ...))
                                                         (let-values ([(last-tail-res tpv ...)
                                                                       (match-tail x fail-to)])
                                                           (values item-null ... tpv ...)))))))])
                                   (let-values ([(ipv ... tpv ...)
                                                 (match-dotted x)])
                                     final-body))))
                       final-pvar-lst
                       final-cata-defs
                       final-dotted-vars)))))]
             [compile-item
              (lambda (item exp nextp fail-k pvar-lst depth cata-defs dotted-vars)
;                (syntax-case item (unquote ->)
                (syntax-case item (unquote)
                  ; normal pattern var
                  [(unquote var)
                   (identifier? (syntax var))
                   (let ([new-exp (car (generate-temporaries (list exp)))])
                     (let-values ([(next-tests new-pvar-lst new-cata-defs new-dotted-vars)
                                   (nextp new-exp (add-pat-var (syntax var) pvar-lst) cata-defs dotted-vars)])
                       (values (with-syntax ([x exp]
                                             [nx new-exp]
                                             [body next-tests]
                                             [fail-to fail-k])
                                 (syntax (if (pair? x)
                                             (let ([nx (cdr x)]
                                                   [var (car x)])
                                               body)
                                             (fail-to))))
                               new-pvar-lst
                               new-cata-defs
                               new-dotted-vars)))]
                  ; named catamorphism
                  [(unquote [cata arrow cvar ...])
                   (eq? (syntax-e (syntax arrow)) '->)
                   (let ([new-exp (car (generate-temporaries (list exp)))]
                         [ctemp (car (generate-temporaries (syntax ([cvar ...]))))])
                     (let-values ([(next-tests new-pvar-lst new-cata-defs new-dotted-vars)
                                   (nextp new-exp
                                          (add-pat-var ctemp pvar-lst)
                                          (add-cata-def depth
                                                        (syntax [cvar ...])
                                                        (syntax cata)
                                                        ctemp
                                                        cata-defs)
                                          dotted-vars)])
                       (values (with-syntax ([x exp]
                                             [nx new-exp]
                                             [ct ctemp]
                                             [body next-tests]
                                             [fail-to fail-k])
                                 (syntax (if (pair? x)
                                             (let ([nx (cdr x)]
                                                   [ct (car x)])
                                               body)
                                             (fail-to))))
                               new-pvar-lst
                               new-cata-defs
                               new-dotted-vars)))]
                  ; basic catamorphism
                  [(unquote [cvar ...])
                   (let ([new-exp (car (generate-temporaries (list exp)))]
                         [ctemp (car (generate-temporaries (syntax ([cvar ...]))))])
                     (if (not cata-fun)
                         (xml-match-syntax-error "catamorphism not allowed in this context"
                                                 (syntax [cvar ...])))
                     (let-values ([(next-tests new-pvar-lst new-cata-defs new-dotted-vars)
                                   (nextp new-exp
                                          (add-pat-var ctemp pvar-lst)
                                          (add-cata-def depth
                                                        (syntax [cvar ...])
                                                        cata-fun
                                                        ctemp
                                                        cata-defs)
                                          dotted-vars)])
                       (values (with-syntax ([x exp]
                                             [nx new-exp]
                                             [ct ctemp]
                                             [body next-tests]
                                             [fail-to fail-k])
                                 (syntax (if (pair? x)
                                             (let ([nx (cdr x)]
                                                   [ct (car x)])
                                               body)
                                             (fail-to))))
                               new-pvar-lst
                               new-cata-defs
                               new-dotted-vars)))]
                  [(tag item ...)
                   (identifier? (syntax tag))
                   (let ([new-exp (car (generate-temporaries (list exp)))])
                     (let-values ([(after-tests after-pvar-lst after-cata-defs after-dotted-vars)
                                   (compile-element-pat (syntax (tag item ...))
                                                        (with-syntax ([x exp])
                                                          (syntax (car x)))
                                                        (lambda (more-pvar-lst more-cata-defs more-dotted-vars)
                                                          (let-values ([(next-tests new-pvar-lst
                                                                                    new-cata-defs
                                                                                    new-dotted-vars)
                                                                        (nextp new-exp
                                                                               more-pvar-lst
                                                                               more-cata-defs
                                                                               more-dotted-vars)])
                                                            (values (with-syntax ([x exp]
                                                                                  [nx new-exp]
                                                                                  [body next-tests])
                                                                      (syntax (let ([nx (cdr x)])
                                                                                body)))
                                                                    new-pvar-lst
                                                                    new-cata-defs
                                                                    new-dotted-vars)))
                                                        fail-k
                                                        pvar-lst
                                                        depth
                                                        cata-defs
                                                        dotted-vars)])
                       ; test that we are not at the end of an item-list, BEFORE
                       ; entering tests for the element pattern (against the 'car' of the item-list)
                       (values (with-syntax ([x exp]
                                             [body after-tests]
                                             [fail-to fail-k])
                                 (syntax (if (pair? x)
                                             body
                                             (fail-to))))
                               after-pvar-lst
                               after-cata-defs
                               after-dotted-vars)))]
                  [(i ...)
                   (xml-match-syntax-error "bad pattern syntax (not an element pattern)"
                                           (syntax (i ...)))]
                  [i
                   (identifier? (syntax i))
                   (xml-match-syntax-error "bad pattern syntax (symbol not allowed in this context)"
                                           (syntax i))]
                  [literal
                   (literal? (syntax literal))
                   (let ([new-exp (car (generate-temporaries (list exp)))])
                     (let-values ([(next-tests new-pvar-lst new-cata-defs new-dotted-vars)
                                   (nextp new-exp pvar-lst cata-defs dotted-vars)])
                       (values (with-syntax ([x exp]
                                             [nx new-exp]
                                             [body next-tests]
                                             [fail-to fail-k])
                                 (syntax (if (and (pair? x) (equal? literal (car x)))
                                             (let ([nx (cdr x)])
                                               body)
                                             (fail-to))))
                               new-pvar-lst
                               new-cata-defs
                               new-dotted-vars)))]))])
          (let ([fail-k (syntax failure)])
;            (syntax-case term (unquote guard ->)
            (syntax-case term (unquote guard)
              [(((unquote var) (guard gexp ...) action0 action ...)
                exp
                fail-exp)
               (identifier? (syntax var))
               (with-syntax ([body (process-output-action (syntax (begin action0 action ...))
                                                          (add-pat-var (syntax var) '())
                                                          '())])
                 (syntax (let ([var exp])
                           (if (and gexp ...)
                               body
                               (fail-exp)))))]
              [(((unquote [cata arrow cvar ...]) (guard gexp ...) action0 action ...)
                exp
                fail-exp)
               (eq? (syntax-e (syntax arrow)) '->)
               (with-syntax ([body (process-output-action (syntax (begin action0 action ...))
                                                          '()
                                                          '())])
                 (syntax (if (and gexp ...)
                             (let-values ([(cvar ...) (cata exp)])
                               body)
                             (fail-exp))))]
              [(((unquote [cvar ...]) (guard gexp ...) action0 action ...)
                exp
                fail-exp)
               (if (not cata-fun)
                   (xml-match-syntax-error "catamorphism not allowed in this context"
                                           (syntax [cvar ...]))
                   (with-syntax ([cata cata-fun]
                                 [body (process-output-action (syntax (begin action0 action ...))
                                                              '()
                                                              '())])
                     (syntax (if (and gexp ...)
                                 (let-values ([(cvar ...) (cata exp)])
                                   body)
                                 (fail-exp)))))]
              [(((unquote var) action0 action ...) exp fail-exp)
               (identifier? (syntax var))
               (with-syntax ([body (process-output-action (syntax (begin action0 action ...))
                                                          (add-pat-var (syntax var) '())
                                                          '())])
                 (syntax (let ([var exp])
                           body)))]
              [(((unquote [cata arrow cvar ...]) action0 action ...) exp fail-exp)
               (eq? (syntax-e (syntax arrow)) '->)
               (with-syntax ([body (process-output-action (syntax (begin action0 action ...))
                                                          '()
                                                          '())])
                 (syntax (let-values ([(cvar ...) (cata exp)])
                           body)))]
              [(((unquote [cvar ...]) action0 action ...) exp fail-exp)
               (if (not cata-fun)
                   (xml-match-syntax-error "catamorphism not allowed in this context"
                                           (syntax [cvar ...]))
                   (with-syntax ([cata cata-fun]
                                 [body (process-output-action (syntax (begin action0 action ...))
                                                              '()
                                                              '())])
                     (syntax (let-values ([(cvar ...) (cata exp)])
                               body))))]
              [(((lst . rst) (guard gexp ...) action0 action ...) exp fail-exp)
               (and (identifier? (syntax lst)) (eq? 'list (syntax-object->datum (syntax lst))))
               (let-values ([(result pvar-lst cata-defs dotted-vars)
                             (compile-item-list (syntax rst)
                                                (syntax exp)
                                                (lambda (new-pvar-lst new-cata-defs new-dotted-vars)
                                                  (values
                                                   (with-syntax
                                                       ([exp-body (process-cata-defs new-cata-defs
                                                                                     (process-output-action
                                                                                      (syntax (begin action0
                                                                                                     action ...))
                                                                                      new-pvar-lst
                                                                                      new-dotted-vars))]
                                                        [fail-to fail-k])
                                                     (syntax (if (and gexp ...) exp-body (fail-to))))
                                                   new-pvar-lst
                                                   new-cata-defs
                                                   new-dotted-vars))
                                                fail-k
                                                #t
                                                '()
                                                0
                                                '()
                                                '())])
                 (with-syntax ([fail-to fail-k]
                               [body result])
                   (syntax (let ([fail-to fail-exp])
                             (if (nodeset? exp)
                                 body
                                 (fail-to))))))]
              [(((lst . rst) action0 action ...) exp fail-exp)
               (and (identifier? (syntax lst)) (eq? 'list (syntax-object->datum (syntax lst))))
               (let-values ([(result pvar-lst cata-defs dotted-vars)
                             (compile-item-list (syntax rst)
                                                (syntax exp)
                                                (lambda (new-pvar-lst new-cata-defs new-dotted-vars)
                                                  (values (process-cata-defs new-cata-defs
                                                                             (process-output-action
                                                                              (syntax (begin action0
                                                                                             action ...))
                                                                              new-pvar-lst
                                                                              new-dotted-vars))
                                                          new-pvar-lst
                                                          new-cata-defs
                                                          new-dotted-vars))
                                                fail-k
                                                #t
                                                '()
                                                0
                                                '()
                                                '())])
                 (with-syntax ([body result]
                               [fail-to fail-k])
                   (syntax (let ([fail-to fail-exp])
                             (if (nodeset? exp)
                                 body
                                 (fail-to))))))]
              [(((fst . rst) (guard gexp ...) action0 action ...) exp fail-exp)
               (identifier? (syntax fst))
               (let-values ([(result pvar-lst cata-defs dotted-vars)
                             (compile-element-pat (syntax (fst . rst))
                                                  (syntax exp)
                                                  (lambda (new-pvar-lst new-cata-defs new-dotted-vars)
                                                    (values
                                                     (with-syntax
                                                         ([body (process-cata-defs new-cata-defs
                                                                                   (process-output-action
                                                                                    (syntax (begin action0
                                                                                                   action ...))
                                                                                    new-pvar-lst
                                                                                    new-dotted-vars))]
                                                          [fail-to fail-k])
                                                       (syntax (if (and gexp ...) body (fail-to))))
                                                     new-pvar-lst
                                                     new-cata-defs
                                                     new-dotted-vars))
                                                  fail-k
                                                  '()
                                                  0
                                                  '()
                                                  '())])
                 (with-syntax ([fail-to fail-k]
                               [body result])
                   (syntax (let ([fail-to fail-exp])
                             body))))]
              [(((fst . rst) action0 action ...) exp fail-exp)
               (identifier? (syntax fst))
               (let-values ([(result pvar-lst cata-defs dotted-vars)
                             (compile-element-pat (syntax (fst . rst))
                                                  (syntax exp)
                                                  (lambda (new-pvar-lst new-cata-defs new-dotted-vars)
                                                    (values (process-cata-defs new-cata-defs
                                                                               (process-output-action
                                                                                (syntax (begin action0
                                                                                               action ...))
                                                                                new-pvar-lst
                                                                                new-dotted-vars))
                                                            new-pvar-lst
                                                            new-cata-defs
                                                            new-dotted-vars))
                                                  fail-k
                                                  '()
                                                  0
                                                  '()
                                                  '())])
                 (with-syntax ([fail-to fail-k]
                               [body result])
                   (syntax (let ([fail-to fail-exp])
                             body))))]
              [(((i ...) (guard gexp ...) action0 action ...) exp fail-exp)
               (xml-match-syntax-error "bad pattern syntax (not an element pattern)"
                                       (syntax (i ...)))]
              [(((i ...) action0 action ...) exp fail-exp)
               (xml-match-syntax-error "bad pattern syntax (not an element pattern)"
                                       (syntax (i ...)))]
              [((pat (guard gexp ...) action0 action ...) exp fail-exp)
               (identifier? (syntax pat))
               (xml-match-syntax-error "bad pattern syntax (symbol not allowed in this context)"
                                       (syntax pat))]
              [((pat action0 action ...) exp fail-exp)
               (identifier? (syntax pat))
               (xml-match-syntax-error "bad pattern syntax (symbol not allowed in this context)"
                                       (syntax pat))]
              [((literal (guard gexp ...) action0 action ...) exp fail-exp)
               (literal? (syntax literal))
               (with-syntax ([body (process-output-action (syntax (begin action0 action ...))
                                                          '()
                                                          '())])
                 (syntax (if (and (equal? literal exp) (and gexp ...))
                             body
                             (fail-exp))))]
              [((literal action0 action ...) exp fail-exp)
               (literal? (syntax literal))
               (with-syntax ([body (process-output-action (syntax (begin action0 action ...))
                                                          '()
                                                          '())])
                 (syntax (if (equal? literal exp)
                             body
                             (fail-exp))))]))))
      (syntax-case stx ()
        [(compile-term term pvar-lst dotted-pvar-lst cata-fun synform)
         (compile-clause (syntax term)
                         (syntax->list (syntax pvar-lst))
                         (syntax->list (syntax dotted-pvar-lst))
                         (syntax-case (syntax cata-fun) ()
                           [#f #f]
                           [other (syntax cata-fun)])
                         (syntax synform))])))
  
  (define-syntax with-match-vars
    (lambda (stx)
      (define (transform-output-term src-stx action pvars dotted-vars)
        (letrec
            ([xml-match-syntax-error
              (lambda (msg sub)
                (raise-syntax-error #f msg src-stx sub))]
             [ellipsis?
              (lambda (stx)
                (and (identifier? stx) (eq? '... (syntax-object->datum stx))))]
             [transform-output-action
              (lambda (action pvars dotted-vars)
                (define (finite-lst? lst)
                  (syntax-case lst ()
                    (item
                     (identifier? (syntax item))
                     #f)
                    (()
                     #t)
                    ((fst dots . rst)
                     (ellipsis? (syntax dots))
                     #f)
                    ((fst . rst)
                     (finite-lst? (syntax rst)))))
                (define (expand-lst lst)
                  (syntax-case lst ()
                    [() (syntax '())]
                    [item
                     (identifier? (syntax item))
                     (syntax item)]
                    [(fst dots)
                     (ellipsis? (syntax dots))
                     (expand-dotted-item (syntax dots)
                                         (transform-output-action (syntax fst)
                                                                  pvars
                                                                  dotted-vars))]
                    [(fst dots . rst)
                     (ellipsis? (syntax dots))
                     (with-syntax ([exp-lft (expand-dotted-item (syntax dots)
                                                                (transform-output-action (syntax fst)
                                                                                         pvars
                                                                                         dotted-vars))]
                                   [exp-rgt (expand-lst (syntax rst))])
                       (syntax (append exp-lft exp-rgt)))]
                    [(fst)
                     (transform-output-action (syntax fst)
                                              pvars
                                              dotted-vars)]
                    [(fst . rst)
                     (with-syntax ([exp-lft (transform-output-action (syntax fst)
                                                                     pvars
                                                                     dotted-vars)]
                                   [exp-rgt (expand-lst (syntax rst))])
                       (syntax (cons exp-lft exp-rgt)))]))
                (define (member-var? var lst)
                  (let iter ([lst lst])
                    (if (null? lst)
                        #f
                        (if (bound-identifier=? var (car lst))
                            #t
                            (iter (cdr lst))))))
                (define (dotted-var? var)
                  (member-var? var dotted-vars))
                (define (merge-pvars lst1 lst2)
                  (if (null? lst1)
                      lst2
                      (if (member-var? (car lst1) lst2)
                          (merge-pvars (cdr lst1) lst2)
                          (cons (car lst1) (merge-pvars (cdr lst1) lst2)))))
                (define (select-dotted-vars x)
                  (define (walk-quasi-body y)
                    (syntax-case y (unquote unquote-splicing)
                      [((unquote a) . rst)
                       (merge-pvars (select-dotted-vars (syntax a))
                                    (walk-quasi-body (syntax rst)))]
                      [((unquote-splicing a) . rst)
                       (merge-pvars (select-dotted-vars (syntax a))
                                    (walk-quasi-body (syntax rst)))]
                      [(fst . rst)
                       (merge-pvars (walk-quasi-body (syntax fst))
                                    (walk-quasi-body (syntax rst)))]
                      [other
                       '()]))
                  (syntax-case x (quote quasiquote)
                    [(quote . rst) '()]
                    [(quasiquote . rst) (walk-quasi-body (syntax rst))]
                    [(fst . rst)
                     (merge-pvars (select-dotted-vars (syntax fst))
                                  (select-dotted-vars (syntax rst)))]
                    [item
                     (and (identifier? (syntax item))
                          (dotted-var? (syntax item)))
                     (list (syntax item))]
                    [item '()]))
                (define (expand-dotted-item dots item)
                  (syntax-case item ()
                    [x
                     (and (identifier? (syntax x)) (member-var? (syntax x) dotted-vars))
                     (syntax x)]
                    [x (let ([dvars (select-dotted-vars item)])
                         (if (null? dvars)
                             (xml-match-syntax-error "improper use of ellipsis in template" dots))
                         (with-syntax ([(dv ...) dvars])
                           (syntax (map (lambda (dv ...) x) dv ...))))]))
                (define (expand-quasiquote-body x)
                  (syntax-case x (unquote unquote-splicing quasiquote)
                    [(quasiquote . rst) (process-quasiquote x)]
                    [(unquote item)
                     (with-syntax ([expanded-item (transform-output-action (syntax item)
                                                                           pvars
                                                                           dotted-vars)])
                       (syntax (unquote expanded-item)))]
                    [(unquote-splicing item)
                     (with-syntax ([expanded-item (transform-output-action (syntax item)
                                                                           pvars
                                                                           dotted-vars)])
                       (syntax (unquote-splicing expanded-item)))]
                    [((unquote item) dots . rst)
                     (ellipsis? (syntax dots))
                     (with-syntax ([expanded-item (expand-dotted-item (syntax dots) 
                                                                      (transform-output-action (syntax item)
                                                                                               pvars
                                                                                               dotted-vars))]
                                   [expanded-rst (expand-quasiquote-body (syntax rst))])
                       (syntax ((unquote-splicing expanded-item) . expanded-rst)))]
                    [(item dots . rst)
                     (ellipsis? (syntax dots))
                     (with-syntax ([expanded-item (expand-dotted-item (syntax dots) 
                                                                      (transform-output-action (syntax (quasiquote item))
                                                                                               pvars
                                                                                               dotted-vars))]
                                   [expanded-rst (expand-quasiquote-body (syntax rst))])
                       (syntax ((unquote-splicing expanded-item) . expanded-rst)))]
                    [(fst . rst)
                     (with-syntax ([expanded-fst (expand-quasiquote-body (syntax fst))]
                                   [expanded-rst (expand-quasiquote-body (syntax rst))])
                       (syntax (expanded-fst . expanded-rst)))]
                    [other x]))
                (define (process-quasiquote x)
                  (syntax-case x ()
                    [(quasiquote term) (with-syntax ([expanded-body (expand-quasiquote-body (syntax term))])
                                         (syntax (quasiquote expanded-body)))]
                    [else (xml-match-syntax-error "bad quasiquote-form" x)]))
                (syntax-case action (quote quasiquote xml-match xml-match1
                                           xml-match-let xml-match-let1 xml-match-let*)
                  [(quote . rst) action]
                  [(quasiquote . rst) (process-quasiquote action)]
                  [(xml-match . rst) (with-syntax ([src-term action]
                                                   [pvars-stx pvars]
                                                   [dvars-stx dotted-vars])
                                       (syntax (xml-match/annotated pvars-stx dvars-stx src-term . rst)))]
                  [(xml-match1 . rst) (with-syntax ([pvars-stx pvars]
                                                    [dvars-stx dotted-vars])
                                        (syntax (xml-match1/annotated pvars-stx dvars-stx . rst)))]
                  [(xml-match-let . rst) (with-syntax ([src-term action]
                                                       [pvars-stx pvars]
                                                       [dvars-stx dotted-vars])
                                           (syntax (xml-match-let/annotated pvars-stx dvars-stx src-term . rst)))]
                  [(xml-match-let1 . rst) (with-syntax ([pvars-stx pvars]
                                                        [dvars-stx dotted-vars])
                                            (syntax (xml-match-let1/annotated pvars-stx dvars-stx . rst)))]
                  [(xml-match-let* . rst) (with-syntax ([src-term action]
                                                        [pvars-stx pvars]
                                                        [dvars-stx dotted-vars])
                                            (syntax (xml-match-let*/annotated pvars-stx dvars-stx src-term . rst)))]
                  [(fst . rst) (if (finite-lst? action)
                                   (with-syntax ([exp-lft (transform-output-action (syntax fst) pvars dotted-vars)]
                                                 [exp-rgt (transform-output-action (syntax rst) pvars dotted-vars)])
                                     (syntax (exp-lft . exp-rgt)))
                                   (with-syntax ([exp-lft (transform-output-action (syntax fst)
                                                                                   pvars dotted-vars)]
                                                 [exp-rgt (expand-lst (syntax rst))])
                                     (syntax (apply exp-lft exp-rgt))))]
                  [item action]))])
          (transform-output-action action pvars dotted-vars)))
      (syntax-case stx ()
        [(with-match-vars src-term pvars dvars action)
         (transform-output-term (syntax src-term)
                                (syntax action)
                                (syntax->list (syntax pvars))
                                (syntax->list (syntax dvars)))])))
  
  (define-syntax xml-match1/annotated
    (syntax-rules ()
      [(xml-match1/annotated pvars dvars synform exp cata-fun clause)
       (compile-term (clause exp (lambda () (error 'xml-match "no matching clause found")))
                     ()
                     dvars
                     cata-fun
                     synform)]
      [(xml-match1/annotated pvars dvars synform exp cata-fun clause0 clause ...)
       (let/ec escape
         (compile-term (clause0 exp (lambda () (escape (xml-match1 synform exp cata-fun clause ...))))
                       ()
                       dvars
                       cata-fun
                       synform))]))
  
  (define-syntax xml-match/annotated
    (lambda (stx)
      (syntax-case stx ()
        ((xml-match/annotated pvars dvars synform val clause0 clause ...)
         (syntax (letrec ([cfun (lambda (exp)
                                    (xml-match1/annotated () dvars synform exp cfun clause0 clause ...))])
                     (cfun (with-match-vars synform () dvars val))))))))
  
  (define-syntax xml-match-let1/annotated
    (syntax-rules ()
      [(xml-match-let1/annotated pvars dvars syntag synform () body0 body ...)
       (let ()
         (with-match-vars synform () dvars body0)
         (with-match-vars synform () dvars body) ...)]
      [(xml-match-let1/annotated pvars dvars syntag synform ([pat exp]) body0 body ...)
       (compile-term ((pat (let () body0 body ...))
                      exp
                      (lambda () (error 'syntag "could not match pattern ~s" 'pat)))
                     pvars
                     dvars
                     #f
                     synform)]
      [(xml-match-let1/annotated pvars dvars syntag synform ([pat0 exp0] [pat exp] ...) body0 body ...)
       (compile-term ((pat0 (xml-match-let1 syntag synform ([pat exp] ...) body0 body ...))
                      exp0
                      (lambda () (error 'syntag "could not match pattern ~s" 'pat0)))
                     pvars
                     dvars
                     #f
                     synform)]))
  
  (define-syntax xml-match-let/annotated
    (lambda (stx)
      (syntax-case stx ()
        [(xml-match-let/annotated pvars dvars synform ([pat exp] ...) body0 body ...)
         (with-syntax ([(temp-name ...) (generate-temporaries (syntax (exp ...)))])
           (syntax (let ([temp-name (with-match-vars synform () dvars exp)] ...)
                     (xml-match-let1/annotated () dvars xml-match-let synform
                                               ([pat temp-name] ...)
                                               body0 body ...))))])))
  
  (define-syntax xml-match-let*/annotated
    (lambda (stx)
      (syntax-case stx ()
        [(xml-match-let*/annotated pvars dvars synform () body0 body ...)
         (syntax (let ()
                   (with-match-vars synform () dvars body0)
                   (with-match-vars synform () dvars body) ...))]
        [(xml-match-let*/annotated pvars dvars synform ([pat0 exp0] [pat exp] ...) body0 body ...)
         (with-syntax ([temp-name (car (generate-temporaries (syntax (exp0))))])
           (syntax (let ([temp-name (with-match-vars synform () dvars exp0)])
                     (xml-match-let1/annotated () dvars xml-match-let* synform
                                               ([pat0 temp-name])
                                               (xml-match-let* ([pat exp] ...)
                                                 body0 body ...)))))])))
  
  (define-syntax xml-match1
    (syntax-rules ()
      [(xml-match1 synform exp cata-fun clause)
       (compile-term (clause exp
                             (lambda () (error 'xml-match "no matching clause found")))
                     ()
                     ()
                     cata-fun
                     synform)]
      [(xml-match1 synform exp cata-fun clause0 clause ...)
       (let/ec escape
         (compile-term (clause0 exp
                                (lambda () (escape (xml-match1 synform exp cata-fun clause ...))))
                       ()
                       ()
                       cata-fun
                       synform))]))
  
  (define-syntax xml-match
    (lambda (stx)
      (syntax-case stx ()
        ((xml-match val clause0 clause ...)
         (with-syntax ([synform stx])
           (syntax (letrec ([cfun (lambda (exp)
                                    (xml-match1 synform exp cfun clause0 clause ...))])
                     (cfun val))))))))
  
  (define-syntax xml-match-let1
    (syntax-rules ()
      [(xml-match-let1 syntag synform () body0 body ...)
       (let () body0 body ...)]
      [(xml-match-let1 syntag synform ([pat exp]) body0 body ...)
       (compile-term ((pat (let () body0 body ...))
                      exp
                      (lambda () (error 'syntag "could not match pattern ~s" 'pat)))
                     ()
                     ()
                     #f
                     synform)]
      [(xml-match-let1 syntag synform ([pat0 exp0] [pat exp] ...) body0 body ...)
       (compile-term ((pat0 (xml-match-let1 syntag synform ([pat exp] ...) body0 body ...))
                      exp0
                      (lambda () (error 'syntag "could not match pattern ~s" 'pat0)))
                     ()
                     ()
                     #f
                     synform)]))
  
  (define-syntax xml-match-let
    (lambda (stx)
      (syntax-case stx ()
        [(xml-match-let ([pat exp] ...) body0 body ...)
         (with-syntax ([synform stx]
                       [(temp-name ...) (generate-temporaries (syntax (exp ...)))])
           (syntax (let ([temp-name exp] ...)
                     (xml-match-let1 xml-match-let synform
                                     ([pat temp-name] ...)
                                     body0 body ...))))])))
  
  (define-syntax xml-match-let*
    (lambda (stx)
      (syntax-case stx ()
        [(xml-match-let* () body0 body ...)
         (syntax (let () body0 body ...))]
        [(xml-match-let* ([pat0 exp0] [pat exp] ...) body0 body ...)
         (with-syntax ([synform stx]
                       [temp-name (car (generate-temporaries (syntax (exp0))))])
           (syntax (let ([temp-name exp0])
                     (xml-match-let1 xml-match-let* synform ([pat0 temp-name])
                                     (xml-match-let* ([pat exp] ...)
                                       body0 body ...)))))])))
  
  )

