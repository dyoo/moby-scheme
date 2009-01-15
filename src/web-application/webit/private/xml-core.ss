;; xml-core.ss -- rs-xml framework

#cs
(module xml-core mzscheme
  
  (require (lib "etc.ss")
           "keyword.ss")
  
  (require (rename (lib "fold.ss" "srfi" "1") fold-right fold-right)
           (rename (lib "filter.ss" "srfi" "1") filter filter))
  
  (provide 
   
   (struct dsssl-keyword (tag print-tag ns-uri))
   declare-keyword
   
   nodeset?
   
   normalized-sxml
   
   make-xml-document
   xml-document?
   xml-document-dtd-info 
   xml-document-content 
   xml-document-body
   
   make-xml-dtd-info
   xml-dtd-info?
   xml-dtd-info-name
   xml-dtd-info-system
   make-xml-dtd-info/public
   xml-dtd-info/public?
   xml-dtd-info/public-public
   
   make-xml-comment
   xml-comment?
   xml-comment-text
   
   make-xml-pi
   xml-pi?
   xml-pi-target
   xml-pi-text
   
   make-xml-entity
   xml-entity?
   xml-entity-public-id
   xml-entity-system-id
   
   make-entity-ref
   entity-ref?
   
   make-pcdata
   pcdata?
   pcdata->string
   string->pcdata
   
   make-xml-element
   make-xml-element/ns
   xml-element?
   xml-element-tag
   xml-element-local-name
   xml-element-ns-uri
   xml-element-ns-list
   xml-element-attributes
   xml-element-contents
   
   xml-element-print-tag
   xml-element-target-ns
   
   make-xml-attribute
   xml-attribute-tag
   xml-attribute-local-name
   xml-attribute-ns-uri
   xml-attribute-value
   
   xml-attribute-print-tag
   xml-attribute-target-ns
   
   make-xml-ns-binding
   xml-ns-binding?
   xml-ns-binding-prefix
   xml-ns-binding-ns-url
   
   make-import-clause
   import-clause?
   import-clause-url
   
   make-css-rule
   css-rule?
   css-rule-selector
   css-rule-avp-list
   
   make-avp
   avp?
   avp-name
   avp-value
   
   make-em-avp
   em-avp?
   
   make-path-selector
   path-selector?
   path-selector-steps
   
   make-css-node
   css-node?
   css-node-decl-list
   
   bind-namespaces
   
   xml-type
   write-xml
   display-xml
   
   match-xml-attribute
   xml-identifier-concat
   xml-print-name/node
   xml-print-name/attr
   xml-element-function-back-end
   raw-xml-element-function-back-end
   )
  
  (define normalized-sxml (make-parameter #f))
  
  (define (nodeset? x)
    (or (and (pair? x) (not (symbol? (car x)))) (null? x)))
  
  ; from Oleg Kiselyov's util.scm (in the SSAX distribution)
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
  
  ;(define-struct xml-document
  ;               (body)
  ;               (make-inspector))
  (define (make-xml-document body)
    (cons '*TOP* body))
  (define (xml-document? node)
    (and (pair? node) (or (eq? (car node) '*TOP*) (eq? (car node) '*top*))))
  (define (xml-document-body node)
    (if (xml-document? node)
        (cdr node)
        (error "xml-document-body: expected an xml-document, given" node)))
  
  (define (xml-document-content doc)
    (let loop ((body (xml-document-body doc)))
      (if (null? body)
          (error "struct xml-document must contain a single XML element; no element found")
          (if (xml-element? (car body))
              (car body)
              (loop (cdr body))))))
  
  (define (xml-document-dtd-info doc)
    (let loop ((body (xml-document-body doc)))
      (if (null? body)
          (error "struct xml-document missing dtd-info node")
          (if (xml-dtd-info? (car body))
              (car body)
              (loop (cdr body))))))
  
  ;(define-struct xml-doc-type-base (name) (make-inspector))
  ;(define-struct (xml-dtd-info xml-doc-type-base) (system) (make-inspector))
  ;(define-struct (xml-dtd-info/public xml-dtd-info) (public) (make-inspector))
  ;(define xml-dtd-info-name xml-doc-type-base-name)
  ;(define set-xml-dtd-info-name! set-xml-doc-type-base-name!)
  (define (make-xml-dtd-info name system)
    (list '*DTD-INFO* name system))
  (define (xml-dtd-info? node)
    (and (pair? node) (or (eq? (car node) '*DTD-INFO*) (eq? (car node) '*DTD-INFO/PUBLIC*)
                          (eq? (car node) '*dtd-info*) (eq? (car node) '*dtd-info/public*))))
  (define (xml-dtd-info-name node)
    (if (xml-dtd-info? node)
        (cadr node)
        (error "xml-dtd-info-name: expected an xml-dtd-info, given" node)))
  (define (xml-dtd-info-system node)
    (if (xml-dtd-info? node)
        (caddr node)
        (error "xml-dtd-info-system: expected an xml-dtd-info, given" node)))
  (define (make-xml-dtd-info/public name system public)
    (list '*DTD-INFO/PUBLIC* name system public))
  (define (xml-dtd-info/public? node)
    (and (pair? node) (or (eq? (car node) '*DTD-INFO/PUBLIC*) (eq? (car node) '*dtd-info/public*))))
  (define (xml-dtd-info/public-public node)
    (if (xml-dtd-info/public? node)
        (cadddr node)
        (error "xml-dtd-info/public-public: expected an xml-dtd-info, given" node)))
  
  ;(define-struct xml-comment
  ;               (text)
  ;               (make-inspector))
  (define (make-xml-comment . rest)
    (cons '*COMMENT* rest))
  (define (xml-comment? node)
    (and (pair? node) (or (eq? (car node) '*COMMENT*) (eq? (car node) '*comment*))))
  (define (xml-comment-text node)
    (if (xml-comment? node)
        (cdr node)
        (error "xml-comment-text: expected an xml-comment, given" node)))
  
  ;(define-struct xml-pi (target text))
  (define (make-xml-pi target text)
    (list '*PI* target text))
  (define (xml-pi? node)
    (and (pair? node) (or (eq? (car node) '*PI*) (eq? (car node) '*pi*))))
  (define (xml-pi-target node)
    (if (xml-pi? node)
        (cadr node)
        (error "xml-pi-target: expected an xml-pi, given" node)))
  (define (xml-pi-text node)
    (if (xml-pi? node)
        (caddr node)
        (error "xml-pi-text: expected an xml-pi, given" node)))
  
  ;(define-struct xml-entity (public-id system-id) (make-inspector))
  (define (make-xml-entity public-id system-id)
    (list '*ENTITY* public-id system-id))
  (define (xml-entity? node)
    (and (pair? node) (or (eq? (car node) '*ENTITY*) (eq? (car node) '*entity*))))
  (define (xml-entity-public-id node)
    (if (xml-entity? node)
        (cadr node)
        (error "xml-entity-public-id: expected an xml-entity, given" node)))
  (define (xml-entity-system-id node)
    (if (xml-entity? node)
        (caddr node)
        (error "xml-entity-system-id: expected an xml-entity, given" node)))
  
  (define (make-entity-ref name)
    (list '& name))
  (define (entity-ref? node)
    (and (pair? node) (eq? '& (car node))))
  
  (define make-xml-element
    (case-lambda
      [(tag print-tag target-ns attributes contents)
       (make-xml-element tag attributes contents)]
      [(tag attributes contents)
       (cons tag
             (if (null? attributes)
                 contents
                 (cons (cons '@ attributes)
                       contents)))]))
  
  (define make-xml-element/ns
    (case-lambda
      [(tag print-tag target-ns ns-list attributes contents)
       (make-xml-element/ns tag ns-list attributes contents)]
      [(tag ns-list attributes contents)
       (cons tag
             (if (and (null? ns-list) (null? attributes))
                 contents
                 (cons (cons '@
                             (if (null? ns-list)
                                 attributes
                                 (cons (list '@ (cons '*NAMESPACES* ns-list)) attributes)))
                       contents)))]))
  
  (define (xml-element? s)
    (and (pair? s) (symbol? (car s))
         (case (car s)
           ((@ & *TOP* *PI* *COMMENT* *ENTITY* *NAMESPACES*
               *DTD-INFO* *DTD-INFO/PUBLIC*
               *top* *pi* *comment* *entity* *namespaces*
               *dtd-info* *dtd-info/public*
               *import* *css-rule* *avp* *em-avp* *path-selector* *css-node*)
            #f)
           (else #t))))
  
  (define (xml-element-tag node)
    (if (xml-element? node)
        (car node)
        (error "xml-element-tag: expected an xml-element, given" node)))
  
  (define (xml-element-local-name node)
    (if (xml-element? node)
        (let-values (((ns qname) (split-tag (car node))))
          qname)
        (error "xml-element-local-name: expected an xml-element, given" node)))
  
  (define (xml-element-ns-uri node)
    (if (xml-element? node)
        (let-values (((ns qname) (split-tag (car node))))
          ns)
        (error "xml-element-ns-uri: expected an xml-element, given" node)))
  
  (define xml-element-print-tag xml-element-local-name)
  (define xml-element-target-ns xml-element-ns-uri)
  
  (define (xml-element-ns-list s)
    (if (xml-element? s)
        (if (normalized-sxml)
            (xml-element-ns-list/normalized s)
            (fold-right (lambda (a b)
                          (if (and (pair? a) (eq? '@ (car a)))
                              (fold-right (lambda (c d)
                                            (if (and (pair? c) (eq? '@ (car c)))
                                                (fold-right (lambda (e f)
                                                              (if (and (pair? e)
                                                                       (or (eq? '*NAMESPACES* (car e))
                                                                           (eq? '*namespaces* (car e))))
                                                                  (if (null? f)
                                                                      (cdr e)
                                                                      (append (cdr e) f))
                                                                  f))
                                                            d (cdr c))
                                                d))
                                          b (cdr a))
                              b))
                        '()
                        (cdr s)))
        (error "xml-element-ns-list: expected an xml-element, given" s)))
  
  (define (xml-element-attributes s)
    (if (xml-element? s)
        (if (normalized-sxml)
            (xml-element-attributes/normalized s)
            (fold-right (lambda (a b)
                          (if (and (pair? a) (eq? '@ (car a)))
                              (if (null? b)
                                  (filter (lambda (i) (not (and (pair? i) (eq? '@ (car i))))) (cdr a))
                                  (fold-right (lambda (c d)
                                                (if (and (pair? c) (eq? '@ (car c)))
                                                    d
                                                    (cons c d)))
                                              b (cdr a)))
                              b))
                        '()
                        (cdr s)))
        (error "xml-element-attributes: expected an xml-element, given" s)))
  
  (define (xml-element-contents s)
    (if (xml-element? s)
        (if (normalized-sxml)
            (xml-element-contents/normalized s)
            (filter (lambda (i)
                      (not (and (pair? i) (eq? '@ (car i)))))
                    (cdr s)))
        (error "xml-element-contents: expected an xml-element, given" s)))
  
  (define (xml-element-ns-list/normalized s)
    (if (and (pair? (cdr s))
             (pair? (cadr s))
             (eq? '@ (car (cadr s))))
        (let ((attrs+annot (cdadr s)))
          (if (and (pair? (car attrs+annot))
                   (eq? '@ (caar attrs+annot)))
              (let ((annots (cdar attrs+annot)))
                (if (and (pair? (car annots))
                         (or (eq? '*NAMESPACES* (caar annots))
                             (eq? '*namespaces* (caar annots))))
                    (cdar annots)
                    '()))
              '()))
        '()))
  
  (define (xml-element-attributes/normalized s)
    (if (and (pair? (cdr s))
             (pair? (cadr s))
             (eq? '@ (car (cadr s))))
        (let ((attrs+annot (cdadr s)))
          (if (and (pair? (car attrs+annot))
                   (eq? '@ (caar attrs+annot)))
              (cdr attrs+annot)
              attrs+annot))
        '()))
  
  (define (xml-element-contents/normalized s)
    (if (and (pair? (cdr s))
             (pair? (cadr s))
             (eq? '@ (car (cadr s))))
        (cddr s)
        (cdr s)))
  
  ;  (define-struct xml-element (tag 
  ;                              print-tag 
  ;                              target-ns 
  ;                              attributes 
  ;                              contents)
  ;                 (make-inspector))
  ;  
  ;  (define-struct (xml-element+nslist xml-element)
  ;                 (ns-list)
  ;                 (make-inspector))
  ;  
  ;  (define (make-xml-element/ns tag 
  ;                               print-tag 
  ;                               target-ns 
  ;                               ns-list 
  ;                               attributes 
  ;                               contents)
  ;    (make-xml-element+nslist tag 
  ;                             print-tag 
  ;                             target-ns
  ;                             attributes 
  ;                             contents 
  ;                             ns-list))
  ;  
  ;  (define xml-element/ns? xml-element+nslist?)
  ;  
  ;  (define (xml-element-ns-list ele)
  ;    (if (xml-element+nslist? ele)
  ;        (xml-element+nslist-ns-list ele)
  ;        '()))
  
  ;(define-struct xml-attribute (tag print-tag target-ns value)
  ;               (make-inspector))
  
  (define make-xml-attribute
    (case-lambda
      [(tag print-tag target-ns value)
       (make-xml-attribute tag value)]
      [(tag value)
       (list tag value)]))
  ;(define (xml-attribute? node)
  ;  (and (pair? node) (symbol? (car node))))
  (define (xml-attribute-tag node)
    (if (and (pair? node) (symbol? (car node)))
        (car node)
        (error "xml-attribute-tag: expected an xml-attribute, given" node)))
  (define (xml-attribute-local-name node)
    (if (and (pair? node) (symbol? (car node)))
        (let-values (((ns qname) (split-tag (car node))))
          qname)
        (error "xml-attribute-local-name: expected an xml-attribute, given" node)))
  (define (xml-attribute-ns-uri node)
    (if (and (pair? node) (symbol? (car node)))
        (let-values (((ns qname) (split-tag (car node))))
          ns)
        (error "xml-attribute-ns-uri: expected an xml-attribute, given" node)))
  (define xml-attribute-print-tag xml-attribute-local-name)
  (define xml-attribute-target-ns xml-attribute-ns-uri)
  (define (xml-attribute-value node)
    (if (and (pair? node) (symbol? (car node)))
        (cadr node)
        (error "xml-attribute-value: expected an xml-attribute, given" node)))
  
  ;(define-struct xml-ns-binding (prefix ns-url))
  (define (make-xml-ns-binding prefix ns-url)
    (list prefix ns-url))
  (define (xml-ns-binding? node)
    (and (pair? node) (symbol? (car node))))
  (define (xml-ns-binding-prefix node)
    (car node))
  (define (xml-ns-binding-ns-url node)
    (cadr node))
  
  ; included only for backward compatibility
  (define (make-pcdata one two str)
    str)
  
  ; included only for backward compatibility
  (define pcdata? string?)
  
  ; included only for backward compatibility
  (define (string->pcdata str)
    str)
  
  ; included only for backward compatibility
  (define (pcdata->string node)
    node)
  
  ;; css structure defintions
  ;(define-struct import-clause (url))
  (define (make-import-clause url)
    (list '*import* url))
  (define (import-clause? node)
    (and (pair? node) (eq? (car node) '*import*)))
  (define (import-clause-url node)
    (if (import-clause? node)
        (cadr node)
        (error "import-clause-url: expected an import-clause, given" node)))
  
  ;(define-struct css-rule (selector avp-list))
  (define (make-css-rule selector avp-list)
    (let ((sel (if (or (symbol? selector) (path-selector? selector))
                   (list '*selector-list* selector)
                   (cons '*selector-list* selector))))
      (cons '*css-rule* (cons sel avp-list))))
  (define (css-rule? node)
    (and (pair? node) (eq? (car node) '*css-rule*)))
  (define (css-rule-selector node)
    (if (css-rule? node)
        (cdr (cadr node))
        (error "css-rule-selector: expected a css-rule, given" node)))
  (define (css-rule-avp-list node)
    (if (css-rule? node)
        (cddr node)
        (error "css-rule-avp-list: expected a css-rule, given" node)))
  
  ;(define-struct avp (name value))
  ;(define-struct (em-avp avp) ())
  (define (make-avp name value)
    (list '*avp* name value))
  (define (make-em-avp name value)
    (list '*em-avp* name value))
  (define (avp? node)
    (and (pair? node) (or (eq? (car node) '*avp*) (eq? (car node) '*em-avp*))))
  (define (em-avp? node)
    (and (pair? node) (eq? (car node) '*em-avp*)))
  (define (avp-name node)
    (if (avp? node)
        (cadr node)
        (error "avp-name: expected an avp, given" node)))
  (define (avp-value node)
    (if (avp? node)
        (caddr node)
        (error "avp-value: expected an avp, given" node)))
  
  ;(define-struct path-selector (steps))
  (define (make-path-selector steps)
    (cons '*path-selector* steps))
  (define (path-selector? node)
    (and (pair? node) (eq? (car node) '*path-selector*)))
  (define (path-selector-steps node)
    (if (path-selector? node)
        (cdr node)
        (error "path-selector-steps: expected a path-selector, given" node)))
  
  ;(define-struct css-node (decl-list))
  (define (make-css-node decl-list)
    (cons '*css-node* decl-list))
  (define (css-node? node)
    (and (pair? node) (eq? (car node) '*css-node*)))
  (define (css-node-decl-list node)
    (if (css-node? node)
        (cdr node)
        (error "css-node-decl-list: expected a css-node, given" node)))
  
  ; more here. the namespace bindings should be an environment
  ; passed within the write functions, not a parameter!
  (define current-namespaces (make-parameter '((xml xml))))
  ;(define xml-empty-element-flag (make-parameter #t))
  ;(define xml-filter-comments-flag (make-parameter #t))
  
  ; namespace-abbreviation declarations
  
  (define-syntax (bind-namespaces stx)
    (syntax-case stx ()
      ((_ () ele) (syntax ele))
      ((_ ((abbr ns) ...) ele)
       (syntax (let ((ns-list (map (lambda (p)
                                     (make-xml-ns-binding (cadr p)
                                                          (car p)))
                                   (quasiquote (((unquote ns) abbr) ...)))))
                 (let ((new-ele ele))
                   (make-xml-element/ns (xml-element-tag new-ele)
                                        ns-list
                                        (xml-element-attributes new-ele)
                                        (xml-element-contents new-ele))))))))
  
  ; utility functions; not intended to be user visible, but 
  ;    (some) are used by the constructor function libraries
  
  (define (xml-identifier-concat pre tag)
    (if (and pre (not (eq? pre '_)))
        (string->symbol (string-append (symbol->string pre)
                                       ":"
                                       (symbol->string tag)))
        tag))
  
  (define (xml-print-name/node ele)
    (let* ((key (xml-element-target-ns ele))
           (tag (xml-element-print-tag ele))
           (binding (assoc key (current-namespaces))))
      (if binding
          (xml-identifier-concat (cadr binding) tag)
          tag)))
  
  (define (xml-print-name/attr attr)
    (let* ((key (xml-attribute-target-ns attr))
           (tag (xml-attribute-print-tag attr))
           (binding (assoc key (current-namespaces))))
      (if binding
          (xml-identifier-concat (cadr binding) tag)
          tag)))
  
  (define (match-xml-attribute key l)
    (if (not (pair? l))
        #f
        (if (eq? (xml-attribute-tag (car l)) key)
            (car l)
            (match-xml-attribute key (cdr l)))))
  
  (define (flatten-contents c)
    (if (null? c)
        '()
        (if (nodeset? c)
            (let ((first (car c))
                  (rest (cdr c)))
              (if (nodeset? first)
                  (append (flatten-contents first) (flatten-contents rest))
                  (if (null? first)
                      (flatten-contents rest)
                      (cons first (flatten-contents rest)))))
            (list c))))
  
  (define raw-xml-element-function-back-end
    (case-lambda
      [(match-tag print-tag with-ns? ns-list attr-list content-items)
       (raw-xml-element-function-back-end match-tag ns-list attr-list content-items)]
      [(match-tag ns-list attr-list content-items)
       (if (null? ns-list)
           (make-xml-element match-tag
                             attr-list
                             (flatten-contents content-items))
           (make-xml-element/ns match-tag
                                ns-list
                                attr-list
                                (flatten-contents content-items)))]))
  
  (define (xml-type ele)
    (if (xml-element? ele)
        (lambda (attrs . contents)
          (make-xml-element (xml-element-tag ele)
                            attrs
                            (flatten-contents contents)))
        (error "xml-type: expects an xml-element, given " ele)))
  
  (define xml-element-function-back-end
    (case-lambda
      [(match-tag print-tag with-ns? items)
       (xml-element-function-back-end match-tag items)]
      [(match-tag items)
       (define (iter items acc)
         (if (null? items)
             (values (reverse acc) '())
             (if (dsssl-keyword? (car items))
                 (if (cadr items)
                     (iter (cddr items)
                           (cons (make-xml-attribute 
                                  (dsssl-keyword-tag (car items))
                                  (dsssl-keyword-print-tag (car items))
                                  (dsssl-keyword-ns-uri (car items))
                                  (pcdata->string (cadr items)))
                                 acc))
                     (iter (cddr items) acc))
                 (values (reverse acc) items))))
       (let ((flat-items (flatten-contents items)))
         (let-values (((attrs conts) (iter flat-items '())))
           (make-xml-element match-tag attrs conts)))]))
  
  (define (write-xml-helper something out-port xml-display-flag escapes-flag)
    
    (define current-xml-indent (make-parameter 0))
    
    (define fill
      (opt-lambda (nsp [out-port (current-output-port)])
        (if (<= nsp 0)
            (void)
            (begin (display " " out-port)
                   (fill (- nsp 1) out-port)))))
    
    ; css writer
    (define write-css-node 
      (opt-lambda (stylesheet [out-port (current-output-port)])
        (define (write-binding binding)
          (display (avp-name binding) out-port)
          (display ": " out-port)
          (display (avp-value binding) out-port)
          (if (em-avp? binding)
              (display " ! important"))
          (display ";" out-port)
          (newline out-port))
        (define (write-basic-selector sel)
          (display sel out-port))
        (define (write-path-selector sel)
          (if (path-selector? sel)
              (let ((sels (path-selector-steps sel)))
                (write-basic-selector (car sels))
                (for-each (lambda (s)
                            (display " " out-port)
                            (write-path-selector s))
                          (cdr sels)))
              (write-basic-selector sel)))
        (define (write-selector sel)
          (cond
            ((pair? sel) 
             (write-path-selector (car sel))
             (for-each (lambda (s)
                         (display ", " out-port)
                         (write-path-selector s))
                       (cdr sel)))
            (else (write-path-selector sel))))
        (define (write-decl decl)
          (write-selector (css-rule-selector decl))
          (display " {" out-port)
          (newline out-port)
          (map write-binding (css-rule-avp-list decl))
          (display "}" out-port)
          (newline out-port))
        (define (write-import import)
          (display "@import url(" out-port)
          (display (import-clause-url import) out-port)
          (display ")" out-port)
          (newline out-port))
        (define (write-items items)
          (if (null? items)
              (void)
              (let ((i (car items)))
                (if (import-clause? i)
                    (write-import i)
                    (write-decl i))
                (write-items (cdr items)))))
        (write-items (css-node-decl-list stylesheet))))
    
    (define write-xml-document
      (opt-lambda (document [out-port (current-output-port)])
        (let ((body (xml-document-body document)))
          (display "<?xml version=\"1.0\"?>"  out-port)
          (if xml-display-flag
              (newline out-port)
              (void))
          (for-each (lambda (item) (write-xml-node item out-port))
                    body))))
    
    (define write-xml-dtd-info
      (opt-lambda (something [out-port (current-output-port)])
        (display "<!DOCTYPE " out-port)
        (if (xml-dtd-info/public? something)
            (display (format "~a PUBLIC ~s ~s>"
                             (xml-dtd-info-name something)
                             (xml-dtd-info-system something)
                             (xml-dtd-info/public-public something)))
            (display (format "~a SYSTEM ~s>"
                             (xml-dtd-info-name something)
                             (xml-dtd-info-system something))))))
    
    (define write-xml-pi
      (opt-lambda (something [out-port (current-output-port)])
        (display "<?" out-port)
        (display (xml-pi-target something) out-port)
        (display " " out-port)
        (for-each (lambda (i)
                    (display i out-port))
                  (xml-pi-text something))
        (display "?>" out-port)))
    
    (define write-xml-comment
      (opt-lambda (something [out-port (current-output-port)])
        (if xml-display-flag
            (fill (current-xml-indent) out-port)
            (void))
        (display "<!-- " out-port)
        (display (xml-comment-text something) out-port)
        (display " -->" out-port)
        (if xml-display-flag
            (newline out-port)
            (void))))
    
    (define (write-escaping-text e out-port)
      (let ((max (string-length e)))
        (let iter ((i 0))
          (if (< i max)
              (let ((c (string-ref e i)))
                (case c
                  [(#\&) (display "&amp;" out-port)]
                  [(#\<) (display "&lt;" out-port)]
                  [(#\>) (display "&gt;" out-port)]
                  [else (display c out-port)])
                (iter (+ i 1)))
              (void)))))
    
    (define (write-pcdata e out-port)
      (if escapes-flag
          (write-escaping-text e out-port)
          (display e out-port)))
    
    (define write-xml-element
      (opt-lambda (e [out-port (current-output-port)])
        (define (write-complex-value v)
          (display (format "\"~a\"" v) out-port))
        (define (write-attribute attr)
          (display " " out-port)
          (display (xml-print-name/attr attr) out-port)
          (display "=" out-port)
          (write-complex-value (xml-attribute-value attr)))
        (define (write-namespaces ns-list)
          (if (null? ns-list)
              (void)
              (begin (let ((ns (car ns-list)))
                       (display " " out-port)
                       (if (and (xml-ns-binding-prefix ns)
                                (not (eq? (xml-ns-binding-prefix ns) '_)))
                           (display (xml-identifier-concat 'xmlns (xml-ns-binding-prefix ns)) out-port)
                           (display "xmlns" out-port))
                       (display "=" out-port)
                       (write (xml-ns-binding-ns-url ns) out-port)
                       (write-namespaces (cdr ns-list))))))
        (define (write-open-tag ot)
          (display "<" out-port)
          (display (xml-print-name/node ot) out-port)
          (for-each write-attribute (xml-element-attributes ot))
          (write-namespaces (xml-element-ns-list ot))
          (display ">" out-port))
        (define (write-close-tag ct)
          (display "</" out-port)
          (display (xml-print-name/node ct) out-port)
          (display ">" out-port))
        (define (write-empty-element-tag eet)
          (if xml-display-flag
              (fill (current-xml-indent) out-port)
              (void))
          (display "<" out-port)
          (display (xml-print-name/node eet) out-port)
          (for-each write-attribute (xml-element-attributes eet))
          (write-namespaces (xml-element-ns-list eet))
          (display " />" out-port)
          (if xml-display-flag
              (newline out-port)
              (void)))
        (define (empty-element? e)
          (null? (xml-element-contents e)))
        (parameterize ((current-namespaces (append (map (lambda (b)
                                                          (list (string->symbol (xml-ns-binding-ns-url b))
                                                                (xml-ns-binding-prefix b)))
                                                        (xml-element-ns-list e))
                                                   (current-namespaces))))
          (if (empty-element? e)
              (write-empty-element-tag e)
              (begin (if xml-display-flag
                         (fill (current-xml-indent) out-port)
                         (void))
                     (write-open-tag e)
                     (if xml-display-flag
                         (newline out-port)
                         (void))
                     (parameterize ((current-xml-indent (+ (current-xml-indent) 2)))
                       (for-each (lambda (n)
                                   (write-xml-node n out-port))
                                 (xml-element-contents e)))
                     (if xml-display-flag
                         (fill (current-xml-indent) out-port)
                         (void))
                     (write-close-tag e)
                     (if xml-display-flag
                         (newline out-port)
                         (void)))))))
    
    (define (write-xml-node something out-port)
      (cond
        ((css-node? something) (write-css-node  something out-port))
        ((xml-element? something) (write-xml-element something out-port))
        ((xml-document? something) (write-xml-document something out-port))
        ((xml-dtd-info? something) (write-xml-dtd-info something out-port))
        ((xml-pi? something) (write-xml-pi something out-port))
        ((xml-comment? something) (write-xml-comment something out-port))
        ((entity-ref? something) (display "&" out-port)
                                 (display (cadr something) out-port)
                                 (display ";" out-port))
        ((string? something) (write-pcdata something out-port))
        ((number? something) (write something out-port))
        (else (error "write-xml: unknown datatype, given " something))))
    
    (write-xml-node something out-port))
  
  (define display-xml
    (opt-lambda (something [out-port (current-output-port)])
      (write-xml-helper something out-port #t #f)))
  
  (define write-xml
    (case-lambda
      ((something) (write-xml-helper something (current-output-port) #f #f))
      ((something port-or-flag) (if (port? port-or-flag)
                                    (write-xml-helper something port-or-flag #f #f)
                                    (write-xml-helper something (current-output-port) #f port-or-flag)))
      ((something port flag) (write-xml-helper something port #f flag))))
  
  )


