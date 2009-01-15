; test suite for WebIt!
; 2005-01-27: Created initial version as a preliminary set of tests for the new WebIt! using SXML

(module tests mzscheme
  
  (require (lib "pretty.ss"))
  
  (define-syntax run-test
    (syntax-rules ()
      ((run-test desc test expected-result)
       (let ((expected expected-result))
         (printf "~nRunning test ~s:~n" desc)
         (pretty-print (quote test))
         (let ((actual test))
           (if (equal? actual expected)
               (begin (printf "gave the expected result: ")
                      (pretty-print actual))
               (write (format "Unexpected result: ~s~nexpected ~s~n" actual expected) (current-error-port))))))))
  
  ;; sample tests
  ;
  ;(run-test "toy test" (+ 2 1) 3)
  ;
  ;(run-test "toy test" (+ 2 2) 3)
  
  ; start of WebIt! test suite
  
  (require "../xml.ss")
  
  (define-element a)
  (define-element (b a-url))
  (define-element (z b-url))
  
  (define-attribute c)
  (define-attribute (d a-url))
  (define-attribute e)
  (define-attribute (f b-url))
  
  (run-test "xml-element? predicate: apply to non-element (text)"
            (xml-element? "abc") #f)
  
  (run-test "xml-element? predicate: apply to non-element (xml-comment)"
            (xml-element? (make-xml-comment "comment test")) #f)
  
  (run-test "xml-element? predicate: apply to a simple, constructed element"
            (xml-element? (a "abc")) #t)
  
  (run-test "test of element-constructor: no children, no-attributes, no ns-list"
            (a) '(a))
  
  (run-test "test of element-constructor: no-attributes, no ns-list"
            (a "text") '(a "text"))
  
  (run-test "test of element-constructor: attributes, no ns-list"
            (a c: "123" e: "456" "text") '(a (@ (c "123") (e "456")) "text"))
  
  (run-test "test of element-constructor (with namespaces): attributes, no ns-list"
            (b d: "123" f: "456" "text") '(a-url:b (@ (a-url:d "123") (b-url:f "456")) "text"))
  
  (run-test "test of element-constructor (with namespaces): attributes, ns-list"
            (bind-namespaces ((ap "a-url")
                              (bp "b-url"))
                             (b d: "123" f: "456" "text"))
            '(a-url:b (@ (@ (*NAMESPACES* (ap "a-url") (bp "b-url"))) (a-url:d "123") (b-url:f "456")) "text"))
  
  (run-test "test #f as an attribute value; should not create attribute"
            (a d: #f "text") '(a "text"))
  
  ; test tag-related accessors
  
  (run-test "test xml-element-tag (local name)"
            (xml-element-tag (a)) 'a)
  
  (run-test "test xml-element-print-tag (local name)"
            (xml-element-print-tag (a)) 'a)
  
  (run-test "test xml-element-target-ns (local name)"
            (xml-element-target-ns (a)) #f)
  
  (run-test "test xml-element-tag (qname)"
            (xml-element-tag (b)) 'a-url:b)
  
  (run-test "test xml-element-print-tag (qname)"
            (xml-element-print-tag (b)) 'b)
  
  (run-test "test xml-element-target-ns (qname)"
            (xml-element-target-ns (b)) 'a-url)
  
  ; test use of accessors with non-normalized SXML
  
  (define vv
    '(e "abc"
        (@ (a 1) (@ (*NAMESPACES* (r "url1") (s "url2")) (*other* "text")) (b 2))
        "cde"
        (@ (@ (*NAMESPACES* (t "url3") (u "url4"))))
        "fgh"
        (@ (c 3))))
  
  (define xx
    '(e "abc"
        (@ (a 1) (@ (*NAMESPACES* (r "url1") (s "url2"))) (b 2))
        "cde"
        (@ )
        "fgh"
        (@ (c 3))))
  
  (run-test "test xml-element-attributes against non-normalized SXML (1)"
            (xml-element-attributes vv)
            '((a 1) (b 2) (c 3)))
  
  (run-test "test xml-element-attributes against non-normalized SXML (2)"
            (xml-element-attributes xx)
            '((a 1) (b 2) (c 3)))
  
  (run-test "test xml-element-ns-list against non-normalized SXML (1)"
            (xml-element-ns-list vv)
            '((r "url1") (s "url2") (t "url3") (u "url4")))
  
  (run-test "test xml-element-ns-list against non-normalized SXML (2)"
            (xml-element-ns-list xx)
            '((r "url1") (s "url2")))
  
  (run-test "test xml-element-contents against non-normalized SXML (1)"
            (xml-element-contents vv)
            '("abc" "cde" "fgh"))
  
  (run-test "test xml-element-contents against non-normalized SXML (2)"
            (xml-element-contents xx)
            '("abc" "cde" "fgh"))
  
  ; test list structure sharing
  
  (define yy-attrs '((a 1) (b 2) (c 3)))
  (define yy-ns-list '((r "url1") (s "url2")))
  (define yy-contents '((a) "bcd" (e)))
  
  (run-test "test list-structure sharing with xml-element-attributes (1)"
            (eq? (xml-element-attributes `(e (@ . ,yy-attrs) . ,yy-contents)) yy-attrs) #t)
  
  (run-test "test list-structure sharing with xml-element-attributes (2)"
            (eq? (xml-element-attributes `(e (@ (@ (*NAMESPACES* . ,yy-ns-list)) . ,yy-attrs) . ,yy-contents))
                 yy-attrs)
            #t)
  
  (run-test "test list-structure sharing with xml-element-ns-list"
            (eq? (xml-element-ns-list `(e (@ (@ (*NAMESPACES* . ,yy-ns-list)) . ,yy-attrs) . ,yy-contents))
                 yy-ns-list)
            #t)
  
  (run-test "test list-structure sharing with xml-element-contents"
            (eq? (xml-element-contents `(e (@ (@ (*NAMESPACES* . ,yy-ns-list)) . ,yy-attrs) . ,yy-contents))
                 yy-contents)
            #t)
  
  ; other accessor tests
  
  (run-test "test xml-element-ns-list: element with attributes, but no ns-list"
            (xml-element-ns-list `(e (@ . ,yy-attrs) . ,yy-contents)) '())
  
  (run-test "test xml-element-attributes on empty attributes and ns-list structure"
            (xml-element-attributes '(a (@ (@)) "text"))
            '())
  
  (run-test "test xml-element-ns-list on empty attributes and ns-list structure"
            (xml-element-ns-list '(a (@ (@)) "text"))
            '())
  
  (run-test "test an generated attribute accessor/test: attribute present"
            (&d? '(a-url:b (@ (@ (*NAMESPACES* (ap "a-url") (bp "b-url"))) (a-url:d "123") (b-url:f "456")) "text"))
            "123")
  
  (run-test "test an generated attribute accessor/test: attribute not present"
            (&d? '(a-url:b (@ (@ (*NAMESPACES* (ap "a-url") (bp "b-url"))) (a-url:zz "123") (b-url:f "456")) "text"))
            #f)
  
  ;; more here. Add tests to verify correct behaviour for lower-case non-XML tags (e.g. *top*, *namespaces*, etc.)
  
  
  ;; more here. Add tests to verify the xml-document accessors
  
  )

(require tests)
