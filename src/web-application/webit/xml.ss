;; xml.ss -- WebIt! XML framework

#cs
(module xml mzscheme
  
  (require (lib "list.ss")
           (lib "etc.ss")
           "private/xml-core.ss"
           "private/types.ss"
           "private/cgi-tools.ss"
           "private/excess.ss"
           "private/quick.ss"
           "private/quote.ss"
           "private/xml-rules.ss"
           "private/xml-match.ss")
  
  (provide
   
   normalized-sxml
   
   ; type definition
   define-element
   define-attribute
   
   ; for "literal" WebIt! XML
   define-namespace
   quote-xml
   quasi-xml
   
   ; visible XML structures
   
   nodeset?
   
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
   
   ; deprecated
   xml-element-print-tag
   xml-element-target-ns
   
   make-xml-attribute
   ;xml-attribute?
   xml-attribute-tag
   xml-attribute-local-name
   xml-attribute-ns-uri
   xml-attribute-value
   
   ; deprecated
   xml-attribute-print-tag
   xml-attribute-target-ns
   
   make-xml-ns-binding
   xml-ns-binding?
   xml-ns-binding-prefix
   xml-ns-binding-ns-url
   
   ; syntax for namespace-abbreviation declaration
   bind-namespaces
   
   ; css structures
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
   
   ; visible function interface
   xml-type
   write-xml
   display-xml
   
   (rename match-xml-attribute has-attribute?)
   
   xml-rules
   xml-case
   stylesheet->expander
   stylesheet
   compound-stylesheet
   
   stylesheet0
   
   servlet-result
   
   xml-match
   xml-match-let
   xml-match-let*
   
   )
  
  )


