(require "../../xml.ss"
         "../../xsl-fo.ss"
         (lib "ssax.ss" "ssax"))

(define poem->xslfo
  (stylesheet->expander
   (stylesheet
     
     (xml-micro *element*
       (lambda (i) '()))
     
     (xml-micro collection
       (lambda (x)
         (xml-match x
           ((collection ,item ...)
            (xsl-fo 
             (fo:root
              (fo:layout-master-set
               (fo:simple-page-master
                fo:master-name: "page"
                fo:page-width: "8.5"
                fo:page-height: "11"
                (fo:region-body
                 fo:region-name: "body"
                 fo:margin-top: "0.5in"
                 fo:margin-bottom: "0.5in"
                 fo:margin-left: "1.0in"
                 fo:margin-right: "0.5in")))
              (fo:page-sequence
               fo:master-reference: "page"
               (fo:static-content
                fo:flow-name: "xsl-footnote-separator"
                (fo:block
                 fo:start-indent: "4pc"
                 fo:end-indent: "4pc"
                 fo:space-after: "8pt"
                 fo:text-align-last: "justify"
                 (fo:leader fo:leader-pattern: "rule")
                 (fo:leader fo:leader-length: "1pc")
                 (fo:character fo:character: ".")
                 (fo:leader fo:leader-length: "1pc")
                 (fo:leader fo:leader-pattern: "rule")))
               (fo:flow fo:flow-name: "body" (map xml-expand (list item ...))))))))))
     
     (xml-macro poem
       (lambda (x)
         (xml-match x
           ((poem ,item ...)
            (list item ...)))))
     
     (xml-micro title
       (lambda (x)
         (xml-match x
           ((title ,t)
            (fo:block fo:space-before: "1pc"
                      fo:font-size: "14pt"
                      fo:font-family: "Georgia, serif"
                      fo:font-weight: "bold"
                      t)))))
     
     (xml-micro stanza
       (lambda (x)
         (xml-match x
           ((stanza (line ,t) ...)
            (fo:block fo:font-size: "14pt"
                      fo:font-family: "Book Antiqua, sans serif"
                      fo:space-before: "6pt"
                      fo:keep-together: "always"
                      (fo:block t) ...)))))
     
     (xml-micro definitions
       (lambda (x)
         (xml-match x
           ((definitions (def (phrase . ,tp) (translation . ,tt)) ...)
            (fo:block fo:space-before: "20pt"
                      (fo:block fo:font-size: "80%" tp ": " tt) ...)))))
     
     )))

(write-xml (poem->xslfo (xml-document-content (ssax:xml->sxml (open-input-file "epistle.xml") '()))))

