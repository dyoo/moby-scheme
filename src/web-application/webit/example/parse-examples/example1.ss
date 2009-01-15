(require "../../xml.ss"
         "../../html.ss"
         (lib "ssax.ss" "ssax"))

(define record->html
  (stylesheet->expander
   (stylesheet
     
     (xml-macro album
       (lambda (x)
         (xml-match x
           ((album title: ,name (catalog (num ,n) (fmt ,f)) ...)
            (h4:ul (h4:li name)
                   (h4:li (h4:b n) " "(h4:i f)) ...))))))))

(write-xml (record->html (xml-document-content (ssax:xml->sxml (open-input-file "records.xml") '()))))


