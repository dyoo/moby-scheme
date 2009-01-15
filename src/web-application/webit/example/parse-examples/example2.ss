(require "../../xml.ss"
         (lib "ssax.ss" "ssax"))

(define-element purchase)
(define-element p)
(define-element text)

(define record->html
  (stylesheet->expander
   (stylesheet
     
     (xml-macro purchase
       (lambda (x)
         (xml-match x
           ((purchase (p ,i))
            (text i))
           ((purchase (p ,i) (p ,i2))
            (text i " and " (purchase (p i2))))
           ((purchase (p ,i) ,rst ...)
            (text i ", " (purchase rst ...)))))))))

(write-xml (record->html (xml-document-content (ssax:xml->sxml (open-input-file "purchase.xml") '()))))




