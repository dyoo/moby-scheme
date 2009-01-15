(require "../../xml.ss"
         "../../html.ss"
         (lib "ssax.ss" "ssax"))

(define soccer->html
  (stylesheet->expander
   (stylesheet
     
     (xml-macro results
       (lambda (x)
         (xml-match x
           ((results group: ,grp ,item ...)
            (h4:html (h4:head (h4:title "Results of Group " grp))
                     (h4:body (h4:h1 "Results of Group " grp)
                              item ...))))))
     
     (xml-macro match
       (lambda (x)
         (xml-match x
           ((match (date ,d) (team score: ,s1 ,t1) (team score: ,s2 ,t2))
            (list (h4:h2 t1 " versus " t2)
                  (h4:p "Played on " d)
                  (h4:p "Result: " t1 " " s1 ", " t2 " " s2))))))
     
     )))

(write-xml (soccer->html (xml-document-content (ssax:xml->sxml (open-input-file "soccer.xml") '()))))




