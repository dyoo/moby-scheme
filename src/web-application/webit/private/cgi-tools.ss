; cgi-tools.ss

(module cgi-tools mzscheme

  (require "xml-core.ss")
  
  (provide servlet-result)
  
  (define (servlet-result item)
    (define out (open-output-string))
    (write-xml item out)
    (list "text/html"
          (get-output-string out)))
  
  )


