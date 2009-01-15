; svg-tool.ss

#cs
(module svg-tool mzscheme
  
  (require "xml.ss")
  
  (define svg-dtd-info 
    (make-xml-dtd-info/public 'svg 
                              "-//W3C//DTD SVG 20001102//EN"
                              "http://www.w3.org/TR/2000/CR-SVG-20001102/DTD/svg-20001102.dtd"))
  
  (define write-svg write-xml)
  
  ;  (define (write-svg something . opt-port)
  ;    (let ((out-port (:optional opt-port (current-output-port))))
  ;      (write-xml 
  ;       (xml (make-svg 
  ;             (make-external-dtd/system 'SVG
  ;                                       "-//W3C//DTD SVG 20001102//EN"
  ;                                       "http://www.w3.org/TR/2000/CR-SVG-20001102/DTD/svg-20001102.dtd")
  ;             something)))))
  
  (provide svg-dtd-info
           write-svg))

