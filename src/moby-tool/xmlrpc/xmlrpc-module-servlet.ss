(module xmlrpc-module-servlet mzscheme
  
  (require (lib "none.ss" "web-server" "managers")
           (file "server-core.ss"))
  
  (provide 
   ;; We must provide these as a module-based servlet
   interface-version 
   manager
   timeout
   start
   ;;; Additional bits for the XML-RPC programmer
   add-handler
   )
  
  ;; This is some magic from Jay McCarthy suggested to force
  ;; the new continuation manager and interface to the webserver
  (define manager (create-none-manager
                   (lambda (failed-request)
                     (make-handler-fault 
                      "Failed request in the continuation manager."
                      999)
                     ))) 
  
  (define start handle-xmlrpc-servlet-request*)
  (define interface-version 'v2)
  (define timeout 10)
  )
  
  
  
  
  