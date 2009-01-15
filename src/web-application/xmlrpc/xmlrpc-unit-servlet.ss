(module xmlrpc-unit-servlet mzscheme
  
  (require 
   (file "server-core.ss")
   (lib "unitsig.ss")
   (lib "sig.ss" "web-server"))
  
  (provide (all-from (lib "unitsig.ss"))
           (all-from (lib "sig.ss" "web-server"))
           add-handler
           handle-xmlrpc-requests)
  
  ;; SYNTAX: handle-xmlrpc-requests
  ;; Expands to the servlet^ unit/sig that handles incoming 
  ;; XML-RPC requests. Pushes down the necessity of 
  ;; passing in the initial-request.
  (define-syntax handle-xmlrpc-requests 
    (lambda (stx)
      (syntax-case stx ()
        [(_)
         #`(unit/sig ()
             (import servlet^)
             (handle-xmlrpc-servlet-request* initial-request))])))
  
  

  
  )
  
  
  
  
  