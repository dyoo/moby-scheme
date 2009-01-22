(module xmlrpc-cgi mzscheme
  
  (require 
   (planet "xxexpr.ss" ("lshift" "xxexpr.plt" 1))
   (lib "cgi.ss" "net")
   "server-core.ss"
   "protocol.ss")
  
  (provide (all-from (lib "cgi.ss" "net"))
           add-handler
           handle-xmlrpc-requests)
  
  ;; syntax : handle-xmlrpc-requests
  ;; Expands to a simple print statement that
  ;; prints the XML-RPC response from handling the incoming
  ;; request. I think errors are thrown sooner if things go 
  ;; wrong...
  (define-syntax handle-xmlrpc-requests 
    (lambda (stx)
      (syntax-case stx ()
        [(_)
         #`(printf "~a~n~n"
                   (handle-xmlrpc-request* (get-bindings/post)))])))

  ;; cleanup-cgi-request : bindings -> string
  ;; Expand the bindings from the CGI library into
  ;; a string. They get mangled incoming.
  (define (cleanup-cgi-request bindings)
    (let ([request-string
            (car (map (lambda (bpair)
                        (string-append
                         (symbol->string (car bpair))
                         "="
                         (cdr bpair)))
                      bindings))])
      request-string))
 
  ;; handle-xmlrpc-request* : bindings -> (U string error)
  ;; Handles an incoming XML-RPC request, and returns
  ;; a string representing the XML-RPC response.
  (define (handle-xmlrpc-request* bindings)
    (let* ([request-string (cleanup-cgi-request bindings)])
      (let ([call (decode-xmlrpc-call request-string)])
        (let ([name (rpc-call-name call)]
              [args (rpc-call-args call)])
          (if (handler-exists? name)
              (xxexpr->string (list
                               '(*pi* xml (version "1.0"))
                               (invoke-handler name args)))
              (make-handler-fault 
               (format "No handler found on server for '~a'" name)
               100))))))
  )