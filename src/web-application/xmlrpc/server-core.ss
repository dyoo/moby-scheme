(module server-core scheme
  (require (lib "servlet.ss" "web-server")
           (file "serialise.ss")
           (file "protocol.ss")
           (lib "xml.ss" "xml"))
  (provide (all-defined-out))
  
  ;; XML-RPC Environment
  ;; Bindings for the XML-RPC server are modeled as a simple
  ;; hash-table. We shouldn't need a more complex environment
  ;; model for an XML-RPC server; the namespace is flat.
  (define environment (make-hash))
  
  ;; add-handler : symbol (any -> any) -> void
  ;; Adds a new identifier and associated procedure to the
  ;; environment. 
  (define (add-handler id fun)
    (hash-set! environment id fun))
  
  ;; handler-exists? : symbol -> (U #t #f)
  ;; Checks to see if the requisite handler is bound in the environment.
  (define (handler-exists? id)
    (hash-ref environment id (lambda () #f)))
  
  ;; invoke-handler : sym (list-of any) -> methodResponse
  ;; Invokes the given handler on the data passed in from
  ;; the call if the handler exists. 
  ;; 
  ;; There might be other checks we could do at this point
  ;; to keep things from falling over in an ugly way; for 
  ;; the moment, I do an arity check, which is more than the 
  ;; spec calls for, I suspect.
  (define (invoke-handler name args)
    (let* ([fun (hash-ref environment name)]
           [arity (procedure-arity fun)]
           [arg-length (length args)])
      (cond
        [(= arity arg-length)
         (let* ([result (apply fun args)]
                [serialised-result (serialise result)])
           ;; (printf "result: ~s~n" result)
           ;; (printf "serialized-result: ~s~n" serialised-result)
           (make-response serialised-result))]
        [else
         (make-handler-fault 
          (format "You invoked '~a' with ~a parameters; '~a' expects ~a."
                  name arg-length name arity)
          101
          )])
      ))
  
  (define (make-response serialised-result)
    (let* ([response    `(methodResponse
                          (params 
                           (param
                            ;; Is there an inconsistent wrapping of 'value'
                            ;; around this?
                            ,serialised-result)))]
           [output (string->bytes/utf-8 (xexpr->string response))])
      (make-response/full 
       200 "Okay" (current-seconds) 
       #"text/xml" '() 
       (list output))))
    
  
  ;; make-handler-fault : string num -> methodResponse
  ;; Makes the XML-RPC 'fault' method response. 
  ;; The error codes thrown by this library should be chosen
  ;; in a less arbitrary way, and documented.
  (define (make-handler-fault string code)
    (let ([errorHash (make-hash)])
      (hash-set! 
       errorHash 'faultString string)
      (hash-set! 
       errorHash 'faultCode code)
      `(methodResponse (fault ,(serialise errorHash)))))
  
    ;; extract-xmlrpc-bindings : request -> string
  ;; The bindings come in all kinds of messed up, it seems.
  ;; This *must* be tested against clients other than ours
  ;; to decide whether this is a sensible way to handle the bindings
  ;; or not.
  (define (extract-xmlrpc-bindings request)
    ;; struct:request looks like:
    ;;   method uri headers/raw bindings/raw
    ;;   host-ip host-port client-ip
    (bytes->string/utf-8 (request-post-data/raw request)))

  ;; handle-xmlrpc-servlet-request* : request -> methodResponse
  ;; Returns the value of the computation requested by the user,
  ;; or returns a fault.
  (define (handle-xmlrpc-servlet-request* request)
    (let ([call (decode-xmlrpc-call
                 (extract-xmlrpc-bindings request))])
      
      (let ([name (rpc-call-name call)]
            [args (rpc-call-args call)])
        (if (handler-exists? name)
            (invoke-handler name args)
            (make-handler-fault 
             (format "No handler found on server for '~a'" name)
             100)))))
  
  )
