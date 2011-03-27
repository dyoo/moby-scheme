#lang scheme/base

(require scheme/contract
         scheme/port
         scheme/class
         scheme/list
         net/url
         net/uri-codec
         "../../compiler/version.ss"
         "../../collects/moby/runtime/stx.ss"
         "../../program-resources.ss"
         "../helpers.ss")




(define current-server-url (make-parameter "http://localhost:8080/"
                                           #;"http://go.cs.brown.edu/package/"))


;; build-android-package: string program/resources -> bytes
;; Calls out to the compiler web service to get back the android package.
(define (build-android-package name program/resources)
  (let ([data (encode-parameters-in-data name program/resources)])
    ;; FIXME: get errors here and do something reasonable.
    (with-handlers ([exn:fail:network?
                     (lambda (exn)
                       (handle-network-failure exn))])
      (log-debug (format "Sending ~s ~s" (current-server-url) data))
      (let* ([ip (post-impure-port (string->url (current-server-url)) 
                                   data)]
             [headers (purify-port ip)]
             [status-code (get-status-code headers)])
        (cond
          [(= status-code 200)
           (port->bytes ip)]
          [else
           (raise (make-exn:fail (bytes->string/utf-8 (port->bytes ip))
                                 (current-continuation-marks)))])))))



;; handle-network-failure: request exn -> response
(define (handle-network-failure exn)
  (raise (make-exn:fail 
          (string-append "We are unable to build your Android package; the web service appears to be offline.\n"
                         "Please contact the Moby developers; in your response, include the following:\n"
                         (exn-message exn))
          (current-continuation-marks))))


;; get-status-code: string -> number
;; Given the 
(define (get-status-code response-headers)
  (log-debug (format "Response headers are ~s~n" response-headers))
  (string->number (second (regexp-match #px"^[^\\s]+\\s([^\\s]+)" response-headers))))


;; encode-parameters-in-data: string program/resources -> bytes
;; Encodes the parameters we need to pass in to get a program.
(define (encode-parameters-in-data name program/resources)
  (let* ([program
          (program/resources-program program/resources)]
         [compiled-program
          (do-compilation program)]
         [android-permissions
          (map permission->string (pinfo-permissions pinfo))])
    
  
  (string->bytes/utf-8
   (alist->form-urlencoded 
    ;; FIXME: we need to encode the following:
    
    ;; The compiled program (its path)
    
    ;; The permissions
    
    ;; The other resources in program/resources

    ;; The index.html
    
    (append (list
             ;; Compiler type: moby2
             (cons 't "moby2")
             
             ;; program name
             (cons 'n name))
            
            ;; The list of android permissions.            
            (map (lambda (permission)
                   (cons 'ps permission))
                 android-permissions)

            ;; The main.js program itself:
            (list 
             (cons 'r (format "~s"
                              (list 'resource 
                                    "main.js"
                                    (string->bytes/utf-8
                                     (compiled-program->main.js compiled-program))))))
               
            ;; The set of its additional resources.
            (map (lambda (a-resource)
                   (cons 'r (format "~s"
                                    (list 'resource
                                          (format "~a" (send a-resource get-name))
                                          (send a-resource get-bytes)))))
                 (program/resources-resources program/resources))
            
            ;; The runtime, index.html, and other ancillary files.
            
            
            
            )))))



;; compiled-program->main.js: compiled-program -> string
(define (compiled-program->main.js compiled-program)
  (let*-values ([(defns pinfo)
                 (values (javascript:compiled-program-defns compiled-program)
                         (javascript:compiled-program-pinfo compiled-program))]
                [(output-port) (open-output-string)]
                [(mappings) 
                 (build-mappings 
                  (PROGRAM-DEFINITIONS defns)
                  (IMAGES (string-append "[" "]"))
                  (PROGRAM-TOPLEVEL-EXPRESSIONS
                   (javascript:compiled-program-toplevel-exprs
                    compiled-program))
                  (PERMISSIONS (get-permission-js-array (pinfo-permissions pinfo))))])
    (fill-template-port (open-input-file javascript-main-template)
                        output-port
                        mappings)
    (get-output-string output-port)))


(provide/contract [build-android-package
                   (string? program/resources? . -> . bytes?)]
                  [current-server-url parameter?])