#lang scheme/base

(require scheme/contract
         scheme/port
         scheme/class
         scheme/list
         scheme/string
         scheme/runtime-path
         net/url
         net/uri-codec
         "../../program-resources.ss"
         
         
         "../../collects/moby/runtime/permission-struct.ss"
         "../../compiler/pinfo.ss"
         (only-in "../../compiler/helpers.ss" program?)
         (prefix-in javascript: "../../compiler/beginner-to-javascript.ss")
         (only-in "../../compiler/helpers.ss" identifier->munged-java-identifier)
         "../../template.ss"
         "../../resource.ss"
         "../../program-resources.ss")



(define-runtime-path phonegap-path "../../../support/phonegap-fork/android-1.5/assets/phonegap.js")
(define-runtime-path javascript-support-path "../../../support/js")
(define-runtime-path javascript-main-template "../../../support/js/main.js.template")


;; This parameter controls which URL's used to connect to the web service.
(define current-server-url (make-parameter "http://localhost:8080/" #;"http://go.cs.brown.edu/package/"))



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


(define (unique-strings strs)
  (define ht (make-hash))
  (for-each (lambda (s) (hash-set! ht s #t)) strs)
  (hash-map ht (lambda (k v) k)))


;; encode-parameters-in-data: string program/resources -> bytes
;; Encodes the parameters we need to pass in to get a program.
(define (encode-parameters-in-data name program/resources)
  (let*-values ([(program)
                 (program/resources-program program/resources)]
                [(compiled-program)
                 (do-compilation program)]
                [(defns pinfo)
                 (values (javascript:compiled-program-defns compiled-program)
                         (javascript:compiled-program-pinfo compiled-program))]
                
                [(android-permissions)
                 (unique-strings (apply append (map permission->android-permissions (pinfo-permissions pinfo))))])
    
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
                   (append (program/resources-resources program/resources)
                           
                           (collect-directory-resources 
                            (build-path javascript-support-path "runtime/compat") "runtime/compat")
                           
                           (list (path->resource 
                                  (build-path javascript-support-path "index.html")
                                  "index.html")
                                 
                                 (path->resource 
                                  (build-path javascript-support-path "runtime/compressed-runtime.js")
                                  "runtime/compressed-runtime.js")
                                 
                                 (path->resource phonegap-path "runtime/phonegap.js")))))))))

(define (path->resource a-path name)
  (new named-bytes-resource% 
       [name name]
       [bytes (call-with-input-file* a-path port->bytes)]))


;; do-compilation: program -> compiled-program
(define (do-compilation program)
  (javascript:program->compiled-program/pinfo program (get-base-pinfo 'moby)))


;; get-permission-js-array: (listof permission) -> string
(define (get-permission-js-array perms) 
  (string-append "["
                 (string-join (map (lambda (x)
                                     (format "plt.Kernel.invokeModule('moby/runtime/permission-struct').EXPORTS['string->permission'](~s)" (permission->string x)))
                                   perms)
                              ", ")
                 "]"))




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