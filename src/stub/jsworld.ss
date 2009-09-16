#lang scheme/base
(require "../compiler/beginner-to-javascript.ss"
         "../compiler/pinfo.ss"
         "../template.ss"
         "../compiler/permission.ss"
         "../compiler/stx.ss"
         "net.ss"
         scheme/local
         scheme/contract
         scheme/runtime-path
         scheme/string
         scheme/tcp
         web-server/servlet
         web-server/servlet-env
         web-server/dispatch)


(define-runtime-path javascript-support "../../support/js")
(define-runtime-path javascript-main-template "../../support/js/main.js.template")




(define-struct jsworld-widget (attrs) #:prefab)
(define-struct (jsworld-widget:div jsworld-widget) () #:prefab)
(define-struct (jsworld-widget:p jsworld-widget) () #:prefab)
(define-struct (jsworld-widget:button jsworld-widget) (f ef) #:prefab)
(define-struct (jsworld-widget:input jsworld-widget) (type) #:prefab)
(define-struct (jsworld-widget:bidirectional-input jsworld-widget) (type val-f update-f) #:prefab)
(define-struct (jsworld-widget:img jsworld-widget) (src) #:prefab)
(define-struct (jsworld-widget:text jsworld-widget) (text) #:prefab)
(define-struct (jsworld-widget:node jsworld-widget) (node) #:prefab)


(define (js-div (attrs '()))
  (make-jsworld-widget:div attrs))

(define (js-p (attrs '()))
  (make-jsworld-widget:p attrs))

(define (js-button f (attrs '()))
  (make-jsworld-widget:button attrs f (lambda (w) '())))

(define (js-button* f ef (attrs '()))
  (make-jsworld-widget:button attrs f ef))

(define (js-input type (attrs '()))
  (make-jsworld-widget:input attrs type))

(define (js-bidirectional-input type val-f update-f (attrs '()))
  (make-jsworld-widget:bidirectional-input attrs type val-f update-f))

(define (js-img src (attrs '()))
  (make-jsworld-widget:img attrs src))

(define (js-text text (attrs '()))
  (make-jsworld-widget:text attrs text))

(define (js-node raw-node (attrs '()))
  (make-jsworld-widget:node attrs raw-node))






;; js-big-bang/source: (listof stx) -> void
;; Generate a web site that compiles and evaluates the program.
(define (js-big-bang/source source-code)
  (local [(define main.js 
            (compiled-program->main.js (do-compilation source-code)))
          
          (define-values (dispatcher url)
            (dispatch-rules
             [("main.js") main-js]
             [("networkProxy") network-proxy]))
          
          (define (main-js req)
            (list #"text/javascript"
                  main.js))
          
          (define (network-proxy req)
            (list #"text/plain"
                  (get-url (extract-binding/single 'url (request-bindings req)))))]
    (let* ([T 84]
           [portno
            (let loop (;; Numerology at work  (P = 80, L = 76, T=84).
                       [portno 8076]
                       [attempts 0]) 
              (with-handlers ((exn:fail:network? (lambda (exn)
                                                   (cond [(< attempts T)
                                                          (loop (add1 portno)
                                                                (add1 attempts))]
                                                         [else
                                                          (raise exn)]))))
                ;; There's still a race condition here... Not sure how to do this right.
                (let ([port (tcp-listen portno 4 #t #f)])
                  (tcp-close port)
                  portno)))])
      (serve/servlet dispatcher
                     #:port portno
                     #:listen-ip #f
                     #:servlet-path "/"
                     #:servlet-regexp #rx"(^/main.js$)|(^/networkProxy)"
                     #:extra-files-paths (list javascript-support)))))


;;; FIXME: A lot of this is just copy-and-pasted from generate-application.  FIXME!

(define (do-compilation program)
  (program->compiled-program/pinfo program (get-base-pinfo 'moby)))

;; compiled-program->main.js: compiled-program -> string
(define (compiled-program->main.js compiled-program)
  (let*-values ([(defns pinfo)
                 (values (compiled-program-defns compiled-program)
                         (compiled-program-pinfo compiled-program))]
                [(output-port) (open-output-string)]
                [(mappings) 
                 (build-mappings 
                  (PROGRAM-DEFINITIONS defns)
                  (IMAGES (string-append "[" "]"))
                  (PROGRAM-TOPLEVEL-EXPRESSIONS
                   (compiled-program-toplevel-exprs
                    compiled-program))
                  (PERMISSIONS (get-permission-js-array (pinfo-permissions pinfo))))])
    (fill-template-port (open-input-file javascript-main-template)
                        output-port
                        mappings)
    (get-output-string output-port)))

;; get-permission-js-array: (listof permission) -> string
(define (get-permission-js-array perms) 
  (string-append "["
                 (string-join (map (lambda (x)
                                     (format "string_dash__greaterthan_permission(~s)" (permission->string x)))
                                   perms)
                              ", ")
                 "]"))



(define attrs/c (listof (list/c string? string?)))

;; FIXME: contracts!
(provide/contract [js-big-bang/source ((listof stx?) . -> . any)]
                  [js-div (() (attrs/c) . ->* . jsworld-widget?)]
                  [js-p (() (attrs/c) . ->* . jsworld-widget?)]
                  [js-button (((any/c . -> . any/c)) 
                              (attrs/c) 
                              . ->* . jsworld-widget?)]
                  [js-button* (((any/c . -> . any/c) 
                                (any/c . -> . any/c))
                               (attrs/c) 
                               . ->* .
                               jsworld-widget?)]
                  [js-input ((string?) 
                             (attrs/c) . ->* . jsworld-widget?)]
                  [js-bidirectional-input ((string? 
                                            (any/c . -> . any/c) 
                                            (any/c . -> . any/c))
                                           (attrs/c) . ->* . jsworld-widget?)]
                  [js-img ((string?) (attrs/c) . ->* . jsworld-widget?)]
                  [js-text (string? . -> . jsworld-widget?)]
                  [js-node ((any/c) (attrs/c) . ->* . jsworld-widget?)])