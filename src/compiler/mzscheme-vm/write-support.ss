#lang scheme/base

;; Writes out some support libraries that need to be included to
;; properly evaluate a mzscheme-vm-compiled program.
;;
;; TODO: automatically generate the bootstrap-teachpack code.



(require scheme/runtime-path
         scheme/port
         scheme/contract
         scheme/string
         "../../collects/moby/runtime/binding.ss"
         "../rbtree.ss"
         "../pinfo.ss"
         "compile.ss")

(define-runtime-path mzscheme-vm-library-path "../../../support/externals/mzscheme-vm/lib")

(define-runtime-path collections-path "collections")


;; cat-to-port: path output-port -> void
;; Write out contents of path to output port.
(define (cat-to-port a-path out-port)
  (call-with-input-file a-path (lambda (ip) (copy-port ip out-port))))

;; write-platform-libraries: string output-port -> void
;; Writes out the platform-specific libraries out to the given output port.
(define (write-support a-platform out-port)
  (let ([platform-specific-js-path
         (build-path mzscheme-vm-library-path 
                     (string-append a-platform "-platform.js"))])
    
    (cond
      [(file-exists? platform-specific-js-path)
       (cat-to-port platform-specific-js-path out-port)
       (call-with-input-file (build-path mzscheme-vm-library-path "order")
         (lambda (order-ip)
           (for ([filename (in-lines order-ip)])
             (cat-to-port (build-path mzscheme-vm-library-path filename) out-port))))]
      [else
       (error 'mobyc (format "No support for platform ~s" a-platform))])))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Collection writing



(define (write-collections out-port)
  ;; FIXME: figure out better way to do this besides assigning toplevel
  ;; variable COLLECTIONS.
  ;;
  ;; FIXME: write exported bindings out so the compiler knows about them.
  (fprintf out-port "var COLLECTIONS = {};\n")
  
  (write-single-collection 'bootstrap/bootstrap-teachpack
                           (build-path collections-path 
                                       "bootstrap" 
                                       "bootstrap-teachpack-translated.ss")
                           out-port)
  
  (write-single-collection 'bootstrap/cage-teachpack
                           (build-path collections-path 
                                       "bootstrap" 
                                       "cage-teachpack-translated.ss")
                           out-port)
  
  (write-single-collection 'bootstrap/function-teachpack
                           (build-path collections-path 
                                       "bootstrap" 
                                       "function-teachpack-translated.ss")
                           out-port))


;; write-collection: symbol path output-port -> void
(define (write-single-collection module-name source-path out-port)
  (fprintf out-port "COLLECTIONS[~s] = { 'bytecode': " (symbol->string module-name))
  (call-with-input-file source-path 
    (lambda (in)
      (let ([pinfo (compile in out-port #:name module-name)])
        (fprintf out-port ", 'provides': [~a]};\n"
                 (string-join (map (lambda (a-binding)
                                     (format "~s" (symbol->string (binding-id a-binding))))
                                   (pinfo-get-exposed-bindings pinfo))
                              ","))))))




(provide/contract [write-support (string? output-port? . -> . any)]
                  [write-collections (output-port? . -> . any)])