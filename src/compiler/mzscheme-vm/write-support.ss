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
         "collections/manifest.ss"
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
  (for ([a-collection-reference (in-list known-collections)])
    (write-single-collection (collection-reference-name a-collection-reference)
                             (collection-reference-path a-collection-reference)
                             out-port)))


;; write-collection: symbol path output-port -> void
;; Writes out a module record.
;; name: string
;; bytecode: bytecode
;; provides: arrayof string
(define (write-single-collection module-name source-path out-port)
  (fprintf out-port "COLLECTIONS[~s] = { 'name': ~s, "
           (symbol->string module-name)
           (symbol->string module-name))
  (call-with-input-file source-path 
    (lambda (in)
      (fprintf out-port "'bytecode': ")
      (let ([pinfo (compile/port in out-port #:name module-name)])
        (fprintf out-port ", 'provides': [~a]};\n"
                 (string-join (map (lambda (a-binding)
                                     (format "~s" (symbol->string (binding-id a-binding))))
                                   (pinfo-get-exposed-bindings pinfo))
                              ","))))))




(provide/contract [write-support (string? output-port? . -> . any)]
                  [write-collections (output-port? . -> . any)])