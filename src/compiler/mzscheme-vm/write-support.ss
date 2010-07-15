#lang scheme/base

;; Writes out some support libraries that need to be included to
;; properly evaluate a mzscheme-vm-compiled program.
;;
;; TODO: automatically generate the bootstrap-teachpack code.



(require scheme/runtime-path
         scheme/port
         scheme/contract)

(define-runtime-path mzscheme-vm-library-path "../../../support/externals/mzscheme-vm/lib")


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

(provide/contract [write-support (string? output-port? . -> . any)])