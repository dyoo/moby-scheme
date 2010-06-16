#lang scheme/base

(require scheme/cmdline
         scheme/runtime-path
         scheme/match
         scheme/port
         "compiler/mzscheme-vm/compile.ss")


(define-runtime-path mzscheme-vm-library-path "../support/externals/mzscheme-vm/lib")


;; write-platform-libraries: string output-port -> void
;; Writes out the platform-specific libraries out to the given output port.
(define (write-platform-libraries a-platform out-port)
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
     

;; cat-to-port: path output-port -> void
;; Write out contents of path to output port.
(define (cat-to-port a-path out-port)
  (call-with-input-file a-path (lambda (ip) (copy-port ip out-port))))



(define (write-compilation a-platform input-file out-port)
  (write-platform-libraries a-platform out-port)
  (fprintf out-port #<<EOF
var state = interpret.load(
EOF
           )
  (call-with-input-file input-file 
    (lambda (ip) 
      ;; HACK: currently ignoring lang line!
      (void (read-line ip))
      (compile ip out-port)))
  (fprintf out-port #<<EOF
               );
interpret.run(state, function(lastResult) {});
EOF
           ))





;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(define current-platform (make-parameter "node"))

;; output-name: path -> path
(define (output-name a-path)
  (regexp-replace #px"\\.\\w+$" 
                  (path->string (build-path a-path)) 
                  ".js"))


;; mobyc: command line compiler
(define files-to-compile
  (command-line #:program "mobyc"
                #:once-any
                [("-p" "--platform") platform 
                                     "Platform"
                                     (current-platform platform)]
                #:args filenames
                
                filenames))

(let ([platform (current-platform)])
  (for ([file (in-list files-to-compile)])
    (call-with-output-file (output-name file)
      (lambda (op)
        (write-compilation platform file op))
      #:exists 'replace)))
