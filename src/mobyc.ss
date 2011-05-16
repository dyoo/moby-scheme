#lang scheme/base

(require scheme/cmdline
         scheme/runtime-path
         "compiler/mzscheme-vm/compile.ss"
         "compiler/mzscheme-vm/write-support.ss")


(define-runtime-path mzscheme-vm-library-path "../support/externals/mzscheme-vm/lib")






(define (write-compilation a-platform input-file out-port)
  (write-support a-platform out-port)
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


(define current-platform (make-parameter "browser"))

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
