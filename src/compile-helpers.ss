#lang scheme/base



(require scheme/port
         scheme/runtime-path
         scheme/contract
         "stx-helpers.ss"
         "config.ss"
         "compiler/pinfo.ss"
         "collects/moby/runtime/stx.ss"
         "collects/moby/runtime/permission-struct.ss"
         "collects/moby/runtime/binding.ss")

;; Common helper functions used in the compiler.

(define-runtime-path yui.jar "../support/yuicompressor-2.4.2.jar")
(define-runtime-path google-closure-compiler.jar "../support/closure-compiler.jar")




;; read-syntaxes: input-port #:name symbol -> (listof stx)
(define (read-syntaxes in #:name name)
  (port-count-lines! in)
  (parameterize ([read-accept-reader #t]
                 [read-decimal-as-inexact #f])
    (map syntax->stx
         (let loop ()
           (let ([stx (read-syntax name in)])
             (cond
               [(eof-object? stx)
                '()]
               [else
                (cons stx (loop))]))))))


;; parse-string-as-program: string -> program
;; Given a text, returns a program as well.
(define (parse-string-as-program a-string [source-name "<unknown>"])
  (let* ([ip (open-input-string a-string)])
    (port-count-lines! ip)
    (parameterize ([read-accept-reader #t]
		   [read-decimal-as-inexact #f])
      (let loop ([a-stx (read-syntax source-name ip)])
        (cond
          [(eof-object? a-stx)
           '()]
          [else
           (cons (syntax->stx a-stx)
                 (loop (read-syntax source-name ip)))])))))



;; get-permissions: pinfo -> (listof permission)
(define (get-permissions a-pinfo)
  (define ht (make-hash))
  (for ([b (pinfo-used-bindings a-pinfo)])
    (cond
      [(binding:function? b)
       (for ([p (binding:function-permissions b)])
         (hash-set! ht p #t))]))
  (for/list ([p (in-hash-keys ht)])
    p))
  



;; copy-port-to-debug-log: input-port -> void
;; Writes out the lines of the input port as debug events.
(define (copy-port-to-debug-log inp)
  (let loop ([line (read-line inp)])
    (unless (eof-object? line)
      (log-debug line)
      (loop (read-line inp)))))


;; copy-port-to-error-log: input-port -> void
;; Writes out the lines of the input port as debug events.
(define (copy-port-to-error-log inp)
  (let loop ([line (read-line inp)])
    (unless (eof-object? line)
      (log-error line)
      (loop (read-line inp)))))



;; run-ant-build.xml: path string -> void
;; Runs ant to build the program in the destination directory.
;; Assumes the build file is called "build.xml" at the top of the directory.
(define (run-ant-build.xml dest-dir target)
  (parameterize ([current-directory dest-dir])
    (let*-values ([(string-output-port string-error-port)
                   (values (open-output-string "")
                           (open-output-string ""))]
                  [(a-subprocess inp outp errp)
                   (subprocess #f #f #f (current-ant-bin-path) target)]
                  [(t1 t2) 
                   (values (thread (lambda () 
                                     (copy-port-to-debug-log (peeking-input-port inp))
                                     (copy-port inp string-output-port)))
                           (thread (lambda ()
                                     (copy-port-to-error-log (peeking-input-port errp))
                                     (copy-port errp string-error-port))))])
      (close-output-port outp)
      (subprocess-wait a-subprocess)
      (sync t1)
      (sync t2)
      (unless (= 0 (subprocess-status a-subprocess))
        (error 'ant "Internal error while running ant: ~a\n\n~a"
               (get-output-string string-output-port)
               (get-output-string string-error-port)))
      (void))))



;; run-java-jar: path bytes (listof string) -> bytes
(define (run-java-jar jar-file-path 
                      #:bytes some-bytes 
                      #:args args)
  (let ([get-java-path
         (lambda ()
           (find-executable-path "java"))])
    (let*-values ([(bytes-output-port) (open-output-bytes)]
                  [(a-subprocess inp outp errp)
                   (apply subprocess #f #f #f (get-java-path) "-jar" (path->string jar-file-path)
                          args)]
                  [(t1 t2) 
                   (values (thread (lambda () 
                                     (copy-port inp bytes-output-port)))
                           (thread (lambda ()
                                     (copy-port errp (current-output-port)))))])
      (copy-port (open-input-bytes some-bytes) outp)
      (close-output-port outp)
      (subprocess-wait a-subprocess)
      (sync t1)
      (sync t2)
      (get-output-bytes bytes-output-port))))

  

;; yui-compress: bytes -> bytes
(define (yui-compress source-code)
  (run-java-jar yui.jar #:bytes source-code #:args (list "--type" "js")))


;; google-closure-compile: bytes -> bytes
(define (google-closure-compile source-code #:aggressive? (aggressive? #f) )
  (run-java-jar google-closure-compiler.jar #:bytes source-code #:args 
                (if aggressive?
                    (list "--compilation_level" "ADVANCED_OPTIMIZATIONS")
                    (list))))
               






(provide/contract
 [read-syntaxes (input-port? #:name any/c . -> . (listof stx?))]
 [parse-string-as-program ((string?) (string?) . ->* .  (listof stx?))]
 [get-permissions (pinfo? . -> . (listof permission?))]
 [run-ant-build.xml (path? string? . -> . any)]
 [yui-compress (bytes? . -> . bytes?)]
 [google-closure-compile ((bytes?) (#:aggressive? boolean?) . ->* . bytes?)]                  
 )