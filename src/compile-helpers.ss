#lang scheme/base



(require scheme/gui/base
         scheme/file
         scheme/class
         scheme/port
         scheme/runtime-path
         scheme/contract
         "config.ss"
         "stx-helpers.ss"
         "image-lift.ss"
         "collects/runtime/stx.ss"
         "compiler/pinfo.ss"
         "collects/runtime/permission-struct.ss"
         "collects/runtime/binding.ss")

;; Common helper functions used in the compiler.

(define-runtime-path yui.jar "../support/yuicompressor-2.4.2.jar")
(define-runtime-path google-closure-compiler.jar "../support/closure-compiler.jar")





;; parse-text-as-program: text -> program
;; Given a text, returns a program as well.
(define (parse-text-as-program a-text [source-name "<unknown>"])
  (let* ([ip (open-input-text-editor a-text)])
    (port-count-lines! ip)
    (parameterize ([read-accept-reader #t]
		   [read-decimal-as-inexact #f])
      (let ([stx (read-syntax source-name ip)])
        (syntax-case stx ()
          [(module name lang (#%module-begin body ...))
           (map syntax->stx (syntax->list #'(body ...)))]
          [(module name lang body ...)
           (map syntax->stx (syntax->list #'(body ...)))]
          [else
           (error 'moby
                  (string-append "The input does not appear to be a Moby module; "
                                 "I don't see a \"#lang moby\" at the top of the file."))])))))






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
  
;; get-on-start-code: pinfo -> string
#;(define (get-on-start-code a-pinfo)
  (apply string-append
         (map permission->on-start-code (get-permissions a-pinfo))))

;; get-on-pause-code: pinfo -> string
#;(define (get-on-pause-code a-pinfo)
  (apply string-append
         (map permission->on-pause-code (get-permissions a-pinfo))))

;; get-on-shutdown-code: pinfo -> string
#;(define (get-on-destroy-code a-pinfo)
  (apply string-append
         (map permission->on-destroy-code (get-permissions a-pinfo))))
  






;; lift-images: text path -> (listof named-bitmap)
;; Lifts up the image snips in the text, writing them into the resource directory.
;; The snips in the text will be replaced with the expression (create-image <path>)
;; where path refers to the file saves in the resource directory.
(define (lift-images-to-directory a-text resource-dir)
  (make-directory* resource-dir)
  (let ([named-bitmaps (lift-images! a-text)])
    (for ([nb named-bitmaps])
      (named-bitmap-save nb resource-dir))
    named-bitmaps))



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


;; open-beginner-program: path-string -> text%
;; Opens up the beginner-level program.
(define (open-beginner-program path)
  (define text (new text%))
  (send text insert-file (if (path? path) 
                             (path->string path)
                             path))
  text)


;; run-ant-build.xml: path string -> void
;; Runs ant to build the program in the destination directory.
;; Assumes the build file is called "build.xml" at the top of the directory.
(define (run-ant-build.xml dest-dir target)
  (parameterize ([current-directory dest-dir])
    (let*-values ([(string-error-port) (open-output-string "")]
                  [(a-subprocess inp outp errp)
                   (subprocess #f #f #f (current-ant-bin-path) target)]
                  [(t1 t2) 
                   (values (thread (lambda () 
                                     (copy-port-to-debug-log inp)))
                           (thread (lambda ()
                                     (copy-port-to-error-log (peeking-input-port errp))
                                     (copy-port errp string-error-port))))])
      (close-output-port outp)
      (subprocess-wait a-subprocess)
      (sync t1)
      (sync t2)
      (unless (= 0 (subprocess-status a-subprocess))
        (error 'ant "Internal error while running ant: ~a"
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
 [parse-text-as-program (((is-a?/c text%)) ((or/c string? false/c)) . ->* .  (listof stx?))]
 [get-permissions (pinfo? . -> . (listof permission?))]
 #;[get-on-start-code (pinfo? . -> . string?)]
 #;[get-on-pause-code (pinfo? . -> . string?)]
 #;[get-on-destroy-code (pinfo? . -> . string?)]
 [lift-images-to-directory ((is-a?/c text%) path? . -> . (listof named-bitmap?))]
 [open-beginner-program (path-string? . -> . (is-a?/c text%))]
 [run-ant-build.xml (path? string? . -> . any)]
 [yui-compress (bytes? . -> . bytes?)]
 [google-closure-compile ((bytes?) (#:aggressive? boolean?) . ->* . bytes?)])