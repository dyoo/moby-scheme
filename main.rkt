#lang racket/base

(require racket/path
         racket/contract
         (prefix-in js-vm: (planet dyoo/js-vm:1:3/main))
         (planet dyoo/js-vm:1:3/private/misc)
         (planet dyoo/js-vm:1:3/private/log-port)
         "src/android/android-packager.ss")


(provide/contract 
 [create-android-phone-package (path-string? path-string? . -> . any)]
 [run-in-browser (path-string? . -> . any)])


;; make-package-name: path -> string
(define (make-package-name a-path)
  (let-values ([(base name dir?)
                (split-path a-path)])
    (remove-filename-extension name)))



;; At the moment, reuse js-vm's run-in-browser.  We may need to do some extra
;; work to add mock classes for cell-phone functionality.
(define run-in-browser js-vm:run-in-browser)



;; create-android-phone-package: path-string path-string -> void
;;
;; Compiles a-filename into an android package, and write it
;; out at the given output-file path.
(define (create-android-phone-package a-filename output-file)
  (let ([a-filename (normalize-path a-filename)]
        [output-file (normalize-path output-file)])
    (with-handlers
        ([exn:fail? 
          (lambda (exn)
            (log-warning (format "An internal error occurred during compilation: ~a\n"
                                 (exn-message exn)))
            (raise exn))])
      
      (call-with-output-file output-file
        (lambda (op) 
          (log-info (format "Writing package to file ~a...\n" output-file))
          (write-bytes (build-android-package 
                        (make-package-name output-file)
                        a-filename)
                       op))
        #:exists 'replace)
      (log-info "Done!\n"))))