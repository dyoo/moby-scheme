#lang scheme/base

;; Moby command line compiler.
;; Usage: <program> [-n name] <filename>

(require "compile-world.ss"
         scheme/cmdline)


;; extract-name: string -> string
(define (extract-name a-name)
  (regexp-replace #rx".(ss|scm)$" (format "~a" a-name) ""))



(define name (make-parameter #f))
(define dest-dir (make-parameter #f))


(define file-to-compile
  (command-line 
   #:once-each
   [("-n" "--name") n "Set the name of the output program"
                    (name n)]
   [("-d" "--dest") d "Set the destination path of the output"
                    (dest-dir d)]
   #:args (filename)
   filename))


(generate-j2me-application (or (name) 
                               (extract-name filename))
                           filename 
                           (or (dest-dir)
                               (build-path (current-directory)
                                           "bin")))

