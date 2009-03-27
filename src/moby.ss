#lang scheme/base

;; Moby command line compiler.
;; Usage: <program> [-n name] <filename>

(require "compile-world.ss"
         "utils.ss"
         scheme/cmdline
         scheme/path)


;; extract-name: path -> string
(define (extract-name a-path)
  (regexp-replace #rx".(ss|scm)$" 
                  (format "~a" 
                          (path->string 
                           (file-name-from-path a-path))) ""))



(define name (make-parameter #f))
(define dest-dir (make-parameter #f))


(define file-to-compile
  (command-line 
   #:once-each
   [("-n" "--name") n "Set the name of the output program"
                    (name n)]
   [("-d" "--dest") d "Set the destination path of the output"
                    (dest-dir (build-path d))]
   #:args (filename)
   (build-path filename)))


;; get-name: -> string
(define (get-name)
  (or (name)
      (extract-name file-to-compile)))

;; get-output-path: -> path
(define (get-output-path)
 (or (dest-dir)
     (build-path (current-directory)
                 (upper-camel-case (get-name)))))

(generate-android-application 
 (get-name)
 file-to-compile
 (get-output-path))


