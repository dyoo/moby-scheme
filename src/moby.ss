#lang scheme/base

;; Moby command line compiler.
;; Usage: <program> [-n name] <filename>

(require "generate-application.ss"
         "utils.ss"
         "android/local-android-packager.ss"
         "compile-helpers-with-images.ss"
         scheme/cmdline
         scheme/path)




;; extract-name: path -> string
(define (extract-name a-path)
  (regexp-replace #rx".(ss|scm)$" 
                  (format "~a" 
                          (path->string 
                           (file-name-from-path a-path))) ""))



(define (generate-android-app name path output-dir)
  (build-android-package-in-path name 
                                 (open-program/resources path)
                                 output-dir))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Parameters with their default values.
(define name (make-parameter #f))
(define dest-dir (make-parameter #f))
(define app-compiler (make-parameter generate-android-app))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



;; lookup-app-type: string -> (string path-string path path-string -> void)
(define (lookup-app-type a-type)
  (cond 
    [(string=? a-type "js")
     generate-javascript-application]
    [(string=? a-type "js+android-phonegap")
     generate-android-app]
    [else
     (error 'moby "Type is expected to be one of [js, js+android-phonegap]")]))


;; get-name: -> string
(define (get-name)
  (or (name)
      (extract-name file-to-compile)))


;; get-output-path: -> path
(define (get-output-path)
  (normalize-path
   (or (dest-dir)
       (build-path (current-directory)
                   (upper-camel-case (get-name))))))



(define file-to-compile
  (normalize-path
   (command-line 
    #:once-each
    [("-n" "--name") n "Set the name of the output program."
                     (name n)]
    [("-d" "--dest") d "Set the destination path of the output."
                     (dest-dir (build-path d))]
    [("-t" "--type") t "Set the application type.  Options: [js, js+android-phonegap]"
                     (app-compiler (lookup-app-type t))]
    #:args (beginner-program-filename)
    (build-path beginner-program-filename))))



(let ([compiler (app-compiler)]
      [name (get-name)]
      [output-path (get-output-path)])
  (with-handlers (;; FIXME: re-enable error reporting on command-line
                  #;[exn:fail:moby-error? 
                     (lambda (exn)
                       (error 'moby "Syntax error: ~a\nAt:\n~a"
                              (exn-message exn)
                              (string-join (for/list ([stx (exn:fail:moby-syntax-error-stxs exn)])
                                             (format "line ~s, column ~s, span ~s, offset ~s, id ~s~n" 
                                                     (Loc-line (stx-loc stx))
                                                     (Loc-column (stx-loc stx))
                                                     (Loc-span (stx-loc stx))
                                                     (Loc-offset (stx-loc stx))
                                                     (Loc-id (stx-loc stx))))
                                           "\n")))])
    (parameterize ([current-directory
                    (let-values ([(base name dir?)
                                  (split-path (normalize-path file-to-compile))])
                      base)])
      (compiler name file-to-compile output-path))))
