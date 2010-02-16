#lang scheme/base
(require scheme/contract
         scheme/string
         scheme/file
         scheme/runtime-path
         scheme/port
         scheme/path
         (only-in xml xexpr->string)
         "compile-helpers-with-images.ss"
         "image-lift.ss"
         "collects/moby/runtime/permission-struct.ss"
         "compiler/pinfo.ss"
         (only-in "compiler/helpers.ss" program?)
         (prefix-in javascript: "compiler/beginner-to-javascript.ss")
         (only-in "compiler/helpers.ss" identifier->munged-java-identifier)
         "utils.ss"
         "template.ss"
         "program-resources.ss")
  


;; A program is a (listof sexp).

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(define-runtime-path javascript-support-path "../support/js")

(define-runtime-path javascript-main-template "../support/js/main.js.template")



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; generate-javascript-application: name file path -> void
(define (generate-javascript-application name path-or-program/resources dest)
  (void
   (compile-program-to-javascript (cond [(program/resources? path-or-program/resources)
                                         path-or-program/resources]
                                        [else
                                         (open-beginner-program path-or-program/resources)])
                                  name 
                                  dest)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; compile-program-to-javascript: platform (or program/resources text%) string path-string -> compiled-program
;; Consumes a text, an application name, destination directory, and produces an application.
;; The text buffer is assumed to contain a beginner-level program that uses only the world
;; teachpack.  We need to consume a text because we must first lift up all the images
;; as resources.
(define (compile-program-to-javascript text-or-program/resources name dest-dir)
  (log-info (format "Compiling ~a to ~s" name dest-dir))
  (make-javascript-directories dest-dir)
  (cond [(program/resources? text-or-program/resources)
                        (program/resources-write-resources! text-or-program/resources dest-dir)]
                       [else
                        (lift-images-to-directory text-or-program/resources (build-path dest-dir))])
  (let*-values ([(program)
                 (cond [(program/resources? text-or-program/resources)
                        (program/resources-program text-or-program/resources)]
                       [else
                        (parse-text-as-program text-or-program/resources name)])]
                [(compiled-program)
                 (do-compilation program)])
    (call-with-output-file (build-path dest-dir "main.js")
      (lambda (op)
        (copy-port (open-input-string 
                    (compiled-program->main.js compiled-program))
                   op))
      #:exists 'replace)
    (delete-file (build-path dest-dir "main.js.template"))
    compiled-program))


(define (do-compilation program)
  (javascript:program->compiled-program/pinfo program (get-base-pinfo 'moby)))


;; compiled-program->main.js: compiled-program (listof named-bitmap) -> string
(define (compiled-program->main.js compiled-program)
  (let*-values ([(defns pinfo)
                (values (javascript:compiled-program-defns compiled-program)
                        (javascript:compiled-program-pinfo compiled-program))]
               [(output-port) (open-output-string)]
               [(mappings) 
                (build-mappings 
                 (PROGRAM-DEFINITIONS defns)
                 (IMAGES (string-append "["
                                        (string-join (map (lambda (b) 
                                                            (format "~s" (named-bitmap-name b)))
                                                          '()) 
                                                     ", ")
                                        "]"))
                 (PROGRAM-TOPLEVEL-EXPRESSIONS
                  (javascript:compiled-program-toplevel-exprs
                   compiled-program))
		 (PERMISSIONS (get-permission-js-array (pinfo-permissions pinfo))))])
    (fill-template-port (open-input-file javascript-main-template)
                        output-port
                        mappings)
    (get-output-string output-port)))



;; get-permission-js-array: (listof permission) -> string
(define (get-permission-js-array perms) 
  (string-append "["
		 (string-join (map (lambda (x)
				     (format "plt.Kernel.invokeModule('moby/runtime/permission-struct').EXPORTS.string_dash__greaterthan_permission(~s)" (permission->string x)))
				   perms)
			      ", ")
		 "]"))


;; subdirectory-of?: boolean
;; Is a-dir a subdirectory of parent-dir?
(define (subdirectory-of? parent-dir -a-dir)
  (let ([parent-dir (normalize-path parent-dir)])
    (let loop ([a-dir (normalize-path -a-dir)])
      (cond [(string=? (path->string parent-dir)
                       (path->string a-dir))
             #t]
            [else
             (let ([new-subdir (normalize-path (simplify-path (build-path a-dir 'up)))])
               (cond [(string=? (path->string new-subdir)
                                (path->string a-dir))
                      #f]
                     [else
                      (loop new-subdir)]))]))))


;; make-javascript-directories: path -> void
(define (make-javascript-directories dest-dir)
  (make-directory* dest-dir)
  
  ;; Paranoid check: if dest-dir is a subdirectory of
  ;; javascript-support-path, we are in trouble!
  (when (subdirectory-of? javascript-support-path dest-dir)
    (error 'moby "The output directory (~s) must not be a subdirectory of ~s."
           (path->string (normalize-path dest-dir))
           (path->string (normalize-path javascript-support-path))))


  (for ([subpath (list "css" "runtime")])
    (copy-directory/files* (build-path javascript-support-path subpath) 
                           (build-path dest-dir subpath)))
  (for ([file (list "index.html" "main.js.template")])
    (when (file-exists? (build-path dest-dir file))
      (delete-file (build-path dest-dir file)))
    (copy-file (build-path javascript-support-path file)
               (build-path dest-dir file))))



(provide/contract [generate-javascript-application
                   (string? (or/c path-string? program/resources?) path-string? . -> . any)]
                  
                  [compiled-program->main.js
                   (javascript:compiled-program? . -> . string?)])