#lang scheme/base
(require scheme/contract
         scheme/string
         scheme/file
         scheme/runtime-path
         scheme/port
         scheme/path
         (only-in xml xexpr->string)
         "compile-helpers.ss"
         "image-lift.ss"
         "compiler/permission.ss"
         "compiler/pinfo.ss"
         (only-in "compiler/helpers.ss" program?)
         (prefix-in javascript: "compiler/beginner-to-javascript.ss")
         (only-in "compiler/helpers.ss" identifier->munged-java-identifier)
         "utils.ss"
         "template.ss"
         "config.ss")


;; A program/resources consists of program source, and the set of named bitmaps
;; associated to it.
;; TODO: We may expand this definition to handle other resource types like music
;; and other media.
(define-struct program/resources (program named-bitmaps))
  
  



;; A program is a (listof sexp).

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-runtime-path phonegap-path "../support/phonegap-fork/android-1.5")
(define-runtime-path icon-path "../support/icons/icon.png")

(define-runtime-path javascript-support-path "../support/js")

(define-runtime-path javascript-main-template "../support/js/main.js.template")



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;






;; generate-javascript-application: name file dest
(define (generate-javascript-application name path-or-program/resources dest)
  (void
   (compile-program-to-javascript (cond [(program/resources? path-or-program/resources)
                                         path-or-program/resources]
                                        [else
                                         (open-beginner-program path-or-program/resources)])
                                  name 
                                  dest)))


;; generate-javascript+android-phonegap-application: name file dest
(define (generate-javascript+android-phonegap-application name path-or-program/resources dest)
  (unless (file-exists? (current-ant-bin-path))
    (error 'generate-javascript+android-phonegap-application
           "The Apache ant binary appears to be missing from the current PATH."))
  (unless (directory-exists? (current-android-sdk-path))
    (error 'generate-javascript+android-phonegap-application
           "The Android SDK could not be found."))

  (make-directory* dest)
  (copy-directory/files* phonegap-path dest)
  (let* ([compiled-program         
          (compile-program-to-javascript (cond [(program/resources? path-or-program/resources)
                                                path-or-program/resources]
                                               [else
                                                (open-beginner-program path-or-program/resources)])
                                         name
                                         (build-path dest "assets"))]
         [classname (upper-camel-case name)]
         [package (string-append "plt.moby." classname)])

    ;; Write out the icon
    (make-directory* (build-path dest "res" "drawable"))
    (copy-or-overwrite-file icon-path (build-path dest "res" "drawable" "icon.png"))

    (copy-or-overwrite-file (build-path phonegap-path "assets" "phonegap.js") 
                            (build-path dest "assets" "runtime" "phonegap.js"))

    ;; Put in the customized manifest.
    (write-android-manifest (build-path dest "AndroidManifest.xml")
			    #:name name
                            #:package package
                            #:activity-class (string-append package "." classname)
                            #:permissions (apply append 
                                                 (map permission->android-permissions
                                                      (pinfo-permissions 
                                                       (javascript:compiled-program-pinfo compiled-program)))))

    ;; Write out local properties so that the build system knows how to compile
    (call-with-output-file (build-path dest "local.properties")
      (lambda (op)
        (fprintf op "sdk-location=~a~n" (path->string (current-android-sdk-path))))
      #:exists 'replace)
 
    ;; HACKS!
    ;; Fix build.xml so it refers to our application.
    (let* ([build-xml-bytes (get-file-bytes (build-path dest "build.xml"))]
           [build-xml-bytes (regexp-replace #rx"DroidGap"
                                            build-xml-bytes
                                            (string->bytes/utf-8 classname))])
      (call-with-output-file (build-path dest "build.xml")
        (lambda (op) (write-bytes build-xml-bytes op))
        #:exists 'replace))
    
    ;; Write out a customized strings.xml
    (let* ([strings-xml-bytes (get-file-bytes (build-path dest "res" "values" "strings.xml"))]
           [strings-xml-bytes (regexp-replace #rx"DroidGap"
                                              strings-xml-bytes
                                              (string->bytes/utf-8 name))])
      ;; FIXME: don't use regular expressions here!
      (call-with-output-file (build-path dest "res" "values" "strings.xml")
        (lambda (op) (write-bytes strings-xml-bytes op))
        #:exists 'replace))
    
    ;; Rename DroidGap to the application name.
    (make-directory* (build-path dest "src" "plt" "moby" classname))
    (let* ([middleware 
            (get-file-bytes (build-path dest "src" "com" "phonegap" "demo" "DroidGap.java"))]
           [middleware 
            (regexp-replace #rx"package com.phonegap.demo;\n" 
                            middleware
                            (string->bytes/utf-8 (format "package plt.moby.~a;\nimport com.phonegap.demo.*;\n" classname)))]
           [middleware 
            (regexp-replace #rx"DroidGap" 
                            middleware
                            (string->bytes/utf-8 classname))])
      (call-with-output-file (build-path dest "src" "plt" "moby" classname (format "~a.java" classname))
        (lambda (op)
          (write-bytes middleware op))
        #:exists 'replace)
      (delete-file (build-path dest "src" "com" "phonegap" "demo" "DroidGap.java")))
      

    ;; Write out the defaults.properties so that ant can build
    ;; Run ant debug.
    (run-ant-build.xml dest "debug")))





;; write-android-manifest: path (#:name string) (#:permissions (listof string)) -> void
(define (write-android-manifest path
				#:name name
                                #:package package
                                #:activity-class activity-class
                                #:permissions (permissions '()))
  (call-with-output-file path
    (lambda (op)
      (display (get-android-manifest #:name name
				     #:package package 
                                     #:activity-class activity-class 
                                     #:permissions permissions) op))
    #:exists 'replace))

  
;; get-android-manifest: (#:name string) (#:package string) (#:activity-class string) (#:permissions (listof string)) -> string
(define (get-android-manifest #:name name
			      #:package package
                              #:activity-class activity-class
                              #:permissions (permissions '()))
  (let ([AndroidManifest.xml
         `(manifest 
           ((xmlns:android "http://schemas.android.com/apk/res/android")
            (package ,package)
            (android:versionCode "1")
            (android:versionName "1.0.0"))

           (uses-sdk ((android:minSdkVersion "2")))
           
           ,@(map (lambda (p)
                    `(uses-permission ((android:name ,p))))
                  permissions)
           
           (application 
            ((android:label "@string/app_name")
             (android:icon "@drawable/icon"))
            
            (activity ((android:name ,activity-class)
                       (android:label ,name)
                       (android:configChanges
                        "keyboardHidden|orientation"))
                      (intent-filter 
                       ()
                       (action ((android:name "android.intent.action.MAIN")))
                       (category
                        ((android:name
                          "android.intent.category.LAUNCHER")))))

	    (activity ((android:name "plt.playlist.PickPlaylist")
		       (android:label "PickPlaylist")
                       (android:configChanges
                        "keyboardHidden|orientation"))
		      (action ((android:name "android.intent.action.PICK")))
		      (category ((android:name "android.intent.category.DEFAULT"))))))])
    
    (xexpr->string AndroidManifest.xml)))
    




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; compile-program-to-javascript: platform (or program/resources text%) string path-string -> compiled-program
;; Consumes a text, an application name, destination directory, and produces an application.
;; The text buffer is assumed to contain a beginner-level program that uses only the world
;; teachpack.  We need to consume a text because we must first lift up all the images
;; as resources.
(define (compile-program-to-javascript text-or-program/resources name dest-dir)
  (log-info (format "Compiling ~a to ~s" name dest-dir))
  (make-javascript-directories dest-dir)
  (let*-values ([(named-bitmaps)
                 (cond [(program/resources? text-or-program/resources)
                        (let ([named-bitmaps (program/resources-named-bitmaps text-or-program/resources)])
                          (for ([bm named-bitmaps])
                            (named-bitmap-save bm dest-dir))
                          named-bitmaps)]
                       [else
                        (lift-images-to-directory text-or-program/resources (build-path dest-dir))])]
                [(program)
                 (cond [(program/resources? text-or-program/resources)
                        (program/resources-program text-or-program/resources)]
                       [else
                        (parse-text-as-program text-or-program/resources name)])]
                [(compiled-program)
                 (do-compilation program)])
    (call-with-output-file (build-path dest-dir "main.js")
      (lambda (op)
        (copy-port (open-input-string 
                    (compiled-program->main.js compiled-program named-bitmaps))
                   op))
      #:exists 'replace)
    (delete-file (build-path dest-dir "main.js.template"))
    compiled-program))


(define (do-compilation program)
  (javascript:program->compiled-program/pinfo program (get-base-pinfo 'moby)))


;; compiled-program->main.js: compiled-program (listof named-bitmap) -> string
(define (compiled-program->main.js compiled-program named-bitmaps)
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
                                                          named-bitmaps) 
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
				     (format "string_dash__greaterthan_permission(~s)" (permission->string x)))
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
                  
                  [generate-javascript+android-phonegap-application
                   (string? (or/c path-string? program/resources?) path-string? . -> . any)]
                  
                  [compiled-program->main.js
                   (javascript:compiled-program? (listof named-bitmap?) . -> . string?)]

                  [struct program/resources ([program program?]
                                             [named-bitmaps (listof named-bitmap?)])])