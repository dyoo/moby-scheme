#lang scheme/base
(require scheme/contract
         scheme/string
         scheme/file
         scheme/runtime-path
         scheme/port
         (only-in xml xexpr->string)
         "compile-helpers.ss"
         "image-lift.ss"
         "compiler/permission.ss"
         "compiler/pinfo.ss"
         (prefix-in javascript: "compiler/beginner-to-javascript.ss")
         (only-in "compiler/helpers.ss" identifier->munged-java-identifier)
         "utils.ss"
         "template.ss"
         "config.ss")

(provide/contract [generate-javascript-application
                   (string? path-string? path-string? . -> . any)]
                  
                  [generate-javascript+android-phonegap-application
                   (string? path-string? path-string? . -> . any)]
                  
                  [compiled-program->main.js
                   (javascript:compiled-program? (listof named-bitmap?) . -> . string?)])



;; A program is a (listof sexp).

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-runtime-path phonegap-path "../support/phonegap/android-1.5")
(define-runtime-path jsworld-path"../support/jsworld")
(define-runtime-path icon-path "../support/icons/icon.png")

(define-runtime-path javascript-support-path "../support/js")

(define-runtime-path javascript-main-template "../support/js/main.js.template")



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;; generate-javascript-application: name file dest
(define (generate-javascript-application name file dest)
  (void
   (compile-program-to-javascript (open-beginner-program file) name dest)))


;; generate-javascript+android-phonegap-application: name file dest
(define (generate-javascript+android-phonegap-application name file dest)
  (make-directory* dest)
  (copy-directory/files* phonegap-path dest)
  (let* ([compiled-program         
          (compile-program-to-javascript (open-beginner-program file) 
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
        (fprintf op "sdk-location=~a~n" (current-android-sdk-path)))
      #:exists 'replace)
 
    ;; HACK!
    ;; Add an import statement to package.R in the class file, so we can compile things.
    (make-directory* (build-path dest "src" "plt" "moby" classname))
    (let* ([middleware 
            (get-file-bytes (build-path dest "src" "com" "phonegap" "demo" "DroidGap.java"))]
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
                       (android:label ,name))
                      (intent-filter 
                       ()
                       (action ((android:name "android.intent.action.MAIN")))
                       (category
                        ((android:name
                          "android.intent.category.LAUNCHER")))))))])
    
    (xexpr->string AndroidManifest.xml)))
    




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; compile-program-to-javascript: platform text% string path-string -> compiled-program
;; Consumes a text, an application name, destination directory, and produces an application.
;; The text buffer is assumed to contain a beginner-level program that uses only the world
;; teachpack.  We need to consume a text because we must first lift up all the images
;; as resources.
(define (compile-program-to-javascript text name dest-dir)
  (log-info (format "Compiling ~a to ~s" name dest-dir))
  (make-javascript-directories dest-dir)
  (let*-values ([(named-bitmaps)
                 (lift-images-to-directory text (build-path dest-dir))]
                [(program)
                 (parse-text-as-program text)]
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




;; make-javascript-directories: path -> void
(define (make-javascript-directories dest-dir)
  (make-directory* dest-dir)
  (copy-directory/files* javascript-support-path dest-dir)
  (copy-directory/files* jsworld-path (build-path dest-dir "runtime" "jsworld")))
