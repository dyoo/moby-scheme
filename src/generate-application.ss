#lang scheme/base
(require scheme/contract
         scheme/string
         scheme/file
         scheme/runtime-path
         scheme/port
         (only-in xml xexpr->string)
         "compile-helpers.ss"
         "image-lift.ss"
         (prefix-in javascript: "beginner-to-javascript.ss")
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

(define-runtime-path javascript-support-path "../support/js")

(define-runtime-path javascript-main-template "../support/js/main.js.template")



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;; generate-javascript-application: name file dest
(define (generate-javascript-application name file dest)
  (compile-program-to-javascript (open-beginner-program file) name dest))


;; generate-javascript-application: name file dest
(define (generate-javascript+android-phonegap-application name file dest)
  (compile-program-to-javascript (open-beginner-program file) name dest))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
;
;;; compile-program-to-android: text string path -> void
;;; Writes out the compilation of the text program with the given name to the
;;; destination directory.
;(define (compile-program-to-android text name dest-dir)
;  (log-info (format "Compiling ~a to ~s" name dest-dir))
;  (make-android-directories dest-dir)
;  (lift-images-to-directory text (build-path dest-dir "src"))
;  (let* ([classname
;          (upper-camel-case name)]
;         [program
;          (parse-text-as-program text)]
;         [compiled-program
;          (program->compiled-program program)]
;         [pinfo
;          (compiled-program-pinfo compiled-program)]
;         [mappings
;          (build-mappings 
;           (PROGRAM-NAME classname)
;           (PROGRAM-DEFINITIONS 
;            (compiled-program-defns compiled-program))
;           (PROGRAM-TOPLEVEL-EXPRESSIONS
;            (compiled-program-toplevel-exprs compiled-program))
;           (ON-START (get-on-start-code pinfo))
;           (ON-PAUSE (get-on-pause-code pinfo))
;           (ON-DESTROY (get-on-destroy-code pinfo)))]
;         [source-path
;          (build-path dest-dir "src" "org" "plt" classname (string-append classname ".java"))])
;    (cond
;      [(stub=? (choose-program-stub pinfo) STUB:WORLD)
;       (fill-template-file j2me-world-stub-path source-path mappings)
;       (write-android:world-resources pinfo name dest-dir)
;       (run-ant-build.xml dest-dir)]      
;
;      [(stub=? (choose-program-stub pinfo) STUB:GUI-WORLD)
;       ;; fixme!
;       (fill-template-file android-gui-world-stub-path source-path mappings)
;       (write-android:gui-world-resources pinfo name dest-dir)
;       (run-ant-build.xml dest-dir)
;       (void)]
;      
;      [else
;       (error 'compile-program-to-android
;              "Unrecognized stub ~s"
;              (choose-program-stub pinfo))])))
;
;
;(define (make-android-directories dest-dir)
;  (when (directory-exists? dest-dir)
;    (delete-directory/files dest-dir))
;  (make-directory* dest-dir)
;  (copy-directory/files* common-support-src-path (build-path dest-dir "src"))
;  ;; At the moment, we use j2me bridge classes.
;  (copy-directory/files* j2me-support-src-path (build-path dest-dir "src"))
;  (copy-directory/files* android-skeleton-path dest-dir)
;  (make-directory* (build-path dest-dir "libs")))
;
;
;(define (write-android:world-resources pinfo a-name dest-dir)
;  (let ([mappings (build-mappings (PROGRAM-NAME (upper-camel-case a-name))
;                                  (ANDROID-SDK-PATH (current-android-sdk-path))
;                                  (ANDROID-TOOLS-PATH (current-android-sdk-tools-path)))])
;    (replace-template-file dest-dir "src/j2ab/android/app/J2ABMIDletActivity.java" mappings)
;    (write-android-manifest dest-dir 
;                            #:name a-name
;                            #:package (string-append "plt." (upper-camel-case a-name))
;                            #:permissions (pinfo-permissions pinfo))
;    (replace-template-file dest-dir "build.xml" mappings)
;    (replace-template-file dest-dir "res/values/strings.xml" mappings)
;    (replace-template-file dest-dir "src/jad.properties" mappings)))
;
;
;(define (write-android:gui-world-resources pinfo a-name dest-dir)
;  (let* ([classname (upper-camel-case a-name)]
;         [mappings (build-mappings (PROGRAM-NAME classname)
;                                   (ANDROID-SDK-PATH (current-android-sdk-path))
;                                   (ANDROID-TOOLS-PATH (current-android-sdk-tools-path)))])
;    (write-android-manifest dest-dir 
;                            #:name a-name
;                            #:package (string-append "plt." (upper-camel-case a-name))
;                            #:activity-class (string-append
;                                              "plt." classname "." classname)
;                            #:permissions (pinfo-permissions pinfo))
;    (replace-template-file dest-dir "build.xml" mappings)
;    (replace-template-file dest-dir "res/values/strings.xml" mappings)
;    (replace-template-file dest-dir "src/jad.properties" mappings)))
;
;
;
;;; write-android-manifest: path (#:name string) (#:permissions (listof string)) -> void
;(define (write-android-manifest dest-dir
;                                #:name name
;                                #:package package
;                                #:activity-class (activity-class 
;                                                  "j2ab.android.app.J2ABMIDletActivity")
;                                #:permissions (permissions '()))
;  (call-with-output-file (build-path dest-dir "AndroidManifest.xml")
;    (lambda (op)
;      (display (get-android-manifest dest-dir 
;                                     #:name name 
;                                     #:package package 
;                                     #:activity-class activity-class 
;                                     #:permissions permissions) op))
;    #:exists 'replace))
;
;  

;; get-android-manifest: path (#:name string) (#:package string) (#:activity-class string) (#:permissions (listof string)) -> string
(define (get-android-manifest dest-dir
                                #:name name
                                #:package package
                                #:activity-class (activity-class 
                                                  "j2ab.android.app.J2ABMIDletActivity")
                                #:permissions (permissions '()))
  (let ([AndroidManifest.xml
         `(manifest 
           ((xmlns:android "http://schemas.android.com/apk/res/android")
            (package ,package)
            (android:versionCode "1")
            (android:versionName "1.0.0"))

           ,@(map (lambda (p)
                    `(uses-permission ((android:name ,p))))
                  permissions)
           
           (application 
            ((android:label "@string/app_name")
             (android:icon "@drawable/icon"))
            (activity ((android:name ,activity-class)
                       (android:label "@string/app_name"))
                      (intent-filter 
                       ()
                       (action ((android:name "android.intent.action.MAIN")))
                       (category
                        ((android:name
                          "android.intent.category.LAUNCHER")))))))])
    
    (xexpr->string AndroidManifest.xml)))
    




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; compile-program-to-javascript: platform text% string path-string -> void
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
                 (javascript:program->compiled-program program)])
    (call-with-output-file (build-path dest-dir "main.js")
      (lambda (op)
        (copy-port (open-input-string 
                    (compiled-program->main.js compiled-program named-bitmaps))
                   op))
      #:exists 'replace)
    (delete-file (build-path dest-dir "main.js.template"))))


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
                 (ON-START (get-on-start-code pinfo))
                 (ON-PAUSE (get-on-pause-code pinfo))
                 (ON-DESTROY (get-on-destroy-code pinfo)))])
    (fill-template-port (open-input-file javascript-main-template)
                        output-port
                        mappings)
    (get-output-string output-port)))



;; make-javascript-directories: path -> void
(define (make-javascript-directories dest-dir)
  (make-directory* dest-dir)
  (copy-directory/files* javascript-support-path dest-dir)
  (copy-directory/files* jsworld-path (build-path dest-dir "runtime" "jsworld"))
  (copy-or-overwrite-file (build-path phonegap-path "assets" "phonegap.js") (build-path dest-dir "runtime" "phonegap.js")))
