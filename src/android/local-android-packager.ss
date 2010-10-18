#lang racket/base
(require racket/file
         racket/runtime-path
         racket/path
         racket/contract
         racket/list
         (only-in xml xexpr->string)
         
         (planet dyoo/js-vm/private/module-record)
         (planet dyoo/js-vm/private/compile-moby-module)
         (planet dyoo/js-vm/private/write-module-records)
         (planet dyoo/js-vm/private/write-runtime)
         
         "../config.rkt" 
         "../utils.rkt"
         "../compile-helpers.rkt"
         )

(define-runtime-path phonegap-path "../../support/phonegap-fork/android-1.5")
(define-runtime-path icon-path "../../support/icons/icon.png")
(define-runtime-path javascript-support-path "../../support/js")

;; FIXME
;; FIXME
;; FIXME
;; Must figure out way to get at these files.  They're no longer at the
;; runtime path here.

(define-runtime-path javascript-main.js 
  "../../support/externals/mzscheme-vm/support/main.js")
(define-runtime-path javascript-evaluator.js 
  "../../support/externals/mzscheme-vm/support/evaluator.js")


;; build-android-package: string path -> bytes
;; Builds the android package with the given program name and path, producing
;; its apk contents as bytes.
(define (build-android-package program-name program-path)
  (with-temporary-directory
   (lambda (dir)
     (let ([dest (simplify-path (build-path dir program-path))])
       (build-android-package-in-path program-name
                                      program-path
                                      dest)
              
       (get-file-bytes 
        (first (find-files (lambda (a-path)
                             (equal? (filename-extension a-path)
                                     #"apk"))
                           dest)))))))


;; build-android-package-in-path: string path path -> void
;; Builds the android package and produces the binary within the path/bin.
(define (build-android-package-in-path name program-path dest) 
  ;; Prepares the non-assets android package structure.
  (prepare-android-package-src-structure name program-path dest)

  ;; Write out local properties so that the build system knows how to compile
  (write-local.properties dest)

  ;; Write out assets.
  (write-assets name program-path (build-path dest "assets"))
  
  
  (unless (file-exists? (current-ant-bin-path))
    (error 'build-android-package-in-path
           "The Apache ant binary appears to be missing from the current PATH."))
  (unless (directory-exists? (current-android-sdk-path))
    (error 'build-android-package-in-path
           "The Android SDK could not be found."))

  (run-ant-build.xml dest "debug"))


;; write-local.properties: path -> void
;; Write out the prerequisite local.properties file that the android ant build
;; system uses to find Android.
(define (write-local.properties dest)
  (call-with-output-file (build-path dest "local.properties")
    (lambda (op)
      (fprintf op "sdk.dir=~a~n" 
               (path->string (current-android-sdk-path)))
      (fprintf op "sdk-location=~a~n" 
               (path->string (current-android-sdk-path))))
    #:exists 'replace))
  



;; prepare-android-package-src-structure: string path path -> void
;; Prepares the directory structure we need to compile the package.
(define (prepare-android-package-src-structure name program-path dest)  
  (make-directory* dest)
  
  ;; write out phonegap source files so they're included in the compilation.
  (copy-directory/files* phonegap-path dest)
  
  ;; Write out the Java class stubs for the build,
  ;; customizing the phonegap sources for this particular application.
  (write-java-class-stubs name dest)
  ;; Write out the icon.
  (write-icon dest))


;; write-icon: path -> void
;; Write out the res/drawable icon image resource.
(define (write-icon dest)
  (make-directory* (build-path dest "res" "drawable"))
  (copy-or-overwrite-file icon-path 
                          (build-path dest "res" "drawable" "icon.png")))
  

;; write-assets: string path path -> void
;; Write out the assets subdirectory.
(define (write-assets name program-path assets-path)
  (make-directory* assets-path)
  ;; Write out index.html
  (copy-or-overwrite-file (build-path javascript-support-path "index.html")
                          (build-path assets-path "index.html"))
  
  ;; Write out the support javascript files (main.js, evaluator.js)
  (copy-support-js-files assets-path)

  ;; Write out the support runtime.
  (call-with-output-file (build-path assets-path "runtime.js")
    (lambda (op)
      (write-runtime "browser" op))
    #:exists 'replace)
  
  ;; Write out the phonegap support file to assets,
  ;; where it can be packaged.
  (copy-or-overwrite-file (build-path phonegap-path "assets" "phonegap.js") 
                          (build-path assets-path "phonegap.js"))
  
  (let ([module-records (get-compiled-modules program-path)])        
    ;; Finally, write out the Javascript-translated program.
    (write-program.js program-path module-records 
                      assets-path)))



;; write-java-class-stubs: string path -> void
;; Write out the java-related files that we'll need to compile the package.
(define (write-java-class-stubs name dest)
  (let* ([normal-name (normalize-name name)]
         [classname (upper-camel-case normal-name)]
         [package (string-append "plt.moby." classname)])
    
    ;; Put in the customized AndroidMainfest.xml.
    (write-android-manifest 
     (build-path dest "AndroidManifest.xml")
     #:name name
     #:package package
     #:activity-class (string-append package "." classname)
     #:permissions '()
     #;(apply append 
              (map permission->android-permissions
                   (pinfo-permissions 
                    (javascript:compiled-program-pinfo compiled-program)))))
    
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
    (let* ([strings-xml-bytes 
            (get-file-bytes (build-path dest "res" "values" "strings.xml"))]
           [strings-xml-bytes 
            (regexp-replace #rx"DroidGap"
                            strings-xml-bytes
                            (string->bytes/utf-8 (xexpr->string name)))])
      ;; FIXME: don't use regular expressions here!
      (call-with-output-file (build-path dest "res" "values" "strings.xml")
        (lambda (op) (write-bytes strings-xml-bytes op))
        #:exists 'replace))
    
    ;; Rename DroidGap to the application name.
    (make-directory* (build-path dest "src" "plt" "moby" classname))
    (let* ([middleware 
            (get-file-bytes 
             (build-path dest "src" "com" "phonegap" "demo" "DroidGap.java"))]
           [middleware 
            (regexp-replace 
             #rx"package com.phonegap.demo;\n" 
             middleware
             (string->bytes/utf-8 
              (format "package plt.moby.~a;\nimport com.phonegap.demo.*;\n"
                      classname)))]
           [middleware 
            (regexp-replace #rx"DroidGap" 
                            middleware
                            (string->bytes/utf-8 classname))])
      (call-with-output-file (build-path dest "src" "plt" "moby" 
                                         classname 
                                         (format "~a.java" classname))
        (lambda (op)
          (write-bytes middleware op))
        #:exists 'replace)
      (delete-file (build-path dest "src" "com" "phonegap" "demo"
                               "DroidGap.java")))))


  
  
  

;; normalize-name: string -> string
;; Translate a name so it doesn't screw with Java conventions.
(define (normalize-name a-name)
  (let ([a-name (regexp-replace* #px"[^\\w\\s]+" a-name "")])
    (cond
      [(or (= (string-length a-name) 0)
           (not (char-alphabetic? (string-ref a-name 0))))
       (string-append "_" a-name)]
      [else
       a-name])))





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
                              #:permissions (permissions '())
                              #:version-code (version-code "1")
                              #:version-name (version-name "1.0.0"))
  (let ([AndroidManifest.xml
         `(manifest 
           ((xmlns:android "http://schemas.android.com/apk/res/android")
            (package ,package)
            (android:versionCode ,version-code)
            (android:versionName ,version-name))
           
           ;; Building for Android 1.5.
           (uses-sdk ((android:minSdkVersion "3")))
           
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
                      (category ((android:name
                                  "android.intent.category.DEFAULT"))))))])
    
    (xexpr->string AndroidManifest.xml)))




;; subdirectory-of?: boolean
;; Is a-dir a subdirectory of parent-dir?
(define (subdirectory-of? parent-dir -a-dir)
  (let ([parent-dir (normalize-path parent-dir)])
    (let loop ([a-dir (normalize-path -a-dir)])
      (cond [(string=? (path->string parent-dir)
                       (path->string a-dir))
             #t]
            [else
             (let ([new-subdir (normalize-path (simplify-path 
                                                (build-path a-dir 'up)))])
               (cond [(string=? (path->string new-subdir)
                                (path->string a-dir))
                      #f]
                     [else
                      (loop new-subdir)]))]))))



;; make-javascript-directories: path -> void
;; Writes out content into assets.
#;(define (make-javascript-directories dest-dir)
  (make-directory* dest-dir)
  
  ;; Paranoid check: if dest-dir is a subdirectory of
  ;; javascript-support-path, we are in trouble!
  (when (subdirectory-of? javascript-support-path dest-dir)
    (error 'moby "The output directory (~s) must not be a subdirectory of ~s."
           (path->string (normalize-path dest-dir))
           (path->string (normalize-path javascript-support-path))))
  (for ([subpath (list "css")])
    (copy-directory/files* (build-path javascript-support-path subpath) 
                           (build-path dest-dir "assets" subpath)))
 
  )





;; get-compiled-modules: path -> (listof module-record)
;; Given the path of the main program, produce the transitive set of compiled module records.
(define (get-compiled-modules program-path)
  (let*-values ([(a-path) (normalize-path program-path)]
                [(base-dir file dir?) (split-path a-path)]
                [(module-records) (compile-moby-modules a-path)])
    module-records))




;; write-program.js: module-records path-string -> void
;; Write out the module records to program.js
(define (write-program.js a-path module-records dest-dir)
  (call-with-output-file (build-path dest-dir "program.js")
    (lambda (op)
      (write-module-records module-records op)
      
      (for ([r module-records])
        (cond
          [(string=? (path->string (module-record-path r))
                     (if (string? a-path) a-path (path->string a-path)))
           (fprintf op "var programModuleName = ~s;\n\n" 
                    (symbol->string (module-record-name r)))]
          [else
           (void)])))
    #:exists 'replace)
  
  
  
  
  #;(program/resources-write-resources! program/resources dest-dir)
  #;(let*-values ([(program)
                 (program/resources-program program/resources)]
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




(provide/contract [build-android-package 
                   (string? path-string? . -> . bytes?)]
                  [build-android-package-in-path
                   (string? path-string? path-string? . -> . any)]

                  ;; The following will be used when the compiler
                  ;; isn't present on the local machine: we can 
                  ;; still delegate the actual compilation off to a 
                  ;; separate compilation server.
                  [prepare-android-package-src-structure
                   (string? path-string? path-string? . -> . any)]
                  
                  [write-local.properties
                   (path-string? . -> . any)]

                  [write-assets
                   (string? path? path? . -> . any)])