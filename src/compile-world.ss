#lang scheme/base
(require scheme/gui/base
         scheme/contract
         scheme/class
         scheme/list
         scheme/match
         scheme/file
         scheme/runtime-path
         (only-in xml xexpr->string)
         "image-lift.ss"
         "beginner-to-java.ss"
         "utils.ss"
         "template.ss"
         "config.ss")

(provide/contract [generate-j2me-application
                   (string? path-string? path-string? . -> . any)]
                  
                  [generate-android-application
                   (string? path-string? path-string? . -> . any)])


;; A program is a (listof sexp).

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-runtime-path antenna.jar "../support/common/externals/antenna-bin-1.1.0-beta.jar")
(define-runtime-path proguard-home "../support/common/externals/proguard4.2")

(define-runtime-path common-support-src-path "../support/common/src")
(define-runtime-path j2me-support-src-path "../support/j2me/src")
(define-runtime-path stub-path "../support/j2me/MidletStub.java.template")
(define-runtime-path android-skeleton-path "../support/android/skeleton")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;




(define-struct world-handlers (big-bang      ;; big-bang-record
                               on-tick-event ;; id of function
                               on-key-event ;; id of function
                               on-mouse-event ;; id of function
                               on-redraw ;; id of function
                               stop-when ;; id of function
                               ))
(define-struct big-bang-record (width height r world0))
(define EMPTY-WORLD-HANDLERS (make-world-handlers #f ; big-bang
                                                  #f ; on-tick
                                                  #f ; on-key-event
                                                  #f ; on-mouse-event
                                                  #f ; on-redraw
                                                  #f ; stop-when
                                                  ))



;; generate-j2me-app: string path path -> void
;; Given a file written in beginner-level scheme, generates a j2me application.
(define (generate-j2me-application name file dest)
  (compile-world-program-to-j2me (open-beginner-program file) name dest))


(define (generate-android-application name file dest)
  (compile-world-program-to-android (open-beginner-program file) name dest))



;; open-beginner-program: path-string -> text%
;; Opens up the beginner-level program, stripping out the metadata.
(define (open-beginner-program path)
  (define text (new text%))
  (send text insert-file (path->string path))
  ;; delete the metadata at the beginning of the file.
  (send text delete 0 (send text paragraph-start-position 3))
  text)
  



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; compiled-world-program-to-j2me: text% string path-string -> void
;; Consumes a text, an application name, destination directory, and produces an application.
;; The text buffer is assumed to contain a beginner-level program that uses only the world
;; teachpack.  We need to consume a text because we must first lift up all the images
;; as resources.
(define (compile-world-program-to-j2me text name dest-dir)
  (make-j2me-directories dest-dir)
  (write-j2me-resources text name dest-dir)
  (write-java-midlet-source text name (build-path dest-dir "src" "org" "plt" (upper-camel-case name)))
  (run-ant-build.xml dest-dir))

;; make-directories: path -> void
;; Creates the necessary directories.
(define (make-j2me-directories dest-dir)
  (when (directory-exists? dest-dir)
    (delete-directory/files dest-dir))
  (make-directory* dest-dir)
  (make-directory* (build-path dest-dir "src"))
  (make-directory* (build-path dest-dir "res")))


;; write-source-and-resources: text% path -> void
;; Writes out all the external resources we need.
(define (write-j2me-resources a-text name dest-dir)
  (lift-images-to-directory a-text (build-path dest-dir "res"))
  (copy-directory/files* common-support-src-path (build-path dest-dir "src"))
  (copy-directory/files* j2me-support-src-path (build-path dest-dir "src"))
  (write-j2me-ant-buildfile name dest-dir))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



(define (compile-world-program-to-android text name dest-dir)
  (make-android-directories dest-dir)
  (write-android-resources text name dest-dir)
  (write-java-midlet-source text name (build-path dest-dir "src" "org" "plt" (upper-camel-case name)))
  (run-ant-build.xml dest-dir))


(define (make-android-directories dest-dir)
  (when (directory-exists? dest-dir)
    (delete-directory/files dest-dir))
  (make-directory* dest-dir)
  ;; At the moment, we use j2me bridge classes.
  (copy-directory/files* android-skeleton-path dest-dir)
  (copy-directory/files* common-support-src-path (build-path dest-dir "src"))
  (copy-directory/files* j2me-support-src-path (build-path dest-dir "src"))
  (make-directory* (build-path dest-dir "libs")))


(define (write-android-resources a-text a-name dest-dir)
  (lift-images-to-directory a-text (build-path dest-dir "src"))
  (let ([mappings (build-mappings (PROGRAM-NAME (upper-camel-case a-name))
                                  (ANDROID-SDK-PATH (current-android-sdk-path))
                                  (ANDROID-TOOLS-PATH (current-android-sdk-tools-path)))])
    (replace-template-file dest-dir "src/j2ab/android/app/J2ABMIDletActivity.java" mappings)
    (replace-template-file dest-dir "AndroidManifest.xml" mappings)
    (replace-template-file dest-dir "build.xml" mappings)
    (replace-template-file dest-dir "res/values/strings.xml" mappings)
    (replace-template-file dest-dir "src/jad.properties" mappings)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (replace-template-file dest-dir a-path mappings)
  (fill-template-file (build-path dest-dir (string-append a-path ".template"))
                      (build-path dest-dir a-path)
                        mappings)
  (delete-file (build-path dest-dir (string-append a-path ".template"))))






;; write-java-midlet-source: text string path -> void
;; Writes out a java source for the application.
(define (write-java-midlet-source a-text a-name src-path)
  (let*-values ([(classname)
                 (upper-camel-case a-name)]
                [(program a-world-handlers) 
                 (program-extract-world-handlers (parse-text-as-program a-text))]
                [(mappings) 
                 (get-mappings classname program a-world-handlers)])
    (fill-template-file stub-path (build-path src-path (string-append classname ".java")) mappings)))



;; get-mappings: string program world-handlers -> (hashtableof string string)
;; Returns the template mappings we need.
(define (get-mappings classname program a-world-handlers)
  (build-mappings (PROGRAM-NAME classname)
                  (PROGRAM-DEFINITIONS (program->java-string program))
                  (INITIAL-WORLD-EXPRESSION
                   (expression->java-string
                    (big-bang-record-world0
                     (world-handlers-big-bang a-world-handlers))
                    '()))
                  (DELAY-EXPRESSION
                   (expression->java-string 
                    `(* ,(big-bang-record-r 
                          (world-handlers-big-bang a-world-handlers))
                        1000)
                    '()))
                  (STOP-WHEN-EXPRESSION
                   (if (world-handlers-stop-when a-world-handlers)
                       (expression->java-string
                        `(,(world-handlers-stop-when a-world-handlers) world) '())
                       "org.plt.Kernel.no_op_stopWhen(world)"))
                  (ON-TICK-EXPRESSION
                   (if (world-handlers-on-tick-event a-world-handlers)
                       (expression->java-string
                        `(,(world-handlers-on-tick-event a-world-handlers) world) '())
                       "org.plt.Kernel.no_op_worldEvent(world)"))
                  (ON-REDRAW-EXPRESSION
                   (if (world-handlers-on-redraw a-world-handlers)
                       (expression->java-string
                        `(,(world-handlers-on-redraw a-world-handlers) world) '())
                       "org.plt.gui.Scene.emptyScene(this.getWidth(), this.getHeight())"))
                  (ON-KEY-EVENT-EXPRESSION
                   (if (world-handlers-on-key-event a-world-handlers)
                       (expression->java-string
                        `(,(world-handlers-on-key-event a-world-handlers) world aKey) '())
                       "org.plt.Kernel.no_op_keyEvent(world, aKey)"))))




;; parse-text-as-program: text -> program
;; Given a text, returns a program as well.
(define (parse-text-as-program a-text)
  (let* ([ip (open-input-text-editor a-text)]
         [s-exp
          (parameterize ([read-accept-reader #t])
            (let loop ([s-exp (read ip)])
              (cond
                [(eof-object? s-exp)
                 '()]
                [else
                 (cons s-exp (loop (read ip)))])))])
    s-exp))


;; program-extract-world-handlers: program -> (values program world-handlers)
;; Consumes a program, and extracts out the calls to the world handlers.  These are the only
;; places where higher-order function stuff is happening, and that's where we have to
;; intercede.
;; The returned program should have those callbacks removed.
(define (program-extract-world-handlers a-program)
  (let loop  ([a-program a-program]
              [processed-program/rev '()]
              [a-world-handlers EMPTY-WORLD-HANDLERS])
    (define (loop/handler new-world-handlers)
      (loop (rest a-program)
            processed-program/rev
            new-world-handlers))
    (cond 
      [(empty? a-program)
       (values (reverse processed-program/rev)
               a-world-handlers)]
      [else
       (let ([defn-or-expr (first a-program)])
         (cond
           [(defn? defn-or-expr)
            (loop (rest a-program)
                  (cons defn-or-expr processed-program/rev)
                  a-world-handlers)]
           [else
            (match defn-or-expr
              [(list 'big-bang x y tick world0)
               (loop/handler
                (struct-copy 
                 world-handlers a-world-handlers
                 (big-bang (make-big-bang-record x y tick world0))))]
              
              [(list 'on-tick-event tock)
               (loop/handler
                (struct-copy
                 world-handlers a-world-handlers
                 (on-tick-event tock)))]
              
              [(list 'on-key-event change)
               (loop/handler
                (struct-copy
                 world-handlers a-world-handlers
                 (on-key-event change)))]
              
              [(list 'on-mouse-event clack)   
               (loop/handler
                (struct-copy 
                 world-handlers a-world-handlers
                 (on-mouse-event clack)))]
              
              [(list 'on-redraw to-scene)  
               (loop/handler
                (struct-copy 
                 world-handlers a-world-handlers
                 (on-redraw to-scene)))]
              
              [(list 'stop-when last-world?)
               (loop/handler
                (struct-copy 
                 world-handlers a-world-handlers
                 (stop-when last-world?)))]

              [else
               (loop (rest a-program)
                     (cons defn-or-expr processed-program/rev)
                     a-world-handlers)])]))])))





;; lift-images: text path -> void
;; Lifts up the image snips in the text, writing them into the resource directory.
;; The snips in the text will be replaced with the expression (create-image <path>)
;; where path refers to the file saves in the resource directory.
(define (lift-images-to-directory a-text resource-dir)
  (for ([nb (lift-images! a-text)])
    (named-bitmap-save nb resource-dir)))



;; write-j2me-ant-buildfile: string path -> void
;; Writes a build file that's specialized toward building the midlet.
(define (write-j2me-ant-buildfile name dest-dir 
                                  #:cdlc-version [cldc-version "1.1" #;"1.0"]
                                  #:midp-version [midp-version "2.0" #;"1.0"]
                                  )
  (define (property name val)
    `(property ((name ,name) (value ,val))))
  
  (let ([build.xml
         `(project 
           ((name ,(upper-camel-case name)) (default "package"))
           "\n"
           (taskdef ((resource "antenna.properties")
                     (classpath ,(path->string antenna.jar))))
           "\n"
           ,(property "wtk.home" (path->string (current-j2me-home))) "\n"
           ,(property "wtk.proguard.home" (path->string proguard-home)) "\n"
           ,(property "wtk.cldc.version" cldc-version) "\n"
           ,(property "wtk.midp.version" midp-version) "\n"
           ,(property "midlet.name" (upper-camel-case name)) "\n"
           ,(property "company.name" "PLT") "\n"
           "\n"
           (target ((name "init"))
                   (mkdir ((dir "classes")))
                   (mkdir ((dir "bin"))))
           "\n"
           (target ((name "make.jad") (depends "init"))
                   (wtkjad ((jadfile ,(path->string 
                                       (build-path 
                                        "bin" 
                                        (string-append (upper-camel-case name) ".jad"))))
                            (jarfile ,(path->string 
                                       (build-path 
                                        "bin" 
                                        (string-append (upper-camel-case name) ".jar"))))
                            (name "${midlet.name}")
                            (vendor "${company.name}")
                            (version "1.0.0"))
                           (midlet ((name ,(upper-camel-case name))
                                    (class ,(string-append "org.plt." 
                                                           (upper-camel-case name)
                                                           "."
                                                           (upper-camel-case name)))))))
           "\n"
           (target ((name "compile") (depends "init"))
                   (copy ((todir "classes"))
                         (fileset ((dir "src"))
                                  (include ((name "**/*.class")))))
                   (wtkbuild ((srcdir "src")
                              (destdir "classes")
                              (source "1.4")
                              (target "1.4")
                              (preverify "false"))))
           "\n"
           (target ((name "package") (depends "compile,make.jad"))
                   (wtkpackage ((jarfile "bin/${midlet.name}.jar")
                                (jadfile "bin/${midlet.name}.jad")
                                (obfuscate "true")
                                (preverify "true"))
                               (fileset ((dir "classes")))
                               (fileset ((dir "res")))))
           "\n"           
           (target ((name "run") (depends "package"))
                   (wtkrun ((jadfile "bin/${midlet.name}.jad")))))])
    (call-with-output-file (build-path dest-dir "build.xml")
      (lambda (op)
        (display (xexpr->string build.xml) op))
      #:exists 'replace)))




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


;; run-ant-build.xml: path -> void
;; Runs ant to build the program in the destination directory.
;; Assumes the build file is called "build.xml" at the top of the directory.
(define (run-ant-build.xml dest-dir)
  (parameterize ([current-directory dest-dir])
    (let*-values ([(a-subprocess inp outp errp)
                   (subprocess #f #f #f (current-ant-bin-path))]
                  [(t1 t2) 
                   (values (thread (lambda () 
                                     (copy-port-to-debug-log inp)))
                           (thread (lambda ()
                                     (copy-port-to-error-log errp))))])
      (close-output-port outp)
      (subprocess-wait a-subprocess)
      (sync t1)
      (sync t2)
      (void))))