#lang scheme/base
(provide (all-defined-out))
(require syntax/modresolve
         scheme/gui/base
         scheme/match
         scheme/file
         scheme/class
         "config.ss"
         "image-lift.ss"
         "helpers.ss"
         "pinfo.ss"
         "env.ss"
         "permission.ss")

;; Common helper functions used in the compiler.


(define WORLD-PATH-1 (resolve-module-path '(lib "world.ss" "moby" "stub") #f))
(define WORLD-PATH-2 (resolve-module-path '(lib "world.ss" "teachpack" "htdp") #f))
(define GUI-WORLD-PATH (resolve-module-path '(lib "gui-world.ss" "gui-world") #f))



;; A platform is one of PLATFORM:ANDROID, PLATFORM:J2ME.
(define PLATFORM:ANDROID 'android)
(define PLATFORM:J2ME 'j2me)


;; A stub is one of the following
(define STUB:WORLD 'stub:world)
(define STUB:GUI-WORLD 'stub:gui-world)

;; stub=?: stub stub -> boolean
(define (stub=? stub-1 stub-2)
  (eq? stub-1 stub-2))


;; parse-text-as-program: text -> program
;; Given a text, returns a program as well.
(define (parse-text-as-program a-text)
  (let* ([ip (open-input-text-editor a-text)])
    (parameterize ([read-accept-reader #t])
      (let ([s-exp (read ip)])
        (match s-exp
          [(list 'module name lang body ...)
           ;; FIXME: check that the language is beginner level!
           body])))))



;; choose-program-stub: pinfo -> stub
;; Returns the stub necessary to compile this program.
(define (choose-program-stub a-pinfo)
  (let/ec return
    (for ([b (pinfo-used-bindings a-pinfo)])
      (cond
        [(and (binding:function? b)
              (binding:function-module-path b))
         (cond
           [(or (path=? (binding:function-module-path b)
                        WORLD-PATH-1)
                (path=? (binding:function-module-path b)
                        WORLD-PATH-2))
            (return STUB:WORLD)]
           [(path=? (binding:function-module-path b)
                    GUI-WORLD-PATH)
            (return STUB:GUI-WORLD)])]))
      (error 'choose-program-stub "Couldn't identify stub to use for this program.")))





;; get-permissions: pinfo -> (listof permission)
(define (get-permissions a-pinfo)
  (define ht (make-hash))
  (for ([b (pinfo-used-bindings a-pinfo)])
    (cond
      [(binding:function? b)
       (for ([p (binding:function-permissions b)])
         (hash-set! ht p #t))]))
  (for/list ([p (in-hash-keys ht)])
    p))
  
;; get-on-start-code: pinfo -> string
(define (get-on-start-code a-pinfo)
  (apply string-append
         (map permission->on-start-code (get-permissions a-pinfo))))

;; get-on-pause-code: pinfo -> string
(define (get-on-pause-code a-pinfo)
  (apply string-append
         (map permission->on-pause-code (get-permissions a-pinfo))))

;; get-on-shutdown-code: pinfo -> string
(define (get-on-destroy-code a-pinfo)
  (apply string-append
         (map permission->on-destroy-code (get-permissions a-pinfo))))
  






;; lift-images: text path -> (listof named-bitmap)
;; Lifts up the image snips in the text, writing them into the resource directory.
;; The snips in the text will be replaced with the expression (create-image <path>)
;; where path refers to the file saves in the resource directory.
(define (lift-images-to-directory a-text resource-dir)
  (make-directory* resource-dir)
  (let ([named-bitmaps (lift-images! a-text)])
    (for ([nb named-bitmaps])
      (named-bitmap-save nb resource-dir))
    named-bitmaps))



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


;; open-beginner-program: path-string -> text%
;; Opens up the beginner-level program.
(define (open-beginner-program path)
  (define text (new text%))
  (send text insert-file (path->string path))
  text)


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
