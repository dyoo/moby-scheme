#lang racket/base

(require racket/gui/base
         racket/unit
         racket/class
         framework
         drracket/tool
         
         (planet dyoo/js-vm:1:3/private/misc)
         (planet dyoo/js-vm:1:3/private/notification-window)
         (planet dyoo/js-vm:1:3/private/log-port)
         "android/android-packager.ss")

;; This tool adds a "Create Phone Package" button to the Racket menu.


(provide tool@)



;; make-reasonable-package-name: path -> string
;; Tries to pick a reasonable default fo(parameterize ([current-log-port notify-port])
                       (with-handlers
                           ([exn:fail? 
                             (lambda (exn)
                               (fprintf notify-port
                                        "An internal error occurred during compilation: ~a\n"
                                        (exn-message exn))
                               (raise exn))])
                         
                         (call-with-output-file output-file
                             (lambda (op) 
                               (fprintf notify-port 
                                        "Writing package to file ~a...\n" output-file)
                               (write-bytes (build-android-package 
                                             (make-package-name output-file)
                                             a-filename)
                                            op))
                             #:exists 'replace)
                         (fprintf notify-port "Done!\n")))r the zip file name.
(define (make-reasonable-package-name a-path)
  (let-values ([(base name dir?)
                (split-path a-path)])
    (string-append (remove-filename-extension name)
                   ".apk")))

;; make-package-name: path -> string
(define (make-package-name a-path)
  (let-values ([(base name dir?)
                (split-path a-path)])
    (remove-filename-extension name)))



(define tool@
  (unit
    (import drracket:tool^)
    (export drracket:tool-exports^)
    
    ;; We're not doing anything language specific, so I don't think we need
    ;; to plug into phase1 or phase2.
    (define (phase1) (void))
    (define (phase2) (void))


    (parameterize ([current-log-port notify-port])
                       (with-handlers
                           ([exn:fail? 
                             (lambda (exn)
                               (fprintf notify-port
                                        "An internal error occurred during compilation: ~a\n"
                                        (exn-message exn))
                               (raise exn))])
                         
                         (call-with-output-file output-file
                             (lambda (op) 
                               (fprintf notify-port 
                                        "Writing package to file ~a...\n" output-file)
                               (write-bytes (build-android-package 
                                             (make-package-name output-file)
                                             a-filename)
                                            op))
                             #:exists 'replace)
                         (fprintf notify-port "Done!\n")))
    ;; unit-frame<%>: interface
    ;; Just a helper interface used for the mixin below.
    (define unit-frame<%> (class->interface drracket:unit:frame%))

    ;; Here we mix in a menu item into the unit frame's Racket menu.
    (drracket:get/extend:extend-unit-frame
     (mixin (unit-frame<%>) (unit-frame<%>)
       (inherit get-language-menu
                get-definitions-text)
    
       
       ;; click!: menu-item% control-event% -> void
       ;; On selection, prompts for a output apk file name, and then writes an apk
       ;; with the contents.
       (define (click! a-menu-item a-control-event)
         (let* ([a-text (get-definitions-text)]
                [a-filename (send a-text get-filename)])
           (cond
             [(not (path-string? a-filename))
              (message-box "Create Phone Package"
                           "Your program needs to be saved first before packaging.")]
             [(send a-text is-modified?)
              (message-box "Create Phone Package"
                           "Your program has changed since your last save or load; please save before packaging.")]
             [else
              (let ([output-file
                     (finder:put-file (make-reasonable-package-name a-filename)
                                      #f
                                      #f
                                      "Where should the Phone package be written to?"
                                      )])
                (cond
                  [(eq? output-file #f)
                   (void)]
                  [else
                   (let ([notify-port 
                          (make-notification-window 
                           #:title "Creating Phone Package")])
                     (parameterize ([current-log-port notify-port])
                       (with-handlers
                           ([exn:fail? 
                             (lambda (exn)
                               (fprintf notify-port
                                        "An internal error occurred during compilation: ~a\n"
                                        (exn-message exn))
                               (raise exn))])
                         
                         (call-with-output-file output-file
                             (lambda (op) 
                               (fprintf notify-port 
                                        "Writing package to file ~a...\n" output-file)
                               (write-bytes (build-android-package 
                                             (make-package-name output-file)
                                             a-filename)
                                            op))
                             #:exists 'replace)
                         (fprintf notify-port "Done!\n"))))]))])))
       
       
       (super-new)
       
       (let ([racket-menu (get-language-menu)])
         (new separator-menu-item% [parent racket-menu])
         (new menu-item% 
              [parent racket-menu]
              [label "Create Phone Package"]
              [callback click!]))))))