#lang scheme/base

(require "../web-application/client.ss"
         "../utils.ss"
         scheme/unit
         scheme/list
         scheme/gui/base
         scheme/class
         drscheme/tool
         mrlib/path-dialog)

(provide tool@)

(define tool@
  (unit
    (import drscheme:tool^)
    (export drscheme:tool-exports^)

    (define (phase1) 
      (void))

    (define (phase2) 
      (install-menu-item))
    
    (define (install-menu-item)
      (drscheme:get/extend:extend-unit-frame menu-introducing-mixin))

    ;; menu-introducing-mixin: (unit-frame% -> unit-frame%)
    ;; Tries to add the menu-item for creating a new application with Moby.
    (define (menu-introducing-mixin super%)
      (class super%
        (inherit get-menu-bar)
        (super-new)
        (when (get-menu-bar)
          (let ([scheme-menu (find-scheme-menu (get-menu-bar))])
            (new separator-menu-item% [parent scheme-menu])
            (new menu-item% 
                 [label "Create Moby Application"]
                 [parent scheme-menu]
                 [callback (lambda (i e)
                             (send (new my-frame% 
                                        [label "Create Moby Application"]
                                        [parent this]
                                        [editor (send this get-editor)])
                                   show #t))])))))
    
    (define my-frame%
      (class frame%
        (init editor)

        (super-new)

        (define -editor editor)
        (define options-panel (new group-box-panel% [parent this] [label "Options"]))
        (define application-name
          (new text-field%
               [parent options-panel]
               [label "Application name"]
               [init-value "hello world"]))
        
        (define platform-box
          (new radio-box% 
               [parent options-panel]
               [label "Platform"]
               [choices (list "J2ME" "Android")]))

 
        (define other-options-panel (new group-box-panel% [parent this] [label "Other options"]))
        (new message%
             [parent other-options-panel]
             [label "You probably do not need to touch these options."])        
        (define server-url
          (new text-field% 
               [parent other-options-panel]
               [label "Server URL"]
               [init-value "http://kfisler-ra1.wpi.edu:8000/servlets/standalone.ss"]))

        
        (define button-panel (new horizontal-panel% [parent this]))

        (new button% [parent button-panel]
             [label "Download Application Locally"]
             [callback (lambda (b e)
                         (let ([app-name (send application-name get-value)]
                               [platform (string-downcase (send platform-box get-item-plain-label
                                                                (send platform-box get-selection)))]
                               [source-code (get-input-port-bytes (open-input-text-editor editor))])
                           (let* ([moby-compile (get-moby-compile (send server-url get-value))]
                                  [name&app-bytes (moby-compile platform app-name source-code)]
                                  [filename (first name&app-bytes)]
                                  [app-bytes (second name&app-bytes)]
                                  [path-dialog
                                   (new path-dialog% 
                                        [label "Save application"]
                                        [message "Choose where to download the application to"]
                                        [parent this]
                                        [filename filename])])
                             (cond [(send path-dialog run) 
                                    => 
                                    (lambda (a-path) 
                                      (call-with-output-file a-path (lambda (op) 
                                                                      (write-bytes app-bytes op)))
                                      
                                                                                                      
                                      (message-box "Application created" 
                                                   (format "Application ~a has been saved to ~a" 
                                                           app-name
                                                           (path->string a-path))))]
                                   [else
                                    (void)]))))])

        
        (new button% [parent button-panel]
             [label "Close"]
             [callback (lambda (b e)
                         (send this show #f))])))))



;; find-scheme-menu: menu-bar -> (U menu #f)
;; Looks for the Scheme menu.  Very hacky.
(define (find-scheme-menu a-menu-bar)
  (let/ec return
    (for ([item (send a-menu-bar get-items)])
      (when (and (is-a? item menu%)
               (string=? (send item get-plain-label) "Scheme"))
        (return item)))
    (return #f)))
          