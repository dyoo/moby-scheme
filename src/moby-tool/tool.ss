#lang scheme/base

(require scheme/unit
         scheme/list
         scheme/gui/base
         scheme/class
         drscheme/tool
         mrlib/path-dialog
         "client.ss")

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
        (define username-field
          (new text-field%
               [parent options-panel]
               [label "User name"]))

        (define email-field
          (new text-field%
               [parent options-panel]
               [label "E-mail"]))
        
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
               [init-value "http://kfisler-ra1.wpi.edu:8080/"]))

        
        (define button-panel (new horizontal-panel% [parent this]))

        (new button% [parent button-panel]
             [label "Compile Application"]
             [callback (lambda (b e)
                         (let ([username (send username-field get-value)]
                               [email (send email-field get-value)]
                               [app-name 
                                (send application-name get-value)]
                               [platform 
                                (string-downcase 
                                 (send platform-box get-item-plain-label
                                       (send platform-box get-selection)))]
                               [source-code 
                                ;; fixme! not right yet.
                                (bytes-append
                                 #"\n\n\n"
                                 (get-input-port-bytes (open-input-text-editor editor)))])
                           (let* ([moby-compile (get-moby-compile 
                                                 (string-append 
                                                  (send server-url get-value)
                                                  "compile/"))]
                                  [result-path (string-append 
                                                (send server-url get-value)
                                                (moby-compile username
                                                              email
                                                              app-name 
                                                              source-code 
                                                              platform))])
                             (message-box
                              "Application created" 
                              (format "Application ~a has been compiled.  Download it from ~a." 
                                      app-name
                                      result-path)))))])
        
        
        (new button% [parent button-panel]
             [label "Close"]
             [callback (lambda (b e)
                         (send this show #f))])))))


;; get-input-port-bytes: input-port -> bytes
(define (get-input-port-bytes ip)
  (let loop ([b (bytes)])
    (let ([chunk (read-bytes 8196 ip)])
      (cond
        [(eof-object? chunk)
         b]
        [else
         (loop (bytes-append b chunk))]))))
  



;; find-scheme-menu: menu-bar -> (U menu #f)
;; Looks for the Scheme menu.  Very hacky.
(define (find-scheme-menu a-menu-bar)
  (let/ec return
    (for ([item (send a-menu-bar get-items)])
      (when (and (is-a? item menu%)
               (string=? (send item get-plain-label) "Scheme"))
        (return item)))
    (return #f)))
