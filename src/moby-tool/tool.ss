#lang scheme/base

(require scheme/unit
         scheme/list
         scheme/path
         scheme/gui/base
         scheme/class
         framework/preferences
         drscheme/tool
         mrlib/path-dialog
         "client.ss")

(provide tool@)

(define MOBY-EMAIL 'moby:email-address)



(define tool@
  (unit
    (import drscheme:tool^)
    (export drscheme:tool-exports^)

    (define (phase1) 
      (void))

    (define (phase2) 
      (install-menu-item)
      (preferences:set-default MOBY-EMAIL "your-email@domain.com" string?))

    
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
 
    
    
    ;; get-definition-editor-bytes: editor% -> bytes
    (define (get-definition-editor-bytes an-editor)
      (let* ([next-settings (send an-editor get-next-settings)]
             [lang
              (drscheme:language-configuration:language-settings-language 
               next-settings)]
             [settings
              (drscheme:language-configuration:language-settings-settings
               next-settings)]
             [name-mod (send lang get-reader-module)]
             [result
              (let* ([op (open-output-bytes)])
                (when name-mod
                  (let ([metadata 
                         (send lang get-metadata 
                               (string->symbol
                                (port-name->name 
                                 (send an-editor get-port-name)))
                               settings)])
                    (write-bytes (string->bytes/utf-8 metadata) op))
                  (send an-editor save-port op)
                  (get-output-bytes op)))])
        result))
        
    
    
    (define my-frame%
      (class frame%
        (init editor)

        (super-new)

        (define -editor editor)
        (define options-panel 
          (new group-box-panel% [parent this] [label "Options"]))
        #;(define username-field
          (new text-field%
               [parent options-panel]
               [label "User name"]))

        (define email-field
          (new text-field%
               [parent options-panel]
               [label "E-mail"]
               [init-value (preferences:get MOBY-EMAIL)]))
        
        (define application-name
          (new text-field%
               [parent options-panel]
               [label "Application name"]
               [init-value (port-name->name (send editor get-port-name))]))
        
        (define platform-box
          (new radio-box% 
               [parent options-panel]
               [label "Platform"]
               [choices (list "J2ME" "Android")]))

 
        (define other-options-panel 
          (new group-box-panel% [parent this] [label "Other options"]))
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
                         (let ([username 
                                "PLT" #;(send username-field get-value)]
                               [email (send email-field get-value)]
                               [app-name 
                                (send application-name get-value)]
                               [platform 
                                (string-downcase 
                                 (send platform-box get-item-plain-label
                                       (send platform-box get-selection)))]
                               [source-code 
                                (get-definition-editor-bytes editor)])
                           (preferences:set MOBY-EMAIL email)
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
                         (preferences:set MOBY-EMAIL 
                                          (send email-field get-value))
                         (send this show #f))])))))


  

;; port-name->string: port-name -> string
;; Try to get a reasonable name out of port-name.
(define (port-name->name a-port-name)
  (regexp-replace #px"\\.\\w*$"
                  (cond
                    [(path? a-port-name)
                     (path->string 
                      (file-name-from-path (path->string a-port-name)))]
                    [(symbol? a-port-name)
                     (symbol->string a-port-name)]
                    [else
                     (format "~a" a-port-name)])
                  ""))



;; find-scheme-menu: menu-bar -> (U menu #f)
;; Looks for the Scheme menu.  Very hacky.
;; FIXME: this is totally not the right way to do this.
(define (find-scheme-menu a-menu-bar)
  (let/ec return
    (for ([item (send a-menu-bar get-items)])
      (when (and (is-a? item menu%)
               (string=? (send item get-plain-label) "Scheme"))
        (return item)))
    (return #f)))
