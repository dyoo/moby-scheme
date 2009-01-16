#lang scheme/base

(require "../web-application/client.ss"
         scheme/unit
         scheme/gui/base
         scheme/class
         drscheme/tool)

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
        (define panel (new group-box-panel% [parent this] [label "Options"]))
        (new message% [parent panel] [label "Command me!"])
        
        (define button-panel (new horizontal-panel% [parent this]))
        (new button% [parent button-panel]
             [label "Create Application"]
             [callback (lambda (b e)
                         (display "Create me!")
                         (newline))])
        
        (new button% [parent button-panel]
             [label "Close"]
             [callback (lambda (b e)
                         (send this show #f))])

        ))))




;; find-scheme-menu: menu-bar -> (U menu #f)
;; Looks for the Scheme menu.  Very hacky.
(define (find-scheme-menu a-menu-bar)
  (let/ec return
    (for ([item (send a-menu-bar get-items)])
      (when (and (is-a? item menu%)
               (string=? (send item get-plain-label) "Scheme"))
        (return item)))
    (return #f)))
          