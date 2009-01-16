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
                             (display "Command me!")
                             (newline))])))))))




;; find-scheme-menu: menu-bar -> (U menu #f)
;; Looks for the Scheme menu.  Very hacky.
(define (find-scheme-menu a-menu-bar)
  (let/ec return
    (for ([item (send a-menu-bar get-items)])
      (when (and (is-a? item menu%)
               (string=? (send item get-plain-label) "Scheme"))
        (return item)))
    (return #f)))
          