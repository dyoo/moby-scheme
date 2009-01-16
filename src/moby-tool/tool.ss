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
      (drscheme:get/extend:extend-unit-frame unit-mixin))

    (define (unit-mixin super%)
      (class super%
        (super-new)))))


  

