#lang scheme/base
(require scheme/class
         drscheme/tool
         string-constants/string-constant
         scheme/unit)

;; Moby hooks.
(provide tool@)


(define-unit tool@
  (import drscheme:tool^)
  (export drscheme:tool-exports^)
    
  (define (phase1) 
    (void))
 
  (define (phase2)
    (void)))