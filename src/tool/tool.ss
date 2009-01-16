#lang scheme/base

(require "../web-application/client.ss"
         scheme/unit
         scheme/gui/base
         drscheme/tool)

(provide tool@)

(define tool@
  (unit
    (import drscheme:tool^)
    (export drscheme:tool-exports^)
    (define (phase1) (message-box "Moby" "phase1"))
    (define (phase2) (message-box "Moby" "phase2"))
    (message-box "Moby" "unit invoked")))