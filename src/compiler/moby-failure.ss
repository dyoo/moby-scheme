#lang scheme/base


(define-struct (moby-failure exn:fail) (val))

(provide (all-defined-out))
