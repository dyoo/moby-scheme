#lang scheme/base
(require scheme/contract)

(define-struct (exn:fail:moby-syntax-error exn:fail) (stxs))

(provide/contract [struct (exn:fail:moby-syntax-error exn:fail) [(message string?)
                                                                 (continuation-marks continuation-mark-set?)
                                                                 (stxs (listof any/c))]])
                          