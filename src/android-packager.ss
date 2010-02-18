#lang scheme/base

(require scheme/contract
         (prefix-in local: "local-android-packager.ss")
         "program-resources.ss")


;; build-android-package: string program/resources -> bytes
(define (build-android-package program-name program/resources)
  (cond
    [else
     (local:build-android-package program-name program/resources)]))



(provide/contract
 [build-android-package (string? program/resources? . -> . bytes?)])