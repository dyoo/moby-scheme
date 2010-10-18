#lang scheme/base

(require scheme/contract
         (prefix-in local: "local-android-packager.ss")
         (prefix-in remote: "server-side-packager/client-side-packager.ss")
         "../config.ss")



;; local-android-ready?: -> boolean
;; Produces true if we can do local android packaging.
(define (local-android-ready?)
  (and (file-exists? (current-ant-bin-path))
       (directory-exists? (current-android-sdk-path))))
  

;; build-android-package: string path -> bytes
;; Either tries to use the local android packager; if the
;; resources aren't available,
;; then tries to use the web service.
(define (build-android-package program-name program-path)
  (cond
    #;[(local-android-ready?)
     (local:build-android-package program-name program-path)]
    [else
     (remote:build-android-package program-name program-path)]))


(provide/contract
 [build-android-package (string? path? . -> . bytes?)])