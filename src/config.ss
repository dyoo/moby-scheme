#lang scheme/base

(require scheme/contract)


(provide/contract [current-ant-bin-path parameter?]
                  [current-android-sdk-path parameter?])
         
;; These parameters may need to be adjusted.

;; current-ant-bin-path: (parameterof file-path)
;; Where the Apache Ant binary is installed.
(define current-ant-bin-path (make-parameter 
                              (cond
                                [(find-executable-path "ant")
                                 => (lambda (a-path)
                                      a-path)]
                                [else
                                 (build-path "/usr/bin/ant")])))

;; current-android-sdk-path: (parameterof directory-path)
;; Where the Google Android SDK is installed.
(define current-android-sdk-path (make-parameter 
                                  (cond
                                    [(find-executable-path "android")
                                     => (lambda (a-path)
                                          (simplify-path
                                           (build-path a-path ".." "..")))]
                                    [else
                                     (build-path "/usr/local/android")])))