#lang scheme/base

(provide (all-defined-out))
         
;; These parameters may need to be adjusted.

;; current-ant-bin-path: (parameterof path)
;; Where the Apache Ant binary is installed.
(define current-ant-bin-path (make-parameter (build-path "/usr/bin/ant")))

;; current-android-sdk-path: (parameterof path)
;; Where the Google Android SDK is installed.
(define current-android-sdk-path (make-parameter (build-path "/usr/local/android")))

;; current-android-sdk-tools-path: (parameterof path)
;; Where the Google Android SDK tools are installed.
(define current-android-sdk-tools-path (make-parameter (build-path "/usr/local/android/tools")))

;; current-has-android-sdk?: (parameterof boolean)
;; If you don't have the Android SDK, set this to false.
(define current-has-android-sdk?  (make-parameter #t))






;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Ignore the following variables for now; they're deprecated at the moment
;; since the J2ME build broke.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; current-j2me-home: (parameterof path)
;; Where the Sun Wireless SDK is installed.
(define current-j2me-home (make-parameter (build-path "/usr/local/WTK2.5.2")))

;; current-has-sun-wireless-sdk?: (parameterof boolean)
;; If you don't have the Sun Wireless SDK, set this to false.
(define current-has-sun-wireless-sdk?  (make-parameter #f))

