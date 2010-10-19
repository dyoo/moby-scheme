#lang racket/base

(require racket/list
         racket/contract)

;; permission->android-permissions: string (-> X)-> (or/c (listof string) X)
;; Given a permission, translates to the permissions associated with the Google Android platform.
(define (permission->android-permissions a-permission on-fail)
  (define mapping '(["PERMISSION:LOCATION" "android.permission.ACCESS_COARSE_LOCATION"
                                           "android.permission.ACCESS_FINE_LOCATION"]
                    ["PERMISSION:SEND-SMS" "android.permission.SEND_SMS"]
                    ["PERMISSION:RECEIVE-SMS" "android.permission.RECEIVE_SMS"]
                    ["PERMISSION:TILT"]
                    ["PERMISSION:SHAKE"]
                    ["PERMISSION:INTERNET" "android.permission.INTERNET"]
                    ["PERMISSION:TELEPHONY" "android.permission.ACCESS_COARSE_UPDATES"]
                    ["PERMISSION:WAKE-LOCK" "android.permission.WAKE_LOCK"]
                    ["PERMISSION:VIBRATE" "android.permission.VIBRATE"]
                    ["PERMISSION:FFI"]))
  (cond [(assoc a-permission mapping) 
         =>
         (lambda (entry) (rest entry))]
        [else
         (on-fail)]))

(provide/contract [permission->android-permissions (string? (-> any/c) 
                                                            . -> . 
                                                            (or/c any/c (listof string?)))])