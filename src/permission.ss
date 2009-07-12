#lang s-exp "lang.ss"


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-struct permission:location ())
(define-struct permission:send-sms ())
(define-struct permission:receive-sms ())
(define-struct permission:tilt ())
(define-struct permission:internet ())
(define-struct permission:telephony ())

(define (permission? datum)
  (or (permission:location? datum)
      (permission:send-sms? datum)
      (permission:receive-sms? datum)
      (permission:tilt? datum)
      (permission:internet? datum)
      (permission:telephony? datum)))


(define PERMISSION:LOCATION (make-permission:location))
(define PERMISSION:SEND-SMS (make-permission:send-sms))
(define PERMISSION:TILT (make-permission:tilt))
(define PERMISSION:INTERNET (make-permission:internet))
(define PERMISSION:TELEPHONY (make-permission:telephony))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



;; permission->android-permissions: permission -> (listof string)
(define (permission->android-permissions a-permission)
  (cond
    [(permission:location? a-permission)
     (list "android.permission.ACCESS_LOCATION"
           "android.permission.ACCESS_GPS"
           "android.permission.ACCESS_FINE_LOCATION")]
    [(permission:send-sms? a-permission)
     (list "android.permission.SEND_SMS")]
    [(permission:receive-sms? a-permission)
     (list "android.permission.RECEIVE_SMS")]
    [(permission:tilt? a-permission)
     (list)]
    [(permission:internet? a-permission)
     (list "android.permission.INTERNET")]
    [(permission:telephony? a-permission)
     (list "android.permission.ACCESS_COARSE_UPDATES")]))


;; permission->on-start-code: permission -> string
(define (permission->on-start-code a-permission)
  (cond
    [(permission:location? a-permission)
     "plt.platform.Platform.getInstance().getLocationService().startService();
      plt.platform.Platform.getInstance().getLocationService().addLocationChangeListener(listener);"]
    [(permission:send-sms? a-permission)
     ""]
    [(permission:receive-sms? a-permission)
     ""]
    [(permission:tilt? a-permission)
     "plt.platform.Platform.getInstance().getTiltService().startService();
      plt.platform.Platform.getInstance().getTiltService().addOrientationChangeListener(listener);
      plt.platform.Platform.getInstance().getTiltService().addAccelerationChangeListener(listener);
      plt.platform.Platform.getInstance().getTiltService().addShakeListener(listener);"
     ]
    [(permission:internet? a-permission)
     ""]
    [(permission:telephony? a-permission)
     ""]))


;; permission->on-pause-code: permission -> string
(define (permission->on-pause-code a-permission)
  (cond
    [(permission:location? a-permission)
     "plt.platform.Platform.getInstance().getLocationService().shutdownService();"]
    [(permission:send-sms? a-permission)
     ""]
    [(permission:receive-sms? a-permission)
     ""]
    [(permission:tilt? a-permission)
     "plt.platform.Platform.getInstance().getTiltService().shutdownService();"]
    [(permission:internet? a-permission)
     ""]
    [(permission:telephony? a-permission)
     ""]))


;; permission->on-destroy-code: permission -> string
(define (permission->on-destroy-code a-permission)
  (cond
    [(permission:location? a-permission)
     "plt.platform.Platform.getInstance().getLocationService().shutdownService();"]
    [(permission:send-sms? a-permission)
     ""]
    [(permission:receive-sms? a-permission)
     ""]
    [(permission:tilt? a-permission)
     "plt.platform.Platform.getInstance().getTiltService().shutdownService();"]
    [(permission:internet? a-permission)
     ""]
    [(permission:telephony? a-permission)
     ""]))



(provide/contract [permission? (any/c . -> . boolean?)]
                  
                  [PERMISSION:LOCATION permission?]
                  [PERMISSION:SEND-SMS permission?]
                  [PERMISSION:TILT permission?]
                  [PERMISSION:INTERNET permission?]
                  [PERMISSION:TELEPHONY permission?]
                  
                  [permission->android-permissions 
                   (permission? . -> . (listof string?))]
                  
                  [permission->on-start-code
                   (permission? . -> . string?)]
                  
                  [permission->on-pause-code
                   (permission? . -> . string?)]
                  
                  [permission->on-destroy-code
                   (permission? . -> . string?)]
                  
                  )