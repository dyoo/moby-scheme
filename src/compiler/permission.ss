#lang s-exp "lang.ss"


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-struct permission:location ())
(define-struct permission:send-sms ())
(define-struct permission:receive-sms ())
(define-struct permission:tilt ())
(define-struct permission:shake ())
(define-struct permission:internet ())
(define-struct permission:telephony ())
(define-struct permission:wake-lock ())

(define (permission? datum)
  (or (permission:location? datum)
      (permission:send-sms? datum)
      (permission:receive-sms? datum)
      (permission:tilt? datum)
      (permission:shake? datum)
      (permission:internet? datum)
      (permission:telephony? datum)
      (permission:wake-lock? datum)))


(define PERMISSION:LOCATION (make-permission:location))
(define PERMISSION:SEND-SMS (make-permission:send-sms))
(define PERMISSION:RECEIVE-SMS (make-permission:send-sms))
(define PERMISSION:TILT (make-permission:tilt))
(define PERMISSION:SHAKE (make-permission:shake))
(define PERMISSION:INTERNET (make-permission:internet))
(define PERMISSION:TELEPHONY (make-permission:telephony))
(define PERMISSION:WAKE-LOCK (make-permission:wake-lock))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;; permission->symbol: permission -> symbol
(define (permission->symbol a-permission)
  (cond
    [(permission:location? a-permission)
     'PERMISSION:LOCATION]
    [(permission:send-sms? a-permission)
     'PERMISSION:SEND-SMS]
    [(permission:receive-sms? a-permission)
     'PERMISSION:RECEIVE-SMS]
    [(permission:tilt? a-permission)
     'PERMISSION:TILT]
    [(permission:shake? a-permission)
     'PERMISSION:SHAKE]
    [(permission:internet? a-permission)
     'PERMISSION:INTERNET]
    [(permission:telephony? a-permission)
     'PERMISSION:TELEPHONY]
    [(permission:wake-lock? a-permission)
     'PERMISSION:WAKE-LOCK]))


;; symbol->permission: symbol -> permission
(define (symbol->permission a-ref)
  (cond
    [(symbol=? a-ref 'PERMISSION:LOCATION)
     PERMISSION:LOCATION]
    [(symbol=? a-ref 'PERMISSION:SEND-SMS)
     PERMISSION:SEND-SMS]     
    [(symbol=? a-ref 'PERMISSION:RECEIVE-SMS)
     PERMISSION:RECEIVE-SMS]
    [(symbol=? a-ref 'PERMISSION:TILT)
     PERMISSION:TILT]
    [(symbol=? a-ref 'PERMISSION:SHAKE)
     PERMISSION:SHAKE]
    [(symbol=? a-ref 'PERMISSION:INTERNET)
     PERMISSION:INTERNET]
    [(symbol=? a-ref 'PERMISSION:TELEPHONY)
     PERMISSION:TELEPHONY]
    [(symbol=? a-ref 'PERMISSION:WAKE-LOCK)
     PERMISSION:WAKE-LOCK]))
  



;; permission->android-permissions: permission -> (listof string)
(define (permission->android-permissions a-permission)
  (cond
    [(permission:location? a-permission)
     (list "android.permission.ACCESS_LOCATION"
           "android.permission.ACCESS_GPS"
	   "android.permission.ACCESS_COARSE_LOCATION"
           "android.permission.ACCESS_FINE_LOCATION")]
    [(permission:send-sms? a-permission)
     (list "android.permission.SEND_SMS")]
    [(permission:receive-sms? a-permission)
     (list "android.permission.RECEIVE_SMS")]
    [(permission:tilt? a-permission)
     (list)]
    [(permission:shake? a-permission)
     (list)]
    [(permission:internet? a-permission)
     (list "android.permission.INTERNET")]
    [(permission:telephony? a-permission)
     (list "android.permission.ACCESS_COARSE_UPDATES")]
    [(permission:wake-lock? a-permission)
     (list "android.permission.WAKE_LOCK")]))


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
   [(permission:shake? a-permission)
     ""
     ] 
    [(permission:internet? a-permission)
     ""]
    [(permission:telephony? a-permission)
     ""]
    [(permission:wake-lock? a-permission)
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
    [(permission:shake? a-permission)
     ""]
    [(permission:internet? a-permission)
     ""]
    [(permission:telephony? a-permission)
     ""]
    [(permission:wake-lock? a-permission)
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
    [(permission:shake? a-permission)
     ""]
    [(permission:internet? a-permission)
     ""]
    [(permission:telephony? a-permission)
     ""]
    [(permission:wake-lock? a-permission)
     ""]))



(provide/contract [permission? (any/c . -> . boolean?)]
                  
                  [PERMISSION:LOCATION permission?]
                  [PERMISSION:SEND-SMS permission?]
                  [PERMISSION:TILT permission?]
                  [PERMISSION:INTERNET permission?]
                  [PERMISSION:TELEPHONY permission?]
                  [PERMISSION:WAKE-LOCK permission?]
                  
		  [permission->symbol
		   (permission? . -> . symbol?)]
                  [symbol->permission
		   (permission? . -> . symbol?)]

                  [permission->android-permissions 
                   (permission? . -> . (listof string?))]
                  
                  [permission->on-start-code
                   (permission? . -> . string?)]
                  
                  [permission->on-pause-code
                   (permission? . -> . string?)]
                  
                  [permission->on-destroy-code
                   (permission? . -> . string?)]
                  
                  )