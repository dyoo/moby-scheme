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
(define-struct permission:open-image-url (url))

(define (permission? datum)
  (or (permission:location? datum)
      (permission:send-sms? datum)
      (permission:receive-sms? datum)
      (permission:tilt? datum)
      (permission:shake? datum)
      (permission:internet? datum)
      (permission:telephony? datum)
      (permission:wake-lock? datum)
      (permission:open-image-url? datum)))


(define PERMISSION:LOCATION (make-permission:location))
(define PERMISSION:SEND-SMS (make-permission:send-sms))
(define PERMISSION:RECEIVE-SMS (make-permission:send-sms))
(define PERMISSION:TILT (make-permission:tilt))
(define PERMISSION:SHAKE (make-permission:shake))
(define PERMISSION:INTERNET (make-permission:internet))
(define PERMISSION:TELEPHONY (make-permission:telephony))
(define PERMISSION:WAKE-LOCK (make-permission:wake-lock))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;; permission->string: permission -> string
(define (permission->string a-permission)
  (cond
    [(permission:location? a-permission)
     "PERMISSION:LOCATION"]
    [(permission:send-sms? a-permission)
     "PERMISSION:SEND-SMS"]
    [(permission:receive-sms? a-permission)
     "PERMISSION:RECEIVE-SMS"]
    [(permission:tilt? a-permission)
     "PERMISSION:TILT"]
    [(permission:shake? a-permission)
     "PERMISSION:SHAKE"]
    [(permission:internet? a-permission)
     "PERMISSION:INTERNET"]
    [(permission:telephony? a-permission)
     "PERMISSION:TELEPHONY"]
    [(permission:wake-lock? a-permission)
     "PERMISSION:WAKE-LOCK"]
    [(permission:open-image-url? a-permission)
     (format "PERMISSION:OPEN-IMAGE-URL ~a" (permission:open-image-url-url a-permission))]))


;; string->permission: string -> permission
(define (string->permission a-ref)
  (cond
    [(string=? a-ref "PERMISSION:LOCATION")
     PERMISSION:LOCATION]
    [(string=? a-ref "PERMISSION:SEND-SMS")
     PERMISSION:SEND-SMS]     
    [(string=? a-ref "PERMISSION:RECEIVE-SMS")
     PERMISSION:RECEIVE-SMS]
    [(string=? a-ref "PERMISSION:TILT")
     PERMISSION:TILT]
    [(string=? a-ref "PERMISSION:SHAKE")
     PERMISSION:SHAKE]
    [(string=? a-ref "PERMISSION:INTERNET")
     PERMISSION:INTERNET]
    [(string=? a-ref "PERMISSION:TELEPHONY")
     PERMISSION:TELEPHONY]
    [(string=? a-ref "PERMISSION:WAKE-LOCK")
     PERMISSION:WAKE-LOCK]
    [(and (> (string-length a-ref)
             (string-length "PERMISSION:OPEN-IMAGE-URL"))
          (string=? (substring a-ref 0 (string-length "PERMISSION:OPEN-IMAGE-URL"))
                    "PERMISSION:OPEN-IMAGE-URL"))
     (make-permission:open-image-url (substring a-ref 
                                                (add1 (string-length "PERMISSION:OPEN-IMAGE-URL"))
                                                (string-length a-ref)))]))




;; permission->android-permissions: permission -> (listof string)
(define (permission->android-permissions a-permission)
  (cond
    [(permission:location? a-permission)
     (list "android.permission.ACCESS_COARSE_LOCATION"
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
     (list "android.permission.WAKE_LOCK")]
    [(permission:open-image-url? a-permission)
     (list)]))


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
     ""]
    [(permission:open-image-url? a-permission)
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
     ""]
    [(permission:open-image-url? a-permission)
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
     ""]
    [(permission:open-image-url? a-permission)
     ""]))



(provide/contract [permission? (any/c . -> . boolean?)]
                  
                  [struct permission:open-image-url ((url string?))]
                  
                  [PERMISSION:LOCATION permission?]
                  [PERMISSION:SEND-SMS permission?]
                  [PERMISSION:RECEIVE-SMS permission?]
                  [PERMISSION:TILT permission?]
                  [PERMISSION:SHAKE permission?]
                  [PERMISSION:INTERNET permission?]
                  [PERMISSION:TELEPHONY permission?]
                  [PERMISSION:WAKE-LOCK permission?]

		  [permission->string
		   (permission? . -> . string?)]
                  [string->permission
		   (permission? . -> . string?)]

                  [permission->android-permissions 
                   (permission? . -> . (listof string?))]
                  
                  [permission->on-start-code
                   (permission? . -> . string?)]
                  
                  [permission->on-pause-code
                   (permission? . -> . string?)]
                  
                  [permission->on-destroy-code
                   (permission? . -> . string?)]
                  
                  )