#lang s-exp "lang.ss"


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-struct permission () #:transparent)

(define-struct (permission:location permission) () #:transparent)
(define-struct (permission:sms permission) () #:transparent)
(define-struct (permission:tilt permission) () #:transparent)
(define-struct (permission:internet permission) () #:transparent)


(define PERMISSION:LOCATION (make-permission:location))
(define PERMISSION:SMS (make-permission:sms))
(define PERMISSION:TILT (make-permission:tilt))
(define PERMISSION:INTERNET (make-permission:internet))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



;; permission->android-permissions: permission -> (listof string)
(define (permission->android-permissions a-permission)
  (match a-permission
    [(struct permission:location ())
     (list "android.permission.ACCESS_LOCATION"
                                   "android.permission.ACCESS_GPS"
                                   "android.permission.ACCESS_FINE_LOCATION")]
    [(struct permission:sms ())
     (list "android.permission.SEND_SMS")]
    [(struct permission:tilt ())
     (list)]
    [(struct permission:internet ())
     (list "android.permission.INTERNET")]))


;; permission->on-start-code: permission -> string
(define (permission->on-start-code a-permission)
  (match a-permission
    [(struct permission:location ())
     "org.plt.platform.Platform.getInstance().getLocationService().startService();
      org.plt.platform.Platform.getInstance().getLocationService().addLocationChangeListener(listener);"]
    [(struct permission:sms ())
     ""]
    [(struct permission:tilt ())
     "org.plt.platform.Platform.getInstance().getTiltService().startService();
      org.plt.platform.Platform.getInstance().getTiltService().addOrientationChangeListener(listener);
      org.plt.platform.Platform.getInstance().getTiltService().addAccelerationChangeListener(listener);"]
    [(struct permission:internet())
     ""]))


;; permission->on-pause-code: permission -> string
(define (permission->on-pause-code a-permission)
  (match a-permission
    [(struct permission:location ())
     "org.plt.platform.Platform.getInstance().getLocationService().shutdownService();"]
    [(struct permission:sms ())
     ""]
    [(struct permission:tilt ())
     "org.plt.platform.Platform.getInstance().getTiltService().shutdownService();"]
    [(struct permission:internet ())
     ""]))


;; permission->on-destroy-code: permission -> string
(define (permission->on-destroy-code a-permission)
  (match a-permission
    [(struct permission:location ())
     "org.plt.platform.Platform.getInstance().getLocationService().shutdownService();"]
    [(struct permission:sms ())
     ""]
    [(struct permission:tilt ())
     "org.plt.platform.Platform.getInstance().getTiltService().shutdownService();"]
    [(struct permission:internet ())
     ""]))



(provide/contract [struct permission ()]

                  [PERMISSION:LOCATION permission?]
                  [PERMISSION:SMS permission?]
                  [PERMISSION:TILT permission?]
                  [PERMISSION:INTERNET permission?]
                                   
                  [permission->android-permissions 
                   (permission? . -> . (listof string?))]

                  [permission->on-start-code
                   (permission? . -> . string?)]
                  
                  [permission->on-pause-code
                   (permission? . -> . string?)]
                  
                  [permission->on-destroy-code
                   (permission? . -> . string?)]

                  )