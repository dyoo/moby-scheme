#lang s-exp "../../../private/restricted-runtime-scheme.ss"


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-struct permission:location ())
(define-struct permission:send-sms ())
(define-struct permission:receive-sms ())
(define-struct permission:tilt ())
(define-struct permission:shake ())
(define-struct permission:internet ())
(define-struct permission:telephony ())
(define-struct permission:wake-lock ())
(define-struct permission:vibrate ())
(define-struct permission:foreign-function-interface ())
(define-struct permission:open-image-url (url))
(define-struct permission:universe (url))


(define (permission? datum)
  (or (permission:location? datum)
      (permission:send-sms? datum)
      (permission:receive-sms? datum)
      (permission:tilt? datum)
      (permission:shake? datum)
      (permission:internet? datum)
      (permission:telephony? datum)
      (permission:wake-lock? datum)
      (permission:vibrate? datum)
      (permission:foreign-function-interface? datum)
      (permission:open-image-url? datum)
      (permission:universe? datum)))


(define PERMISSION:LOCATION (make-permission:location))
(define PERMISSION:SEND-SMS (make-permission:send-sms))
(define PERMISSION:RECEIVE-SMS (make-permission:receive-sms))
(define PERMISSION:TILT (make-permission:tilt))
(define PERMISSION:SHAKE (make-permission:shake))
(define PERMISSION:INTERNET (make-permission:internet))
(define PERMISSION:TELEPHONY (make-permission:telephony))
(define PERMISSION:VIBRATE (make-permission:vibrate))
(define PERMISSION:WAKE-LOCK (make-permission:wake-lock))
(define PERMISSION:FOREIGN-FUNCTION-INTERFACE (make-permission:foreign-function-interface))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;; permission->string: permission -> string
;; Translates a permission into a string reference.
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
    [(permission:vibrate? a-permission)
     "PERMISSION:VIBRATE"]
    [(permission:foreign-function-interface? a-permission)
     "PERMISSION:FOREIGN-FUNCTION-INTERFACE"]
    [(permission:open-image-url? a-permission)
     (format "PERMISSION:OPEN-IMAGE-URL ~a" (permission:open-image-url-url a-permission))]
    [(permission:universe? a-permission)
     (format "PERMISSION:UNIVERSE ~a" (permission:universe-url a-permission))]))


;; string->permission: string -> permission
;; Translates a string reference of a permission back into a permission.
(define (string->permission a-ref)
  (local [
          ;; is-permission/1?: string string -> boolean
          ;; Returns true if the reference appears to be a permission of the given
          ;; name
          (define (is-permission/1? permission-name a-ref)
            (and (> (string-length a-ref)
                    (string-length permission-name))
                 (string=? (substring a-ref 0 (string-length permission-name))
                           permission-name)))
          
          ;; construct-permission: string string (string -> permission) -> permission
          ;; Constructs a permission out of a permission string reference.
          (define (construct-permission/1 permission-name a-ref make-permission:*)
            (make-permission:* (substring a-ref 
                                          (add1 (string-length permission-name))
                                          (string-length a-ref))))]
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
      [(string=? a-ref "PERMISSION:VIBRATE")
       PERMISSION:VIBRATE]
      [(string=? a-ref "PERMISSION:FOREIGN-FUNCTION-INTERFACE")
       PERMISSION:FOREIGN-FUNCTION-INTERFACE]
      [(is-permission/1? "PERMISSION:OPEN-IMAGE-URL" a-ref)
       (construct-permission/1 "PERMISSION:OPEN-IMAGE-URL" a-ref make-permission:open-image-url)]
      [(is-permission/1? "PERMISSION:UNIVERSE" a-ref)
       (construct-permission/1 "PERMISSION:UNIVERSE" a-ref make-permission:universe)])))




;; permission->android-permissions: permission -> (listof string)
;; Given a permission, translates to the permissions associated with the Google Android platform.
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
    [(permission:vibrate? a-permission)
     (list "android.permission.VIBRATE")]
    [(permission:foreign-function-interface? a-permission)
     (list)]
    [(permission:open-image-url? a-permission)
     (list)]
    [(permission:universe? a-permission)
     (list "android.permission.INTERNET")]))


(provide/contract [permission? (any/c . -> . boolean?)]
                  
                  [struct permission:location ()]
                  [struct permission:send-sms ()]
                  [struct permission:receive-sms ()]
                  [struct permission:tilt ()]
                  [struct permission:shake ()]
                  [struct permission:internet ()]
                  [struct permission:telephony ()]
                  [struct permission:wake-lock ()]
                  [struct permission:vibrate ()]
                  [struct permission:foreign-function-interface ()]
                  [struct permission:open-image-url ((url string?))]
                  [struct permission:universe ((url string?))]
                  
                  [PERMISSION:LOCATION permission?]
                  [PERMISSION:SEND-SMS permission?]
                  [PERMISSION:RECEIVE-SMS permission?]
                  [PERMISSION:TILT permission?]
                  [PERMISSION:SHAKE permission?]
                  [PERMISSION:INTERNET permission?]
                  [PERMISSION:TELEPHONY permission?]
                  [PERMISSION:WAKE-LOCK permission?]
                  [PERMISSION:VIBRATE permission?]
                  [PERMISSION:FOREIGN-FUNCTION-INTERFACE permission?]
                  
                  [permission->string
                   (permission? . -> . string?)]
                  [string->permission
                   (string? . -> . permission?)]
                  
                  [permission->android-permissions 
                   (permission? . -> . (listof string?))])