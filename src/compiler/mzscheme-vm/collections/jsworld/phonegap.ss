(provide on-acceleration!
	 on-acceleration
         on-shake!
         on-shake
;;	 on-tilt!
;;	 on-tilt
	 on-location-change!
	 on-location-change
	 on-sms-receive!
	 on-sms-receive)



#;(define (on-tick! world-updater effect-updater interval-seconds)
  (make-world-config (lambda (handler)
                       (js-call (js-get-named-object "setInterval")
                                #f
                                handler
                                (scheme->prim-js (* interval-seconds 1000))))
                     (lambda (id)
                       (js-call (js-get-named-object "clearInterval") #f id))
                     (lambda (w)
                       (world-with-effects (effect-updater w)
                                           (world-updater w)))))

#;(define (on-tick world-updater interval-seconds)
  (make-world-config (lambda (handler)
                       (js-call (js-get-named-object "setInterval")
                                #f
                                handler
                                (scheme->prim-js (* interval-seconds 1000))))
                     (lambda (id)
                       (js-call (js-get-named-object "clearInterval") #f id))
                     world-updater))

(define (on-acceleration! world-updater effect-updater)
  (let ([accelerometer (js-new (js-get-field (js-get-named-object "phonegap") "Accelerometer"))])
    (make-world-config (lambda (success error)
                         (js-call (js-get-field accelerometer "watchAcceleration")
                                  accelerometer
                                  success
                                  error))
                       (lambda (shutdown-f) (js-call shutdown-f #f))
                       (lambda (w js-x js-y js-z)
                         (let ([x (prim-js->scheme js-x)]
                               [y (prim-js->scheme js-y)]
                               [z (prim-js->scheme js-z)])
                           (world-with-effects (effect-updater w x y z)
                                               (world-updater w x y z))))
                       (lambda (w e)
                         (error 'on-acceleration! "an error occured with the accelerometer")))))


(require-permission on-acceleration! "PERMISSION:TILT")


(define (on-acceleration world-updater)
  (let ([accelerometer (js-new (js-get-field (js-get-named-object "phonegap") "Accelerometer"))])
    (make-world-config (lambda (success error)
                         (js-call (js-get-field accelerometer "watchAcceleration")
                                  accelerometer
                                  success
                                  error))
                       (lambda (shutdown-f) (js-call shutdown-f #f))
                       (lambda (w js-x js-y js-z)
                         (let ([x (prim-js->scheme js-x)]
                               [y (prim-js->scheme js-y)]
                               [z (prim-js->scheme js-z)])
                           (effect-updater w x y z)))
                       (lambda (w e)
                         (error 'on-acceleration "an error occured with the accelerometer")))))
(require-permission on-acceleration "PERMISSION:TILT")





(define (on-shake! world-updater effect-updater)
  (let ([accelerometer (js-new (js-get-field (js-get-named-object "phonegap") "Accelerometer"))])
    (make-world-config (lambda (success error)
                         (js-call (js-get-field accelerometer "watchShake")
                                  accelerometer
                                  success
                                  error))
                       (lambda (shutdown-f) (js-call shutdown-f #f))
                       (lambda (w)
                         (world-with-effects (effect-updater w)
                                             (world-updater w)))
                       (lambda (w e)
                         (error 'on-shake! "an error occured with the accelerometer")))))

(require-permission on-shake! "PERMISSION:SHAKE")
                     

(define (on-shake world-updater)
  (let ([accelerometer (js-new (js-get-field (js-get-named-object "phonegap") "Accelerometer"))])
    (make-world-config (lambda (success error)
                         (js-call (js-get-field accelerometer "watchShake")
                                  accelerometer
                                  success
                                  error))
                       (lambda (shutdown-f) (js-call shutdown-f #f))
                       world-updater
                       (lambda (w e)
                         (error 'on-shake "an error occured with the accelerometer")))))
(require-permission on-shake "PERMISSION:SHAKE") 


;; (define (on-tilt! world-updater effect-updater)
;;   (let ([accelerometer (js-new (js-get-named-object "Accelerometer"))])
;;     (make-world-config (lambda (success error)
;;                          (js-call (js-get-field accelerometer "watchOrientation")
;;                                   accelerometer
;;                                   success
;;                                   error))
;;                        (lambda (shutdown-f) (js-call shutdown-f #f))
;;                        (lambda (w js-azimuth js-pitch js-roll)
;;                          (let ([azimuth (prim-js->scheme js-azimuth)]
;;                                [pitch (prim-js->scheme js-pitch)]
;;                                [roll (prim-js->scheme js-roll)])
;;                            (world-with-effects (effect-updater w azimuth pitch roll)
;;                                                (world-updater w azimuth pitch roll))))
;;                        (lambda (w e)
;;                          (error 'on-tilt! "an error occured with the accelerometer")))))
;; (require-permission on-tilt! "PERMISSION:TILT") 


;; (define (on-tilt world-updater)
;;   (let ([accelerometer (js-new (js-get-named-object "Accelerometer"))])
;;     (make-world-config (lambda (success error)
;;                          (js-call (js-get-field accelerometer "watchOrientation")
;;                                   accelerometer
;;                                   success
;;                                   error))
;;                        (lambda (shutdown-f) (js-call shutdown-f #f))
;;                        (lambda (w js-azimuth js-pitch js-roll)
;;                          (let ([azimuth (prim-js->scheme js-azimuth)]
;;                                [pitch (prim-js->scheme js-pitch)]
;;                                [roll (prim-js->scheme js-roll)])
;;                            (world-updater w azimuth pitch roll)))
;;                        (lambda (w e)
;;                          (error 'on-tilt "an error occured with the accelerometer")))))

;; (require-permission on-tilt "PERMISSION:TILT")


(define (on-location-change! world-updater effect-updater)
  (let ([geolocation (js-get-field (js-get-named-object "navigator")
				   "phonegap_geo")])
    (make-world-config (lambda (success error)
                         (js-call (js-get-field geolocation "watchPosition")
                                  geolocation
                                  success
                                  error))
                       (lambda (id) (js-call (js-get-field geolocation "clearWatch")
                                             geolocation
                                             id))
                       (lambda (w lat lng)
                         (world-with-effects (effect-updater w (prim-js->scheme lat) (prim-js->scheme lng))
                                             (world-updater w (prim-js->scheme lat) (prim-js->scheme lng))))
                       (lambda (w e)
                         (error 'on-location-change! "an error occurred with accessing GPS locations")))))

(require-permission on-location-change! "PERMISSION:LOCATION")


(define (on-location-change world-updater)
  (let ([geolocation (js-get-field (js-get-named-object "navigator")
				   "phonegap_geo")])
    (make-world-config (lambda (success error)
                         (js-call (js-get-field geolocation "watchPosition")
                                  geolocation
                                  success
                                  error))
                       (lambda (id) (js-call (js-get-field geolocation "clearWatch")
                                             geolocation
                                             id))
                       (lambda (w lat lng)
                         (world-updater w (prim-js->scheme lat) (prim-js->scheme lng)))
                       (lambda (w e)
                         (error 'on-location-change "an error occurred with accessing GPS locations")))))

(require-permission on-location-change "PERMISSION:LOCATION")


(define (on-sms-receive! world-updater effect-updater)
  (let ([sms (js-new (js-get-field (js-get-named-object "phonegap") "Sms"))])
    (make-world-config (lambda (handler)
                         (js-call (js-get-field sms "addListener")
                                  sms
                                  handler))
                       void ;; FIXME: We need some sort of shutdown here!
                       (lambda (w sender-js-str msg-js-str)
                         (let ([sender (prim-js->scheme sender-js-str)]
                               [msg (prim-js->scheme msg-js-str)])
                         (world-with-effects (effect-updater w sender msg)
                                             (world-updater w sender msg)))))))

(require-permission on-sms-receive! "PERMISSION:SEND-SMS")

(define (on-sms-receive world-updater)
  (let ([sms (js-new (js-get-field (js-get-named-object "phonegap") "Sms"))])
    (make-world-config (lambda (handler)
                         (js-call (js-get-field sms "addListener")
                                  sms
                                  handler))
                       void ;; FIXME: We need some sort of shutdown here!
                       (lambda (w sender-js-str msg-js-str)
                         (let ([sender (prim-js->scheme sender-js-str)]
                               [msg (prim-js->scheme msg-js-str)])
                         (world-updater w sender msg))))))


(require-permission on-sms-receive! "PERMISSION:RECEIVE-SMS")