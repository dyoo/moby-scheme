#lang planet dyoo/js-vm:1:3/base

(require (planet dyoo/js-vm:1:3/permissions/require-permission))
(require (planet dyoo/js-vm:1:3/ffi/ffi))
(require (planet dyoo/js-vm:1:3/jsworld/jsworld))

(require "in-phone.rkt")

(provide ;on-acceleration!
	 on-acceleration
         ;on-shake!
         on-shake
	 ;on-tilt!
	 on-tilt)


(require-permission "PERMISSION:TILT"
		    "PERMISSION:SHAKE")



;; get-accelerometer: -> js-value
;; Gets the accelerometer object.
(define (get-accelerometer)
  (let ([navigator (js-get-global-value "navigator")])
    (js-get-field navigator "accelerometer")))
    



#;(define (on-acceleration! world-updater effect-updater)
  (let ([accelerometer (js-new (js-get-field (js-get-global-value "phonegap") "Accelerometer"))])
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




(define (on-acceleration world-updater)
  (let ([accelerometer (get-accelerometer)])
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
                           (world-updater w x y z)))
                       (lambda (w e)
                         (error 'on-acceleration "an error occured with the accelerometer")))))



#;(define (on-shake! world-updater effect-updater)
  (let ([accelerometer (js-new (js-get-field (js-get-global-value "phonegap") "Accelerometer"))])
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


                     

(define (on-shake world-updater)
  (let ([accelerometer (get-accelerometer)])
    (make-world-config (lambda (success error)
                         (js-call (js-get-field accelerometer "watchShake")
                                  accelerometer
                                  success
                                  error))
                       (lambda (shutdown-f) (js-call shutdown-f #f))
                       world-updater
                       (lambda (w e)
                         (error 'on-shake "an error occured with the accelerometer")))))


#;(define (on-tilt! world-updater effect-updater)
  (let ([accelerometer (js-new (js-get-global-value "Accelerometer"))])
    (make-world-config (lambda (success error)
                         (js-call (js-get-field accelerometer "watchOrientation")
                                  accelerometer
                                  success
                                  error))
                       (lambda (shutdown-f) (js-call shutdown-f #f))
                       (lambda (w js-azimuth js-pitch js-roll)
                         (let ([azimuth (prim-js->scheme js-azimuth)]
                               [pitch (prim-js->scheme js-pitch)]
                               [roll (prim-js->scheme js-roll)])
                           (world-with-effects (effect-updater w azimuth pitch roll)
                                               (world-updater w azimuth pitch roll))))
                       (lambda (w e)
                         (error 'on-tilt! "an error occured with the accelerometer")))))







(define (on-tilt world-updater)
  (let ([accelerometer (get-accelerometer)])
    (make-world-config (lambda (on-change)
			 (let ([shutdown-f
				(js-call (js-get-field 
					  accelerometer
					  "watchOrientation")
					 accelerometer
					 on-change)])
			   shutdown-f))

                       (lambda (shutdown-f)
			 (js-call shutdown-f #f))

		       ;; on-change
                       (lambda (w js-azimuth js-pitch js-roll)
                         (let ([azimuth (prim-js->scheme js-azimuth)]
                               [pitch (prim-js->scheme js-pitch)]
                               [roll (prim-js->scheme js-roll)])
                           (world-updater w azimuth pitch roll))))))
