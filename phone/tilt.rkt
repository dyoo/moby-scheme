#lang planet dyoo/js-vm:1:4/base

(require (planet dyoo/js-vm:1:4/permissions/require-permission))
(require (planet dyoo/js-vm:1:4/ffi/ffi))
(require (planet dyoo/js-vm:1:4/jsworld/jsworld))

(require "in-phone.rkt")
(require "mock-tilt-setup.rkt")


(provide ;on-acceleration!
	 on-acceleration
         ;on-shake!
         on-shake
	 ;on-tilt!
	 on-tilt)


(require-permission "PERMISSION:TILT"
		    "PERMISSION:SHAKE")









;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Mocks

(define (mock-on-acceleration world-updater)
  (make-world-config (lambda (on-acc)
                       (mock-acceleration-setup on-acc))
                     (lambda (shutdown-f)
                       (shutdown-f))
                     (lambda (w x y z)
                       (world-updater w 
                                      (prim-js->scheme x)
                                      (prim-js->scheme y)
                                      (prim-js->scheme z)))))


(define (mock-on-tilt world-updater)
  (make-world-config (lambda (on-acc)
                       (mock-tilt-setup on-acc))
                     (lambda (shutdown-f)
                       (shutdown-f))
                     (lambda (w x y z)
                       (world-updater w 
                                      (prim-js->scheme x)
                                      (prim-js->scheme y)
                                      (prim-js->scheme z)))))

(define (mock-on-shake world-updater)
  (make-world-config (lambda (on-acc)
                       (mock-shake-setup on-acc))
                     (lambda (shutdown-f)
                       (shutdown-f))
                     (lambda (w)
                       (world-updater w))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(define (phonegap-on-acceleration world-updater)
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





(define (phonegap-on-tilt world-updater)
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



(define (phonegap-on-shake world-updater)
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




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



;; adapt: (world-updater -> world-config) (world-updater -> world-config) -> (world-updater -> world-config)
;; Does the run-time check to see which implementation of a world updater is
;; appropriate.
(define (adapt mock-impl phone-impl)
  (lambda (world-updater)
    (cond
      [(running-in-phone-context?)
       (phone-impl world-updater)]
      [else
       (mock-impl world-updater)])))



;; Here are the provided bindings.

(define on-acceleration (adapt mock-on-acceleration phonegap-on-acceleration))
(define on-tilt (adapt mock-on-tilt phonegap-on-tilt))
(define on-shake (adapt mock-on-shake phonegap-on-shake))
