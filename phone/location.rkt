#lang planet dyoo/js-vm:1:4/base

(require (planet dyoo/js-vm:1:4/permissions/require-permission))
(require (planet dyoo/js-vm:1:4/ffi/ffi))
(require (planet dyoo/js-vm:1:4/jsworld/jsworld))


(require "in-phone.rkt"
	 "mock-location-setup.rkt")


(provide #;on-location-change!
	 on-location-change)


(require-permission "PERMISSION:LOCATION")



;; get-geo: -> js-value
;; Gets the geolocation structure.
(define (get-geo)
  (js-get-field (js-get-global-value "navigator")
		"phonegap_geo"))




(define (on-location-change world-updater)
  (cond
   [(running-in-phone-context?)
    (phonegap-on-location-change world-updater)]
   [else
    (mock-on-location-change world-updater)]))
    


(define (mock-on-location-change world-updater)
  (make-world-config (lambda (location-changed)
		       (mock-location-setup location-changed))
		     (lambda (shutdown-thunk)
		       (shutdown-thunk))
		     (lambda (w lat lng)
                       (world-updater w 
				      (prim-js->scheme lat)
				      (prim-js->scheme lng)))))



(define (phonegap-on-location-change world-updater)
  (let ([geolocation (get-geo)])
    (make-world-config (lambda (success error)
                         (js-call (js-get-field geolocation "watchPosition")
                                  geolocation
                                  success
                                  error))
                       (lambda (id) (js-call (js-get-field geolocation "clearWatch")
                                             geolocation
                                             id))
                       (lambda (w lat lng)
                         (world-updater w
			  		(prim-js->scheme lat)
					(prim-js->scheme lng)))
                       (lambda (w e)
                         (error 'on-location-change "an error occurred with accessing GPS locations")))))




#;(define (on-location-change! world-updater effect-updater)
  (let ([geolocation (get-geo)])
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



