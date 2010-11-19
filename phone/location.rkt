#lang s-exp "../js-vm/base.rkt"

(require "../js-vm/permissions/require-permission.rkt")
(require "../js-vm/ffi/ffi.rkt")
(require "../js-vm/jsworld/jsworld.rkt")


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




(define on-location-change 
  (case-lambda [(world-updater delay)
                (cond
                  [(running-in-phone-context?)
                   (phonegap-on-location-change world-updater)]
                  [else
                   (mock-on-location-change world-updater)])]
               [(world-updater)
                (on-location-change world-updater 60)]))



(define (mock-on-location-change world-updater)
  (make-world-config (lambda (location-changed)
		       (mock-location-setup location-changed))
		     (lambda (shutdown-thunk)
		       (shutdown-thunk))
		     (lambda (w lat lng)
                       (world-updater w 
				      (prim-js->scheme lat)
				      (prim-js->scheme lng)))))




(define (phonegap-on-location-change world-updater delay)
  (let ([geolocation (get-geo)]
    	[options (js-make-hash)])
    (js-set-field! options
		   "frequency"
		   (racket->prim-js (* delay 1000)))
    
    (make-world-config (lambda (success error)
                         (js-call (js-get-field geolocation "watchPosition")
                                  geolocation
                                  success
                                  error
                                  options))
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



