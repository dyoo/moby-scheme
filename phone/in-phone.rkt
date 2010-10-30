#lang planet dyoo/js-vm:1:3/base

(require (planet dyoo/js-vm:1:3/ffi/ffi))


;; running-in-phone-context?
;; Produces true if we're running in the context of a phonegap-supporting
;; environment.
(define (running-in-phone-context?)
  (not (js-undefined? (js-get-global-value "Device"))))


(provide running-in-phone-context?)