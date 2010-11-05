#lang s-exp "../js-vm/base.rkt"

(require "../js-vm/ffi/ffi")


;; running-in-phone-context?
;; Produces true if we're running in the context of a phonegap-supporting
;; environment.
(define (running-in-phone-context?)
  (not (js-undefined? (js-get-global-value "Device"))))


(provide running-in-phone-context?)