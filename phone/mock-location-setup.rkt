#lang s-exp "../js-vm/js-impl.rkt"

(require-js "mock-location-setup.js")
(require "../js-vm/ffi/ffi.rkt")



;; mock-location-setup: (world lat lng -> world) -> js-shutdown-function
(provide mock-location-setup)
