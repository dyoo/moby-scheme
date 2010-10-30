#lang planet dyoo/js-vm:1:4/js-impl

(require-js "mock-location-setup.js")
(require (planet dyoo/js-vm:1:4/ffi/ffi))



;; mock-location-setup: (world lat lng -> world) -> js-shutdown-function
(provide mock-location-setup)
