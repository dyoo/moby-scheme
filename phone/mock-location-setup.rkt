#lang planet dyoo/js-vm:1:5/js-impl

(require-js "mock-location-setup.js")
(require (planet dyoo/js-vm:1:5/ffi/ffi))



;; mock-location-setup: (world lat lng -> world) -> js-shutdown-function
(provide mock-location-setup)
