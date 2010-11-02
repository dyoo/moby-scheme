#lang planet dyoo/js-vm:1:5/js-impl

(require-js "mock-tilt-setup.js")
(require (planet dyoo/js-vm:1:5/ffi/ffi))


;; mock-acceleration-setup: (world number number number -> world) -> js-shutdown-function
(provide mock-acceleration-setup)


;; mock-tilt-setup: (world number number number -> world) -> js-shutdown-function
(provide mock-tilt-setup)

;; mock-shake-setup: (world -> world) -> js-shutdown-function
(provide mock-shake-setup)
