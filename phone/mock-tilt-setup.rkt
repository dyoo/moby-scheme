#lang s-exp "../js-vm/js-impl.rkt"

(require-js "mock-tilt-setup.js")
(require "../js-vm/ffi/ffi.rkt")


;; mock-acceleration-setup: (world number number number -> world) -> js-shutdown-function
(provide mock-acceleration-setup)


;; mock-tilt-setup: (world number number number -> world) -> js-shutdown-function
(provide mock-tilt-setup)

;; mock-shake-setup: (world -> world) -> js-shutdown-function
(provide mock-shake-setup)
