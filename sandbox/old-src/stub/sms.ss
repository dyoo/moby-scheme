#lang scheme/base
(require lang/prim)


;; send-text-message: string string world -> world
(define (send-text-message an-address
                           a-message
                           a-world)
  (printf "Sending message ~s to ~a~n"
          a-message
          an-address)
  a-world)


(provide-primitive send-text-message)