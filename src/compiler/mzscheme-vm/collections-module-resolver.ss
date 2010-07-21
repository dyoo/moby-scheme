#lang scheme/base

;; Collections module resolver: resolve the built-in collections
(require scheme/contract
         scheme/runtime-path
         "../../collects/moby/runtime/binding.ss")


;; extend-module-resolver-with-collections: module-resolver -> module-resolver
;; Creates an extended resolver.
(define (extend-module-resolver-with-collections a-resolver)
  (lambda (module-name)
    (printf "Trying to resolve ~s\n" module-name)
    (a-resolver module-name)))


(define module-resolver/c
  (module-name? . -> . (or/c module-binding? false/c)))


(provide/contract [extend-module-resolver-with-collections
                   (module-resolver/c . -> . module-resolver/c)] )
                   