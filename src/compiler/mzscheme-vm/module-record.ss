#lang racket/base

(require scheme/contract)

(require 
 (prefix-in bcode: 
            "../../../support/externals/mzscheme-vm/src/bytecode-structs.ss"))


;; module-record:
;; name is a resolved module path
;; exports is a (listof symbol)
;; impl is an impl
(define-struct module-record (name exports impl))


;; An impl is one of the following:
(define-struct impl ())

;; impl:bytecode:
;; bytecode: is a bytecode-structs compilation-top.
(define-struct (impl:bytecode impl) (bytecode))

;; impl:js: impl
;; js-path is a complete system path.
(define-struct (impl:js impl) (js-path))



(provide/contract 
 [struct module-record ([name resolved-module-path?]
                        [exports (listof symbol?)]
                        [impl impl?])]
 [struct impl ()]
 [struct (impl:bytecode impl) ([bytecode  (or/c bcode:form? bcode:indirect? any/c)])]
 [struct (impl:js impl) ([js-path complete-path?])])