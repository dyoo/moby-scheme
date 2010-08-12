#lang scheme/base

(require scheme/contract
         "mzscheme-vm.ss"
         "collections-module-resolver.ss"
         "../pinfo.ss"
         (only-in "../helpers.ss" program?)
         "../modules.ss"
         "../../compile-helpers.ss"
         "../../../support/externals/mzscheme-vm/src/bytecode-compiler.ss"
         "../../../support/externals/mzscheme-vm/src/sexp.ss")


(define default-base-pinfo (pinfo-update-module-resolver
                            (pinfo-update-allow-redefinition? 
                             (get-base-pinfo 'moby) #f)
                            (extend-module-resolver-with-collections
                             default-module-resolver)))

;; compile: input-port output-port #:name -> pinfo
(define (compile/port in out #:name name #:pinfo (pinfo default-base-pinfo))
  (let ([stxs (read-syntaxes in #:name name)])
    (compile/program stxs out #:name name #:pinfo pinfo)))



;; compile/program: program output-port name -> pinfo
(define (compile/program a-program out 
                         #:name name 
                         #:pinfo (a-pinfo default-base-pinfo))
  (let*-values ([(a-compilation-top a-pinfo)
                 (compile-compilation-top a-program 
                                          a-pinfo
                                          #:name name)]
                [(a-jsexp) (compile-top a-compilation-top)])
    (display (jsexp->js a-jsexp)
             out)
  a-pinfo))
  


;; port-name: port -> string
(define (port-name a-port)
  (format "~s" (object-name a-port)))



(provide/contract [compile/port (input-port? output-port? #:name symbol? . -> . any)]
                  [compile/program (program? output-port? #:name symbol? . -> . any)])