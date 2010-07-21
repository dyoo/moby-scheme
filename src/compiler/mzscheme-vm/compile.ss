#lang scheme/base

(require scheme/list
         scheme/contract
         "mzscheme-vm.ss"
         "collections-module-resolver.ss"
         "../pinfo.ss"
         (only-in "../helpers.ss" program?)
         "../../stx-helpers.ss"
         "../../../support/externals/mzscheme-vm/src/bytecode-compiler.ss"
         "../../../support/externals/mzscheme-vm/src/sexp.ss")


;; compile: input-port output-port #:name -> pinfo
(define (compile/port in out #:name name)
  (let ([stxs (read-syntaxes in #:name name)])
    (compile/program stxs out #:name name)))



;; compile/program: program output-rp9ort name -> pinfo
(define (compile/program a-program out #:name name)
  (let*-values ([(a-pinfo)
                 (pinfo-update-allow-redefinition? (get-base-pinfo 'moby)
                                                   #f)]
                [(a-pinfo)
                  (pinfo-update-module-resolver a-pinfo 
                                                (extend-module-resolver-with-collections
                                                 (pinfo-module-resolver a-pinfo)))]
                [(a-compilation-top a-pinfo)
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


;; read-syntaxes: input-port #:name symbol -> (listof stx)
(define (read-syntaxes in #:name name)
  (port-count-lines! in)
  (map syntax->stx
       (let loop ()
         (let ([stx (read-syntax name in)])
           (cond
             [(eof-object? stx)
              empty]
             [else
              (cons stx (loop))])))))


(provide/contract [compile/port (input-port? output-port? #:name symbol? . -> . any)]
                  [compile/program (program? output-port? #:name symbol? . -> . any)])