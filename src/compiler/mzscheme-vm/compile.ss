#lang scheme/base

(require scheme/list
         scheme/contract
         "mzscheme-vm.ss"
         "../pinfo.ss"
         "../../stx-helpers.ss"
         "../../../support/externals/mzscheme-vm/src/bytecode-compiler.ss"
         "../../../support/externals/mzscheme-vm/src/sexp.ss")


;; compile: input-port output-port #:name -> pinfo
(define (compile/port in out #:name name)
  (let*-values 
      ([(stxs) (read-syntaxes in #:name name)]
       [(a-pinfo)
         (pinfo-update-allow-redefinition? (get-base-pinfo 'moby)
                                           #f)]
       [(a-compilation-top a-pinfo)
        (compile-compilation-top stxs 
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


(provide/contract [compile/port (input-port? output-port? #:name symbol? . -> . any)])