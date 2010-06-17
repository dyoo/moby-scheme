#lang scheme/base

(require scheme/list
         scheme/contract
         "mzscheme-vm.ss"
         "../pinfo.ss"
         "../../stx-helpers.ss"
         "../../../support/externals/mzscheme-vm/src/bytecode-compiler.ss"
         "../../../support/externals/mzscheme-vm/src/sexp.ss")


;; compile: input-port output-port -> void
(define (compile in out)
  (let*-values 
      ([(stxs) (read-syntaxes in)]
       [(a-compilation-top a-pinfo)
        (compile-compilation-top-module stxs 
                                        (get-base-pinfo 'moby)
                                        #:name (string->symbol (port-name in)))]
       [(a-jsexp) (compile-top a-compilation-top)])
    (display (jsexp->js a-jsexp)
             out)))


;; port-name: port -> string
(define (port-name a-port)
  (format "~s" (object-name a-port)))


;; read-syntaxes
(define (read-syntaxes in)
  (port-count-lines! in)
  (map syntax->stx
       (let loop ()
         (let ([stx (read-syntax (object-name in) in)])
           (cond
             [(eof-object? stx)
              empty]
             [else
              (cons stx (loop))])))))


(provide/contract [compile (input-port? output-port? . -> . any)])