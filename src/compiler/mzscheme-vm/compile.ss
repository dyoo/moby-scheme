#lang scheme/base

(require scheme/list
         "mzscheme-vm.ss"
         "../pinfo.ss"
         "../../stx-helpers.ss"
         "../../../support/externals/mzscheme-vm/src/bytecode-compiler.ss")


;; compile: input-port output-port -> void
(define (compile in out)
  (let* ([stxs (read-syntaxes in)]
         [a-compilation-top 
          (compile-compilation-top-module stxs 
                                          (get-base-pinfo 'moby)
                                          #:name (string->symbol (port-name in)))]
         )
    stxs
    #;(void)))

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