#lang scheme/base
(require "env.ss"
         scheme/contract)



(define toplevel-env empty-env)


(define (lookup-toplevel-id a-name)
  (env-lookup toplevel-env a-name))

;; register-toplevel-constant!: symbol string -> void
(define (register-toplevel-constant! a-name java-string)
  (set! toplevel-env
        (env-extend-constant toplevel-env a-name java-string)))


;; register-toplevel-function!: symbol (or/c module-path #f) number boolean string -> void
(define (register-toplevel-function! a-name module-path arity vararity? java-string)
  (set! toplevel-env
        (env-extend-function toplevel-env a-name module-path arity vararity? java-string)))




;; get-toplevel-env: -> env
(define (get-toplevel-env)
  toplevel-env)


;; We register the toplevel identifiers here.
(register-toplevel-constant! 'null "org.plt.types.Empty.EMPTY")
(register-toplevel-constant! 'empty "org.plt.types.Empty.EMPTY")
(register-toplevel-constant! 'true "org.plt.types.Logic.TRUE")
(register-toplevel-constant! 'false "org.plt.types.Logic.FALSE")
(register-toplevel-constant! 'eof "org.plt.types.EofObject.EOF")

(for ([kernel-constant '(pi e)])
  (register-toplevel-constant! kernel-constant
                               (format "org.plt.Kernel.~a" 
                                       kernel-constant)))









(provide/contract
 [get-toplevel-env (-> env?)]
 [lookup-toplevel-id (symbol? . -> . (or/c binding? false/c))]
 [register-toplevel-constant! (symbol? string? . -> . void?)]
 [register-toplevel-function! (symbol? (or/c false/c module-path?) number? boolean? string? . -> . void?)])