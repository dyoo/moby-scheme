#lang s-exp "lang.ss"

(require "../collects/runtime/binding.ss")
(require "rbtree.ss")
(require "helpers.ss")


;; An env collects a set of bindings.
(define-struct env (bindings))
(define empty-env (make-env empty-rbtree))



;; env-extend: env binding -> env
(define (env-extend an-env new-binding)
  (cond
    [(binding:constant? new-binding)
     (make-env (rbtree-insert symbol< 
                              (env-bindings an-env) 
                              (binding-id new-binding)
                              new-binding))]
    [(binding:function? new-binding)
     (make-env (rbtree-insert symbol< 
                              (env-bindings an-env) 
                              (binding-id new-binding)
                              new-binding))]
    [(binding:structure? new-binding)
     (make-env (rbtree-insert symbol< 
                              (env-bindings an-env) 
                              (binding-id new-binding)
                              new-binding))]))



;; env-lookup: env symbol -> (or/c binding false)
(define (env-lookup an-env name)
  (local [(define result (rbtree-lookup symbol< (env-bindings an-env) name))]
    (cond [(pair? result)
           (second result)]
          [else
           false])))


;; env-contains?: env symbol -> boolean
(define (env-contains? an-env name)
  (binding? (env-lookup an-env name)))
    


;; env-keys: env -> (listof symbol)
;; Produces the keys in the environment.
(define (env-keys an-env)
  (map first (rbtree->list (env-bindings an-env))))




;; env-extend-constant: env symbol string -> env
;; Extends the environment with a new constant binding.
(define (env-extend-constant an-env id java-string)
  (env-extend an-env
              (make-binding:constant id java-string empty)))


;; env-extend-function: env symbol (or/c string false) number boolean? string? -> env
;; Extends the environment with a new function binding
(define (env-extend-function an-env id module-source min-arity var-arity? java-string)
  (env-extend an-env
              (make-binding:function id 
                                     module-source
                                     min-arity 
                                     var-arity?
                                     java-string
                                     empty
                                     false)))



(provide/contract  
 [struct env ([bindings (listof binding?)])]
 [empty-env env?]
 [env-extend (env? binding? . -> . env?)]
 [env-lookup (env? symbol? . -> . (or/c false/c binding?))]
 [env-contains? (env? symbol? . -> . boolean?)]
 [env-keys (env? . -> . (listof symbol?))]
 
 [env-extend-constant (env? symbol? string? . -> . env?)]
 [env-extend-function (env? symbol? (or/c false/c string?) number? boolean? string?
                       . -> . env?)])