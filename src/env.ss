#lang s-exp "lang.ss"

(require "permission.ss")


;; An env collects a set of bindings.
(define-struct env (bindings))
(define empty-env (make-env (make-immutable-hasheq '())))


;; A binding associates a symbol with some value.
;; binding?: any -> boolean
(define (binding? datum)
  (or (binding:constant? datum)
      (binding:function? datum)))


;; binding:constant records an id and its associated Java implementation.
(define-struct binding:constant
  (name java-string permissions))


;; Function bindings try to record more information.
(define-struct binding:function

  (name         ;; name of the function
   module-path  ;; path where the function's really defined
   min-arity    ;; minimal arity to call
   var-arity?   ;; is this vararity?
   java-string  ;; the java-string name of the function
   permissions  ;; what permissions do we need to call this function?
   cps?         ;; does the function respect CPS calling conventions?
   ))


;; binding-id: binding -> symbol
(define (binding-id a-binding)
  (cond
    [(binding:constant? a-binding)
     (binding:constant-name a-binding)]
    [(binding:function? a-binding)
     (binding:function-name a-binding)]))



;; env-extend: env binding -> env
(define (env-extend an-env new-binding)
  (make-env (hash-set (env-bindings an-env) (binding-id new-binding) new-binding)))



;; env-lookup: env symbol -> (or/c binding false)
(define (env-lookup an-env name)
  (hash-ref (env-bindings an-env) name false))

;; env-remove: env symbol -> env
(define (env-remove an-env name)
  (hash-remove (env-bindings an-env) name))


;; env-contains?: env symbol -> boolean
(define (env-contains? an-env name)
  (binding? (hash-ref (env-bindings an-env) name false)))
    


;; env-keys: env -> (listof symbol)
;; Produces the keys in the environment.
(define (env-keys an-env)
  (hash-map (env-bindings an-env)
            (lambda (k v)
              k)))




;; env-extend-constant: env symbol string -> env
;; Extends the environment with a new constant binding.
(define (env-extend-constant an-env id java-string)
  (env-extend an-env
              (make-binding:constant id java-string empty)))


;; env-extend-function: env symbol (or/c module-path false) number boolean? string? -> env
;; Extends the environment with a new function binding
(define (env-extend-function an-env id module-path min-arity var-arity? java-string)
  (env-extend an-env
              (make-binding:function id 
                                     module-path
                                     min-arity 
                                     var-arity?
                                     java-string
                                     empty
                                     false)))



(provide/contract 
 [binding? (any/c . -> . boolean?)]
 [struct binding:constant ([name symbol?]
                           [java-string string?]
                           [permissions (listof permission?)])]
 [struct binding:function ([name symbol?]
                           [module-path (or/c false/c path?)]
                           [min-arity natural-number/c]
                           [var-arity? boolean?]
                           [java-string string?]
                           [permissions (listof permission?)]
                           [cps? boolean?])]
 [binding-id (binding? . -> . symbol?)]
 
 
 [struct env ([bindings (listof binding?)])]
 [empty-env env?]
 [env-extend (env? binding? . -> . env?)]
 [env-lookup (env? symbol? . -> . (or/c false/c binding?))]
 [env-remove (env? symbol? . -> . env?)]
 [env-contains? (env? symbol? . -> . boolean?)]
 [env-keys (env? . -> . (listof symbol?))]
 
 [env-extend-constant (env? symbol? string? . -> . env?)]
 [env-extend-function (env? symbol? (or/c false/c path?) number? boolean? string?
                       . -> . env?)])