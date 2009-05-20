#lang scheme/base

(require scheme/contract
         scheme/match
         scheme/list
         "permission.ss")


;; An env collects a set of bindings.
(define-struct env (bindings))
(define empty-env (make-env (make-immutable-hasheq '())))


;; A binding associates a symbol with some value.
(define-struct binding ())


;; binding:constant records an id and its associated Java implementation.
(define-struct (binding:constant binding) 
  (name java-string permissions))


;; Function bindings try to record more information.
(define-struct (binding:function binding) 

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
  (match a-binding
    [(struct binding:constant (name-2 _ _))
     name-2]
    [(struct binding:function (name-2 _ _ _ _ _ _))
     name-2]))



;; env-extend: env binding -> env
(define (env-extend an-env new-binding)
  (make-env (hash-set (env-bindings an-env) (binding-id new-binding) new-binding)))



;; env-lookup: env symbol -> (or/c binding #f)
(define (env-lookup an-env name)
  (match an-env
    [(struct env (bindings))
     (hash-ref (env-bindings an-env) name #f)]))
       

;; env-keys: env -> (listof symbol)
;; Produces the keys in the environment.
(define (env-keys an-env)
  (match an-env
    [(struct env (bindings))
     (for/list ([k (in-hash-keys bindings)])
       k)]))




;; env-extend-constant: env symbol string -> env
;; Extends the environment with a new constant binding.
(define (env-extend-constant an-env id java-string)
  (env-extend an-env
              (make-binding:constant id java-string empty)))


;; env-extend-function: env symbol (or/c module-path #f) number boolean? string? -> env
;; Extends the environment with a new function binding
(define (env-extend-function an-env id module-path min-arity var-arity? java-string 
                             #:cps? (cps? #f))
  (env-extend an-env
              (make-binding:function id 
                                     module-path
                                     min-arity 
                                     var-arity?
                                     java-string
                                     empty
                                     cps?)))



(provide/contract 
 [struct binding ()]
 [struct (binding:constant binding) ([name symbol?]
                                     [java-string string?]
                                     [permissions (listof permission?)])]
 [struct (binding:function binding) ([name symbol?]
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
 [env-keys (env? . -> . (listof symbol?))]
 
 [env-extend-constant (env? symbol? string? . -> . env?)]
 [env-extend-function ((env? symbol? (or/c false/c path?) number? boolean? string?)
                       (#:cps? boolean?)
                       . ->* . env?)])