#lang s-exp "lang.ss"

(require "permission.ss")
(require "rbtree.ss")
(require "helpers.ss")


;; An env collects a set of bindings.
(define-struct env (bindings))
(define empty-env (make-env empty-rbtree))


;; A binding associates a symbol with some value.
;; binding?: any -> boolean
(define (binding? datum)
  (or (binding:constant? datum)
      (binding:function? datum)
      (binding:structure? datum)))


;; binding:constant records an id and its associated Java implementation.
(define-struct binding:constant
  (name java-string permissions))


;; Function bindings try to record more information about the toplevel-bound
;; function
(define-struct binding:function

  (name           ;; name of the function
   module-source  ;; source where the function's really defined
   min-arity      ;; minimal arity to call
   var-arity?     ;; is this vararity?
   java-string    ;; the java-string name of the function
   permissions    ;; what permissions do we need to call this function?
   cps?           ;; does the function respect CPS calling conventions?
   ))


;; A binding to a structure.
(define-struct binding:structure
  (name        ;; symbol
   fields      ;; (listof symbol)

   constructor ;; symbol
   predicate   ;; symbol
   accessors   ;; (listof symbol)
   mutators    ;; (listof symbol)
   ))



;; binding-id: binding -> symbol
;; Given a binding, produces its identifier.
(define (binding-id a-binding)
  (cond
    [(binding:constant? a-binding)
     (binding:constant-name a-binding)]
    [(binding:function? a-binding)
     (binding:function-name a-binding)]
    [(binding:structure? a-binding)
     (binding:structure-name a-binding)]))




;; env-extend: env binding -> env
(define (env-extend an-env new-binding)
  (make-env (rbtree-insert symbol< (env-bindings an-env) (binding-id new-binding) new-binding)))



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
 [binding? (any/c . -> . boolean?)]
 
 [struct binding:constant ([name symbol?]
                           [java-string string?]
                           [permissions (listof permission?)])]
 
 [struct binding:function ([name symbol?]
                           [module-source (or/c false/c string?)]
                           [min-arity natural-number/c]
                           [var-arity? boolean?]
                           [java-string string?]
                           [permissions (listof permission?)]
                           [cps? boolean?])]

 [struct binding:structure ([name symbol?]
                            [fields (listof symbol?)]
                            [constructor symbol?]
                            [predicate symbol?]
                            [accessors (listof symbol?)]
                            [mutators (listof symbol?)])]
 
                            
 [binding-id (binding? . -> . symbol?)]
 
 
 [struct env ([bindings (listof binding?)])]
 [empty-env env?]
 [env-extend (env? binding? . -> . env?)]
 [env-lookup (env? symbol? . -> . (or/c false/c binding?))]
 [env-contains? (env? symbol? . -> . boolean?)]
 [env-keys (env? . -> . (listof symbol?))]
 
 [env-extend-constant (env? symbol? string? . -> . env?)]
 [env-extend-function (env? symbol? (or/c false/c string?) number? boolean? string?
                       . -> . env?)])