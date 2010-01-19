#lang s-exp "private/restricted-runtime-scheme.ss"


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


(provide/contract
 [binding? (any/c . -> . boolean?)]
 
 [struct binding:constant ([name symbol?]
                           [java-string string?]
                           [permissions (listof any/c #;permission?)])]
 
 [struct binding:function ([name symbol?]
                           [module-source (or/c false/c string?)]
                           [min-arity natural-number/c]
                           [var-arity? boolean?]
                           [java-string string?]
                           [permissions (listof any/c #;permission?)]
                           [cps? boolean?])]

 [struct binding:structure ([name symbol?]
                            [fields (listof symbol?)]
                            [constructor symbol?]
                            [predicate symbol?]
                            [accessors (listof symbol?)]
                            [mutators (listof symbol?)])]
 
                            
 [binding-id (binding? . -> . symbol?)])
