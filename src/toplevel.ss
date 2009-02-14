#lang scheme/base

(require scheme/contract)

 


;; registered-toplevel-ids: (parameter (hashtable-of symbol (symbol -> string))
;; Keeps a mapping from symbols to functions that do the translation to Java.
(define registered-toplevel-ids (make-parameter (make-hasheq)))

(define-struct id-info ())
(define-struct (id-info:constant id-info) 
  (name ->java-string))
(define-struct (id-info:function id-info) 
  (name min-arity var-arity? ->java-string))

;; register-toplevel-id-constant: symbol (symbol -> string) -> void
;; Registers a translator for the use of a toplevel id constant.
(define (register-plain-toplevel-id-constant! id java-string)
  (when (hash-ref (registered-toplevel-ids) id)
    (error 'register-toplevel-id-constant! "~s already bound" id))
  (hash-set! (registered-toplevel-ids) 
             id 
             (make-id-info:constant id (lambda (sym) java-string))))


;; register-toplevel-id-function!: symbol number boolean? ??? -> void
(define (register-toplevel-id-function! id min-arity var-arity? ->java-string)
  (when (hash-ref (registered-toplevel-ids) id)
    (error 'register-toplevel-id-constant! "~s already bound" id))
  (hash-set! (registered-toplevel-ids)
             id
             (make-id-info:function id min-arity var-arity? ->java-string)))


;; lookup-toplevel-id: symbol -> (or/c #f id-info)
(define (lookup-toplevel-id an-id)
  (hash-ref (registered-toplevel-ids) an-id #f))



(provide/contract 
 [struct id-info ()]
 [struct (id-info:constant id-info) ([name symbol?]
                                     [->java-string (symbol? . -> . string?)])]
 [register-plain-toplevel-id-constant! (symbol? string? . -> . any)]
 [lookup-toplevel-id (symbol? . -> . (or/c id-info? false/c))])
