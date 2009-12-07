#lang s-exp "lang.ss"

(require "env.ss")
(require "toplevel.ss")
(require "helpers.ss")
(require "permission.ss")
(require "modules.ss")
(require "rbtree.ss")



;; pinfo (program-info) is the "world" structure for the compilers; 
;; it captures the information we get from analyzing and compiling
;; the program, and also maintains some auxillary structures.
(define-struct pinfo (env                    ; env
                      modules                ; (listof module-binding) 
                      used-bindings-hash     ; (hashof symbol binding)
                      gensym-counter         ; number
                      
                      ;; names that aren't allowed to be re-extended.
                      enduring-names         ; (listof symbol)
                      shared-expressions     ; (hashof expression labeled-translation)
                      ))


;; empty-pinfo: pinfo
;; An empty pinfo that doesn't know any toplevel environment bindings.
(define empty-pinfo
  (make-pinfo empty-env
              empty 
              empty-rbtree
              0
              empty
              empty-rbtree))




;; pinfo-used-bindings: pinfo -> (listof binding)
;; Returns the list of used bindings computed from the program analysis.
(define (pinfo-used-bindings a-pinfo)
  (map second (rbtree->list (pinfo-used-bindings-hash a-pinfo))))



(define (pinfo-clear-enduring-names a-pinfo)
  (make-pinfo (pinfo-env a-pinfo)
              (pinfo-modules a-pinfo)
              (pinfo-used-bindings-hash a-pinfo)
              (pinfo-gensym-counter a-pinfo)
              empty
              (pinfo-shared-expressions a-pinfo)))


;; pinfo-update-env: pinfo env -> pinfo
;; Updates the env of a pinfo.
(define (pinfo-update-env a-pinfo an-env)
  (make-pinfo
   an-env
   (pinfo-modules a-pinfo)
   (pinfo-used-bindings-hash a-pinfo)
   (pinfo-gensym-counter a-pinfo)
   (pinfo-enduring-names a-pinfo)
   (pinfo-shared-expressions a-pinfo)))


(define (pinfo-accumulate-shared-expression a-shared-expression a-labeled-translation a-pinfo)
  (make-pinfo (pinfo-env a-pinfo)
              (pinfo-modules a-pinfo)
              (pinfo-used-bindings-hash a-pinfo)
              (pinfo-gensym-counter a-pinfo)
              (pinfo-enduring-names a-pinfo)
              (rbtree-insert expression<? 
                             (pinfo-shared-expressions a-pinfo) 
                             a-shared-expression 
                             a-labeled-translation)))

                                            

;; pinfo-accumulate-binding: binding pinfo -> pinfo
;; Adds a new binding to a pinfo's set.
(define (pinfo-accumulate-binding a-binding a-pinfo)
  (make-pinfo
   (env-extend (pinfo-env a-pinfo) a-binding)
   (pinfo-modules a-pinfo)
   (pinfo-used-bindings-hash a-pinfo)
   (pinfo-gensym-counter a-pinfo)
   (pinfo-enduring-names a-pinfo)
   (pinfo-shared-expressions a-pinfo)))


;; pinfo-accumulate-bindings: (listof binding) pinfo -> pinfo
;; Adds a list of bindings to the pinfo's set.
(define (pinfo-accumulate-bindings bindings a-pinfo)
  (foldl pinfo-accumulate-binding
         a-pinfo
         bindings))


;; pinfo-accumulate-module: module-binding pinfo -> pinfo
;; Adds a module to the pinfo's set.
(define (pinfo-accumulate-module a-module a-pinfo)
  (make-pinfo (pinfo-env a-pinfo)
              (cons a-module (pinfo-modules a-pinfo))
              (pinfo-used-bindings-hash a-pinfo)
              (pinfo-gensym-counter a-pinfo)
              (pinfo-enduring-names a-pinfo)
              (pinfo-shared-expressions a-pinfo)))


;; pinfo-accumulate-binding-use: binding pinfo -> pinfo
;; Adds a binding's use to a pinfo's set.
(define (pinfo-accumulate-binding-use a-binding a-pinfo)
  (make-pinfo (pinfo-env a-pinfo)
              (pinfo-modules a-pinfo)
              (rbtree-insert symbol<
                             (pinfo-used-bindings-hash a-pinfo)
                             (binding-id a-binding)
                             a-binding)
              (pinfo-gensym-counter a-pinfo)
              (pinfo-enduring-names a-pinfo)
              (pinfo-shared-expressions a-pinfo)))


;; pinfo-gensym: pinfo symbol -> (list pinfo symbol)
;; Generates a unique symbol.
(define (pinfo-gensym a-pinfo a-label)
  (list (make-pinfo (pinfo-env a-pinfo)
                    (pinfo-modules a-pinfo)
                    (pinfo-used-bindings-hash a-pinfo)
                    (add1 (pinfo-gensym-counter a-pinfo))
                    (pinfo-enduring-names a-pinfo)
                    (pinfo-shared-expressions a-pinfo))

        (string->symbol
         (string-append (symbol->string a-label)
                        (number->string (pinfo-gensym-counter a-pinfo))))))

   


;; pinfo-permissions: pinfo -> (listof permission)
;; Given a pinfo, collect the list of permissions.
(define (pinfo-permissions a-pinfo)
  (local [;; unique: (listof X) -> (listof X)
          (define (unique lst)
            (cond [(empty? lst)
                   empty]
                  [(member? (first lst)
                            (rest lst))
                   (unique (rest lst))]
                  [else
                   (cons (first lst)
                         (unique (rest lst)))]))
          ;; member?: X (listof X) -> boolean
          (define (member? x lst)
            (cond
              [(empty? lst)
               false]
              [(eq? (first lst) x)
               true]
              [else
               (member? x (rest lst))]))]
    (unique
     (foldl (lambda (a-binding permissions)
              (cond [(binding:function? a-binding)
                     (append (binding:function-permissions a-binding)
                             permissions)]
                    [(binding:constant? a-binding)
                     (append (binding:constant-permissions a-binding)
                             permissions)]))
            empty
            (pinfo-used-bindings a-pinfo)))))



;; get-base-pinfo: pinfo symbol -> pinfo
;; Returns a pinfo that knows the base definitions.
;; Language can be one of the following:
;; 'base
;; 'moby
(define (get-base-pinfo language)
  (cond
    [(symbol=? language 'moby)
     (pinfo-update-env empty-pinfo
                       (extend-env/module-binding (get-toplevel-env language)
                                                  moby-module-binding))]
    [(symbol=? language 'base)
     (pinfo-update-env empty-pinfo
                       (get-toplevel-env language))]))



(provide/contract [struct pinfo ([env env?]
                                 [modules (listof module-binding?)]
                                 [used-bindings-hash hash?]
                                 [gensym-counter number?]
                                 [enduring-names (listof symbol?)]
                                 [shared-expressions hash?])]
                  [empty-pinfo pinfo?]
                  [get-base-pinfo (symbol? . -> . pinfo?)]
                  [pinfo-used-bindings (pinfo? . -> . (listof binding?))]
                  [pinfo-accumulate-module (module-binding? pinfo? . -> . pinfo?)]
                  [pinfo-accumulate-binding (binding? pinfo? . -> . pinfo?)]
                  [pinfo-accumulate-binding-use (binding? pinfo? . -> . pinfo?)]
                  [pinfo-accumulate-bindings ((listof binding?) pinfo? . -> . pinfo?)]
                  [pinfo-update-env (pinfo? env? . -> . pinfo?)]
                  [pinfo-gensym (pinfo? symbol? . -> . (list/c pinfo? symbol?))]
                  [pinfo-permissions (pinfo? . -> . (listof permission?))])
