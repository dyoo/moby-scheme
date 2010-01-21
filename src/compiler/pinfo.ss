#lang s-exp "lang.ss"

(require "env.ss")
(require "toplevel.ss")
(require "helpers.ss")
(require "modules.ss")
(require "rbtree.ss")
(require "labeled-translation.ss")
(require "../runtime/permission-struct.ss")
(require "../runtime/binding.ss")


;; pinfo (program-info) is the "world" structure for the compilers; 
;; it captures the information we get from analyzing and compiling
;; the program, and also maintains some auxillary structures.
(define-struct pinfo (env                    ; env
                      modules                ; (listof module-binding) 
                      used-bindings-hash     ; (hashof symbol binding)
                      gensym-counter         ; number
                      provided-names         ; (hashof symbol provide-binding)
                      defined-names          ; (hashof symbol binding)
                      shared-expressions     ; (hashof expression labeled-translation)
                      with-location-emits?   ; boolean
                      module-resolver        ; (string -> (module-binding | false))
                      ))


;; empty-pinfo: pinfo
;; An empty pinfo that doesn't know any toplevel environment bindings.
(define empty-pinfo
  (make-pinfo empty-env
              empty 
              empty-rbtree              
              0
              empty-rbtree
              empty-rbtree
              empty-rbtree
              true
              default-module-resolver))




;; pinfo-used-bindings: pinfo -> (listof binding)
;; Returns the list of used bindings computed from the program analysis.
(define (pinfo-used-bindings a-pinfo)
  (map second (rbtree->list (pinfo-used-bindings-hash a-pinfo))))



;; pinfo-update-env: pinfo env -> pinfo
;; Updates the env of a pinfo.
(define (pinfo-update-env a-pinfo an-env)
  (make-pinfo
   an-env
   (pinfo-modules a-pinfo)
   (pinfo-used-bindings-hash a-pinfo)
   (pinfo-gensym-counter a-pinfo)
   (pinfo-provided-names a-pinfo)
   (pinfo-defined-names a-pinfo)
   (pinfo-shared-expressions a-pinfo)
   (pinfo-with-location-emits? a-pinfo)
   (pinfo-module-resolver a-pinfo)))


;; pinfo-update-defined-names: pinfo rbtree -> pinfo
;; Updates the provided names of a pinfo.
(define (pinfo-update-provided-names a-pinfo provided-names)
  (make-pinfo
   (pinfo-env a-pinfo)
   (pinfo-modules a-pinfo)
   (pinfo-used-bindings-hash a-pinfo)
   (pinfo-gensym-counter a-pinfo)
   provided-names
   (pinfo-defined-names a-pinfo)
   (pinfo-shared-expressions a-pinfo)
   (pinfo-with-location-emits? a-pinfo)
   (pinfo-module-resolver a-pinfo)))

;; pinfo-update-defined-names: pinfo rbtree -> pinfo
;; Updates the defined names of a pinfo.
(define (pinfo-update-defined-names a-pinfo defined-names)
  (make-pinfo
   (pinfo-env a-pinfo)
   (pinfo-modules a-pinfo)
   (pinfo-used-bindings-hash a-pinfo)
   (pinfo-gensym-counter a-pinfo)
   (pinfo-provided-names a-pinfo)
   defined-names
   (pinfo-shared-expressions a-pinfo)
   (pinfo-with-location-emits? a-pinfo)
   (pinfo-module-resolver a-pinfo)))


;; pinfo-update-with-location-emits?: pinfo boolean -> pinfo
;; Updates the with-location-emits? field.
(define (pinfo-update-with-location-emits? a-pinfo with-location-emits?)
  (make-pinfo (pinfo-env a-pinfo)
              (pinfo-modules a-pinfo)
              (pinfo-used-bindings-hash a-pinfo)
              (pinfo-gensym-counter a-pinfo)
              (pinfo-provided-names a-pinfo)
              (pinfo-defined-names a-pinfo)
              (pinfo-shared-expressions a-pinfo)
              with-location-emits?
              (pinfo-module-resolver a-pinfo)))

(define (pinfo-accumulate-shared-expression a-shared-expression a-translation a-pinfo)
  (make-pinfo (pinfo-env a-pinfo)
              (pinfo-modules a-pinfo)
              (pinfo-used-bindings-hash a-pinfo)
              (add1 (pinfo-gensym-counter a-pinfo))
              (pinfo-provided-names a-pinfo)
              (pinfo-defined-names a-pinfo)
              (rbtree-insert expression<? 
                             (pinfo-shared-expressions a-pinfo) 
                             a-shared-expression 
                             (make-labeled-translation (pinfo-gensym-counter a-pinfo)
                                                       a-translation))
              (pinfo-with-location-emits? a-pinfo)
              (pinfo-module-resolver a-pinfo)))

                                            

;; pinfo-accumulate-defined-binding: binding pinfo -> pinfo
;; Adds a new defined binding to a pinfo's set.
(define (pinfo-accumulate-defined-binding a-binding a-pinfo)
  (make-pinfo (env-extend (pinfo-env a-pinfo) a-binding)
              (pinfo-modules a-pinfo)
              (pinfo-used-bindings-hash a-pinfo)
              (pinfo-gensym-counter a-pinfo)
              (pinfo-provided-names a-pinfo)
              (rbtree-insert symbol< 
                             (pinfo-defined-names a-pinfo)
                             (binding-id a-binding)
                             a-binding)
              (pinfo-shared-expressions a-pinfo)
              (pinfo-with-location-emits? a-pinfo)
              (pinfo-module-resolver a-pinfo)))


;; pinfo-accumulate-bindings: (listof binding) pinfo -> pinfo
;; Adds a list of defined bindings to the pinfo's set.
(define (pinfo-accumulate-defined-bindings bindings a-pinfo)
  (foldl pinfo-accumulate-defined-binding
         a-pinfo
         bindings))



;; pinfo-accumuldate-module-bindings: (listof binding) pinfo -> pinfo
;; Adds a list of module-imported bindings to the pinfo's known set of bindings, without
;; including them within the set of defined names.
(define (pinfo-accumulate-module-bindings bindings a-pinfo)
  (foldl (lambda (a-binding a-pinfo)
           (make-pinfo (env-extend (pinfo-env a-pinfo) a-binding)
                       (pinfo-modules a-pinfo)
                       (pinfo-used-bindings-hash a-pinfo)
                       (pinfo-gensym-counter a-pinfo)
                       (pinfo-provided-names a-pinfo)
                       (pinfo-defined-names a-pinfo)
                       (pinfo-shared-expressions a-pinfo)
                       (pinfo-with-location-emits? a-pinfo)
                       (pinfo-module-resolver a-pinfo)))
         a-pinfo
         bindings))


;; pinfo-accumulate-module: module-binding pinfo -> pinfo
;; Adds a module to the pinfo's set.
(define (pinfo-accumulate-module a-module a-pinfo)
  (make-pinfo (pinfo-env a-pinfo)
              (cons a-module (pinfo-modules a-pinfo))
              (pinfo-used-bindings-hash a-pinfo)
              (pinfo-gensym-counter a-pinfo)
              (pinfo-provided-names a-pinfo)
              (pinfo-defined-names a-pinfo)
              (pinfo-shared-expressions a-pinfo)
              (pinfo-with-location-emits? a-pinfo)
              (pinfo-module-resolver a-pinfo)))


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
              (pinfo-provided-names a-pinfo)
              (pinfo-defined-names a-pinfo)
              (pinfo-shared-expressions a-pinfo)
              (pinfo-with-location-emits? a-pinfo)
              (pinfo-module-resolver a-pinfo)))


;; pinfo-gensym: pinfo symbol -> (list pinfo symbol)
;; Generates a unique symbol.
(define (pinfo-gensym a-pinfo a-label)
  (list (make-pinfo (pinfo-env a-pinfo)
                    (pinfo-modules a-pinfo)
                    (pinfo-used-bindings-hash a-pinfo)
                    (add1 (pinfo-gensym-counter a-pinfo))
                    (pinfo-provided-names a-pinfo)
                    (pinfo-defined-names a-pinfo)
                    (pinfo-shared-expressions a-pinfo)
                    (pinfo-with-location-emits? a-pinfo)
                    (pinfo-module-resolver a-pinfo))

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




(define-struct provide-binding:id (stx))
(define-struct provide-binding:struct-id (stx))


;; provide-binding?: any -> boolean
;; Produces true if the input is a provide-binding.
(define (provide-binding? x)
  (or (provide-binding:id? x)
      (provide-binding:struct-id? x)))


;; provide-binding-stx: provide-binding -> stx
;; Extract the stx where the provide binding was defined.
(define (provide-binding-stx a-provide-binding)
  (cond
    [(provide-binding:id? a-provide-binding)
     (provide-binding:id-stx a-provide-binding)]
    [(provide-binding:struct-id? a-provide-binding)
     (provide-binding:struct-id-stx a-provide-binding)]))





;; pinfo-get-exposed-bindings: pinfo -> (listof pinfo)
;; Extract the list of the defined bindings that are exposed by provide.
(define (pinfo-get-exposed-bindings a-pinfo)
  (local [;; lookup-provide-binding-in-definition-bindings: provide-binding compiled-program -> binding
          ;; Lookup the provided bindings.
          (define (lookup-provide-binding-in-definition-bindings a-provide-binding)
            (local [(define list-or-false
                      (rbtree-lookup symbol<
                                     (pinfo-defined-names a-pinfo)
                                     (stx-e (provide-binding-stx a-provide-binding))))]
              (cond
                [(list? list-or-false)
                 (check-binding-compatibility a-provide-binding
                                              (second list-or-false))]
                [else
                 (syntax-error (format "The provided name ~s has not been defined"
                                       (stx-e (provide-binding-stx a-provide-binding)))
                               (provide-binding-stx a-provide-binding))])))
          
          ;; Make sure that if the provide says "struct-out ...", that the exported binding
          ;; is really a structure.
          (define (check-binding-compatibility a-provide-binding a-binding)
            (cond
              [(provide-binding:struct-id? a-provide-binding)
               (cond [(binding:structure? a-binding)
                      a-binding]
                     [else
                      (syntax-error 
                       (format "The provided name ~s does not appear to be the name of a defined structure" 
                               (stx-e (provide-binding-stx a-provide-binding)))
                       (provide-binding-stx a-provide-binding))])]
              [else
               a-binding]))]
    (rbtree-fold (pinfo-provided-names a-pinfo)
                 (lambda (id a-provide-binding acc)
                   (cons (lookup-provide-binding-in-definition-bindings a-provide-binding)
                         acc))
                 empty)))



(provide/contract [struct pinfo ([env env?]
                                 [modules (listof module-binding?)]
                                 [used-bindings-hash rbtree?]
                                 [gensym-counter number?]
                                 [provided-names rbtree?]
                                 [defined-names rbtree?]
                                 [shared-expressions rbtree?]
                                 [with-location-emits? boolean?]
                                 [module-resolver (symbol? . -> . (or/c module-binding? false/c))])]
                  
                  
                  [empty-pinfo pinfo?]
                  [get-base-pinfo (symbol? . -> . pinfo?)]
                  [pinfo-used-bindings (pinfo? . -> . (listof binding?))]
                  [pinfo-accumulate-module (module-binding? pinfo? . -> . pinfo?)]
                  [pinfo-accumulate-defined-binding (binding? pinfo? . -> . pinfo?)]
                  [pinfo-accumulate-binding-use (binding? pinfo? . -> . pinfo?)]
                  [pinfo-accumulate-defined-bindings ((listof binding?) pinfo? . -> . pinfo?)]
                  [pinfo-accumulate-module-bindings ((listof binding?) pinfo? . -> . pinfo?)]
                  [pinfo-accumulate-shared-expression (expression? string? pinfo? . -> . pinfo?)]
                  [pinfo-update-provided-names (pinfo? rbtree? . -> . pinfo?)]
                  [pinfo-update-defined-names (pinfo? rbtree? . -> . pinfo?)]
                  [pinfo-update-env (pinfo? env? . -> . pinfo?)]
                  [pinfo-update-with-location-emits? (pinfo? boolean? . -> . pinfo?)]
                  [pinfo-gensym (pinfo? symbol? . -> . (list/c pinfo? symbol?))]
                  [pinfo-permissions (pinfo? . -> . (listof permission?))]
 
                  [pinfo-get-exposed-bindings (pinfo? . -> . (listof binding?))]
                  
                  
                  [struct provide-binding:id ([stx stx?])]
                  [struct provide-binding:struct-id ([stx stx?])]
                  [provide-binding? (any/c . -> . boolean?)]
                  [provide-binding-stx (provide-binding? . -> . stx?)])
