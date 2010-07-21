#lang s-exp "lang.ss"

(require "env.ss")
(require "toplevel.ss")
(require "helpers.ss")
(require "modules.ss")
(require "rbtree.ss")
(require "labeled-translation.ss")
(require "../collects/moby/runtime/error-struct.ss")
(require "../collects/moby/runtime/permission-struct.ss")
(require "../collects/moby/runtime/binding.ss")
(require "../collects/moby/runtime/stx.ss")


;; pinfo (program-info) is the "world" structure for the compilers; 
;; it captures the information we get from analyzing and compiling
;; the program, and also maintains some auxillary structures.
(define-struct pinfo (env                    ; env
                      modules                ; (listof module-binding) 
                      used-bindings-hash     ; (hashof symbol binding)
                      free-variables         ; (listof symbol)
                      gensym-counter         ; number
                      provided-names         ; (hashof symbol provide-binding)
                      defined-names          ; (hashof symbol binding)

                      shared-expressions     ; (hashof expression labeled-translation)
                      ;; Maintains a mapping between expressions and a labeled translation.  Acts
                      ;; as a symbol table to avoid duplicate construction of common literal values.
                      
                      with-location-emits?   ; boolean
                      ;; If true, the compiler emits calls to plt.Kernel.setLastLoc to maintain
                      ;; source position during evaluation.                      

                      allow-redefinition?     ; boolean
                      ;; If true, redefinition of a value that's already defined will not raise an error.
                      
                      ;; For the module system.
                      module-resolver        ; (module-name -> (module-binding | false))
                      module-path-resolver   ; (string module-path -> module-name)
                      current-module-path    ; module-path

                      declared-permissions            ; (listof (listof symbol any/c))
                      
                      ))





(define default-current-module-path "")


;; empty-pinfo: pinfo
;; An empty pinfo that doesn't know any toplevel environment bindings.
(define empty-pinfo
  (make-pinfo empty-env
              empty 
              empty-rbtree
              '()
              0
              empty-rbtree
              empty-rbtree
              empty-rbtree
              true
              true
              default-module-resolver
              default-module-path-resolver
              default-current-module-path
              empty))




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
   (pinfo-free-variables a-pinfo)
   (pinfo-gensym-counter a-pinfo)
   (pinfo-provided-names a-pinfo)
   (pinfo-defined-names a-pinfo)
   (pinfo-shared-expressions a-pinfo)
   (pinfo-with-location-emits? a-pinfo)
   (pinfo-allow-redefinition? a-pinfo)
   (pinfo-module-resolver a-pinfo)
   (pinfo-module-path-resolver a-pinfo)
   (pinfo-current-module-path a-pinfo)
   (pinfo-declared-permissions a-pinfo)))


;; pinfo-update-defined-names: pinfo rbtree -> pinfo
;; Updates the provided names of a pinfo.
(define (pinfo-update-provided-names a-pinfo provided-names)
  (make-pinfo
   (pinfo-env a-pinfo)
   (pinfo-modules a-pinfo)
   (pinfo-used-bindings-hash a-pinfo)
   (pinfo-free-variables a-pinfo)
   (pinfo-gensym-counter a-pinfo)
   provided-names
   (pinfo-defined-names a-pinfo)
   (pinfo-shared-expressions a-pinfo)
   (pinfo-with-location-emits? a-pinfo)
   (pinfo-allow-redefinition? a-pinfo)
   (pinfo-module-resolver a-pinfo)
   (pinfo-module-path-resolver a-pinfo)
   (pinfo-current-module-path a-pinfo)
   (pinfo-declared-permissions a-pinfo)))

;; pinfo-update-defined-names: pinfo rbtree -> pinfo
;; Updates the defined names of a pinfo.
(define (pinfo-update-defined-names a-pinfo defined-names)
  (make-pinfo
   (pinfo-env a-pinfo)
   (pinfo-modules a-pinfo)
   (pinfo-used-bindings-hash a-pinfo)
   (pinfo-free-variables a-pinfo)
   (pinfo-gensym-counter a-pinfo)
   (pinfo-provided-names a-pinfo)
   defined-names
   (pinfo-shared-expressions a-pinfo)
   (pinfo-with-location-emits? a-pinfo)
   (pinfo-allow-redefinition? a-pinfo)
   (pinfo-module-resolver a-pinfo)
   (pinfo-module-path-resolver a-pinfo)
   (pinfo-current-module-path a-pinfo)
   (pinfo-declared-permissions a-pinfo)))


;; pinfo-update-with-location-emits?: pinfo boolean -> pinfo
;; Updates the with-location-emits? field.
(define (pinfo-update-with-location-emits? a-pinfo with-location-emits?)
  (make-pinfo (pinfo-env a-pinfo)
              (pinfo-modules a-pinfo)
              (pinfo-used-bindings-hash a-pinfo)
              (pinfo-free-variables a-pinfo)
              (pinfo-gensym-counter a-pinfo)
              (pinfo-provided-names a-pinfo)
              (pinfo-defined-names a-pinfo)
              (pinfo-shared-expressions a-pinfo)
              with-location-emits?
              (pinfo-allow-redefinition? a-pinfo)
              (pinfo-module-resolver a-pinfo)
              (pinfo-module-path-resolver a-pinfo)
              (pinfo-current-module-path a-pinfo)
              (pinfo-declared-permissions a-pinfo)))


;; pinfo-update-allow-redefinition?: pinfo boolean -> pinfo
(define (pinfo-update-allow-redefinition? a-pinfo allow-redefinition?)
  (make-pinfo (pinfo-env a-pinfo)
              (pinfo-modules a-pinfo)
              (pinfo-used-bindings-hash a-pinfo)
              (pinfo-free-variables a-pinfo)
              (pinfo-gensym-counter a-pinfo)
              (pinfo-provided-names a-pinfo)
              (pinfo-defined-names a-pinfo)
              (pinfo-shared-expressions a-pinfo)
              (pinfo-with-location-emits? a-pinfo)
              allow-redefinition?
              (pinfo-module-resolver a-pinfo)
              (pinfo-module-path-resolver a-pinfo)
              (pinfo-current-module-path a-pinfo)
              (pinfo-declared-permissions a-pinfo)))

;; pinfo-update-module-resolver: pinfo module-resolver -> pinfo
(define (pinfo-update-module-resolver a-pinfo module-resolver)
  (make-pinfo (pinfo-env a-pinfo)
              (pinfo-modules a-pinfo)
              (pinfo-used-bindings-hash a-pinfo)
              (pinfo-free-variables a-pinfo)
              (pinfo-gensym-counter a-pinfo)
              (pinfo-provided-names a-pinfo)
              (pinfo-defined-names a-pinfo)
              (pinfo-shared-expressions a-pinfo)
              (pinfo-with-location-emits? a-pinfo)
              (pinfo-allow-redefinition? a-pinfo)
              module-resolver
              (pinfo-module-path-resolver a-pinfo)
              (pinfo-current-module-path a-pinfo)
              (pinfo-declared-permissions a-pinfo)))

;; pinfo-update-module-path-resolver: pinfo module-resolver -> pinfo
(define (pinfo-update-module-path-resolver a-pinfo module-path-resolver)
  (make-pinfo (pinfo-env a-pinfo)
              (pinfo-modules a-pinfo)
              (pinfo-used-bindings-hash a-pinfo)
              (pinfo-free-variables a-pinfo)
              (pinfo-gensym-counter a-pinfo)
              (pinfo-provided-names a-pinfo)
              (pinfo-defined-names a-pinfo)
              (pinfo-shared-expressions a-pinfo)
              (pinfo-with-location-emits? a-pinfo)
              (pinfo-allow-redefinition? a-pinfo)
              (pinfo-module-resolver a-pinfo)
              module-path-resolver
              (pinfo-current-module-path a-pinfo)
              (pinfo-declared-permissions a-pinfo)))

;; pinfo-update-module-resolver: pinfo module-resolver -> pinfo
(define (pinfo-update-current-module-path a-pinfo current-module-path)
  (make-pinfo (pinfo-env a-pinfo)
              (pinfo-modules a-pinfo)
              (pinfo-used-bindings-hash a-pinfo)
              (pinfo-free-variables a-pinfo)
              (pinfo-gensym-counter a-pinfo)
              (pinfo-provided-names a-pinfo)
              (pinfo-defined-names a-pinfo)
              (pinfo-shared-expressions a-pinfo)
              (pinfo-with-location-emits? a-pinfo)
              (pinfo-allow-redefinition? a-pinfo)
              (pinfo-module-resolver a-pinfo)
              (pinfo-module-path-resolver a-pinfo)
              current-module-path
              (pinfo-declared-permissions a-pinfo)))

(define (pinfo-accumulate-declared-permission a-name a-permission a-pinfo)
  (make-pinfo (pinfo-env a-pinfo)
              (pinfo-modules a-pinfo)
              (pinfo-used-bindings-hash a-pinfo)
              (pinfo-free-variables a-pinfo)
              (add1 (pinfo-gensym-counter a-pinfo))
              (pinfo-provided-names a-pinfo)
              (pinfo-defined-names a-pinfo)
              (pinfo-shared-expressions a-pinfo)
              (pinfo-with-location-emits? a-pinfo)
              (pinfo-allow-redefinition? a-pinfo)
              (pinfo-module-resolver a-pinfo)
              (pinfo-module-path-resolver a-pinfo)
              (pinfo-current-module-path a-pinfo)
              (cons (list a-name a-permission)
                    (pinfo-declared-permissions a-pinfo))))

  

;; pinfo-accumulate-shared-expression: expression string pinfo -> pinfo
(define (pinfo-accumulate-shared-expression a-shared-expression a-translation a-pinfo)
  (make-pinfo (pinfo-env a-pinfo)
              (pinfo-modules a-pinfo)
              (pinfo-used-bindings-hash a-pinfo)
              (pinfo-free-variables a-pinfo)
              (add1 (pinfo-gensym-counter a-pinfo))
              (pinfo-provided-names a-pinfo)
              (pinfo-defined-names a-pinfo)
              (rbtree-insert expression<? 
                             (pinfo-shared-expressions a-pinfo) 
                             a-shared-expression 
                             (make-labeled-translation (pinfo-gensym-counter a-pinfo)
                                                       a-translation))
              (pinfo-with-location-emits? a-pinfo)
              (pinfo-allow-redefinition? a-pinfo)
              (pinfo-module-resolver a-pinfo)
              (pinfo-module-path-resolver a-pinfo)
              (pinfo-current-module-path a-pinfo)
              (pinfo-declared-permissions a-pinfo)))

                                            
;; pinfo-accumulate-defined-binding: binding pinfo loc -> pinfo
;; Adds a new defined binding to a pinfo's set.
(define (pinfo-accumulate-defined-binding a-binding a-pinfo a-loc)
  (cond
    [(and (not (pinfo-allow-redefinition? a-pinfo))
          (is-redefinition? (binding-id a-binding) a-pinfo))
     (raise (make-moby-error a-loc
                             (make-moby-error-type:redefinition-not-allowed
                              (binding-id a-binding))))]
    [else
     (make-pinfo (env-extend (pinfo-env a-pinfo) a-binding)
                 (pinfo-modules a-pinfo)
                 (pinfo-used-bindings-hash a-pinfo)
                 (pinfo-free-variables a-pinfo)
                 (pinfo-gensym-counter a-pinfo)
                 (pinfo-provided-names a-pinfo)
                 (rbtree-insert symbol< 
                                (pinfo-defined-names a-pinfo)
                                (binding-id a-binding)
                                a-binding)
                 (pinfo-shared-expressions a-pinfo)
                 (pinfo-with-location-emits? a-pinfo)
                 (pinfo-allow-redefinition? a-pinfo)
                 (pinfo-module-resolver a-pinfo)
                 (pinfo-module-path-resolver a-pinfo)
                 (pinfo-current-module-path a-pinfo)
                 (pinfo-declared-permissions a-pinfo))]))


;; is-redefinition?: symbol -> boolean
(define (is-redefinition? a-name a-pinfo)
  (binding? (env-lookup (pinfo-env a-pinfo) a-name)))



;; pinfo-accumulate-bindings: (listof binding) pinfo Loc -> pinfo
;; Adds a list of defined bindings to the pinfo's set.
(define (pinfo-accumulate-defined-bindings bindings a-pinfo a-loc)
  (foldl (lambda (a-binding a-pinfo)
           (pinfo-accumulate-defined-binding a-binding a-pinfo a-loc))
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
                       (pinfo-free-variables a-pinfo)
                       (pinfo-gensym-counter a-pinfo)
                       (pinfo-provided-names a-pinfo)
                       (pinfo-defined-names a-pinfo)
                       (pinfo-shared-expressions a-pinfo)
                       (pinfo-with-location-emits? a-pinfo)
                       (pinfo-allow-redefinition? a-pinfo)
                       (pinfo-module-resolver a-pinfo)
                       (pinfo-module-path-resolver a-pinfo)
                       (pinfo-current-module-path a-pinfo)
                       (pinfo-declared-permissions a-pinfo)))
         a-pinfo
         bindings))


;; pinfo-accumulate-module: module-binding pinfo -> pinfo
;; Adds a module to the pinfo's set.
(define (pinfo-accumulate-module a-module a-pinfo)
  (make-pinfo (pinfo-env a-pinfo)
              (cons a-module (pinfo-modules a-pinfo))
              (pinfo-used-bindings-hash a-pinfo)
              (pinfo-free-variables a-pinfo)
              (pinfo-gensym-counter a-pinfo)
              (pinfo-provided-names a-pinfo)
              (pinfo-defined-names a-pinfo)
              (pinfo-shared-expressions a-pinfo)
              (pinfo-with-location-emits? a-pinfo)
              (pinfo-allow-redefinition? a-pinfo)
              (pinfo-module-resolver a-pinfo)
              (pinfo-module-path-resolver a-pinfo)
              (pinfo-current-module-path a-pinfo)
              (pinfo-declared-permissions a-pinfo)))


;; pinfo-accumulate-binding-use: binding pinfo -> pinfo
;; Adds a binding's use to a pinfo's set.
(define (pinfo-accumulate-binding-use a-binding a-pinfo)
  (make-pinfo (pinfo-env a-pinfo)
              (pinfo-modules a-pinfo)
              (rbtree-insert symbol<
                             (pinfo-used-bindings-hash a-pinfo)
                             (binding-id a-binding)
                             a-binding)
              (pinfo-free-variables a-pinfo)
              (pinfo-gensym-counter a-pinfo)
              (pinfo-provided-names a-pinfo)
              (pinfo-defined-names a-pinfo)
              (pinfo-shared-expressions a-pinfo)
              (pinfo-with-location-emits? a-pinfo)
              (pinfo-allow-redefinition? a-pinfo)
              (pinfo-module-resolver a-pinfo)
              (pinfo-module-path-resolver a-pinfo)
              (pinfo-current-module-path a-pinfo)
              (pinfo-declared-permissions a-pinfo)))


;; pinfo-accumulate-free-variable-use: symbol pinfo -> pinfo
;; Mark a free variable usage.
(define (pinfo-accumulate-free-variable-use a-sym a-pinfo)
  (make-pinfo (pinfo-env a-pinfo)
              (pinfo-modules a-pinfo)
              (pinfo-used-bindings-hash a-pinfo)
              (cond [(member a-sym (pinfo-free-variables a-pinfo))
                     (pinfo-free-variables a-pinfo)]
                    [else
                     (cons a-sym (pinfo-free-variables a-pinfo))])
              (pinfo-gensym-counter a-pinfo)
              (pinfo-provided-names a-pinfo)
              (pinfo-defined-names a-pinfo)
              (pinfo-shared-expressions a-pinfo)
              (pinfo-with-location-emits? a-pinfo)
              (pinfo-allow-redefinition? a-pinfo)
              (pinfo-module-resolver a-pinfo)
              (pinfo-module-path-resolver a-pinfo)
              (pinfo-current-module-path a-pinfo)
              (pinfo-declared-permissions a-pinfo)))

  


;; pinfo-gensym: pinfo symbol -> (list pinfo symbol)
;; Generates a unique symbol.
(define (pinfo-gensym a-pinfo a-label)
  (list (make-pinfo (pinfo-env a-pinfo)
                    (pinfo-modules a-pinfo)
                    (pinfo-used-bindings-hash a-pinfo)
                    (pinfo-free-variables a-pinfo)
                    (add1 (pinfo-gensym-counter a-pinfo))
                    (pinfo-provided-names a-pinfo)
                    (pinfo-defined-names a-pinfo)
                    (pinfo-shared-expressions a-pinfo)
                    (pinfo-with-location-emits? a-pinfo)
                    (pinfo-allow-redefinition? a-pinfo)
                    (pinfo-module-resolver a-pinfo)
                    (pinfo-module-path-resolver a-pinfo)
                    (pinfo-current-module-path a-pinfo)
                    (pinfo-declared-permissions a-pinfo))

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





;; pinfo-get-exposed-bindings: pinfo -> (listof binding)
;; Extract the list of the defined bindings that are exposed by provide.
(define (pinfo-get-exposed-bindings a-pinfo)
  (local [;; lookup-provide-binding-in-definition-bindings: provide-binding compiled-program -> (listof binding)
          ;; Lookup the provided bindings.
          (define (lookup-provide-binding-in-definition-bindings a-provide-binding)
            (local [(define list-or-false
                      (rbtree-lookup symbol<
                                     (pinfo-defined-names a-pinfo)
                                     (stx-e (provide-binding-stx a-provide-binding))))
                    
                    (define the-binding
                      (cond
                        [(list? list-or-false)
                         (check-binding-compatibility a-provide-binding
                                                      (second list-or-false))]
                        [else
                         (raise (make-moby-error (stx-loc (provide-binding-stx a-provide-binding))
                                                 (make-moby-error-type:provided-name-not-defined
                                                  (stx-e (provide-binding-stx a-provide-binding)))))]))

                    ;; ref: symbol -> binding
                    ;; Lookup the binding, given the symbolic identifier.
                    (define (ref id)
                      (second (rbtree-lookup symbol< (pinfo-defined-names a-pinfo) id)))]
              (cond
                [(provide-binding:struct-id? a-provide-binding)
                 (append (list the-binding
                               (ref (binding:structure-constructor the-binding))
                               (ref (binding:structure-predicate the-binding)))
                         (map ref (binding:structure-accessors the-binding))
                         (map ref (binding:structure-mutators the-binding))
                         )]
                [else
                 (list the-binding)])))
          
          
          ;; decorate-with-permissions: binding -> binding
          ;; HACK!
          (define (decorate-with-permissions a-binding)
            (binding-update-permissions a-binding
                                        (map second
                                             (filter (lambda (entry)
                                                       (symbol=? (first entry)
                                                                 (binding-id a-binding)))
                                                     (pinfo-declared-permissions a-pinfo)))))

          
          ;; Make sure that if the provide says "struct-out ...", that the exported binding
          ;; is really a structure.
          (define (check-binding-compatibility a-provide-binding a-binding)
            (cond
              [(provide-binding:struct-id? a-provide-binding)
               (cond [(binding:structure? a-binding)
                      a-binding]
                     [else
                      (raise (make-moby-error 
                              (stx-loc (provide-binding-stx a-provide-binding))
                              (make-moby-error-type:provided-structure-not-structure
                               (stx-e (provide-binding-stx a-provide-binding)))))])]
              [else
               a-binding]))]
    (rbtree-fold (pinfo-provided-names a-pinfo)
                 (lambda (id a-provide-binding acc)
                   (append (map decorate-with-permissions
                                (lookup-provide-binding-in-definition-bindings a-provide-binding))
                         acc))
                 empty)))



(provide/contract [struct pinfo ([env env?]
                                 [modules (listof module-binding?)]
                                 [used-bindings-hash rbtree?]
                                 [free-variables (listof symbol?)]
                                 [gensym-counter number?]
                                 [provided-names rbtree?]
                                 [defined-names rbtree?]
                                 [shared-expressions rbtree?]
                                 
                                 [with-location-emits? boolean?]
                                 [allow-redefinition? boolean?]
                                 
                                 [module-resolver (module-name? . -> . (or/c module-binding? false/c))]
                                 [module-path-resolver (module-path? module-path? . -> . module-name?)]
                                 [current-module-path module-path?]
                                 [declared-permissions (listof (list/c symbol? permission?))])]
                  
                  
                  [empty-pinfo pinfo?]
                  [get-base-pinfo (symbol? . -> . pinfo?)]
                  [pinfo-used-bindings (pinfo? . -> . (listof binding?))]
                  [pinfo-accumulate-module (module-binding? pinfo? . -> . pinfo?)]
                  [pinfo-accumulate-binding-use (binding? pinfo? . -> . pinfo?)]
                  [pinfo-accumulate-defined-binding (binding? pinfo? Loc? . -> . pinfo?)]
                  [pinfo-accumulate-defined-bindings ((listof binding?) pinfo? Loc? . -> . pinfo?)]
                  [pinfo-accumulate-module-bindings ((listof binding?) pinfo? . -> . pinfo?)]
                  [pinfo-accumulate-shared-expression (expression? string? pinfo? . -> . pinfo?)]
                  [pinfo-accumulate-free-variable-use (symbol? pinfo? . -> . pinfo?)]

                  [pinfo-accumulate-declared-permission (symbol? permission? pinfo? . -> . pinfo?)]

                  [pinfo-update-provided-names (pinfo? rbtree? . -> . pinfo?)]
                  [pinfo-update-defined-names (pinfo? rbtree? . -> . pinfo?)]
                  [pinfo-update-env (pinfo? env? . -> . pinfo?)]
                  [pinfo-update-with-location-emits? (pinfo? boolean? . -> . pinfo?)]
                  [pinfo-update-allow-redefinition? (pinfo? boolean? . -> . pinfo?)]

                  [pinfo-update-module-resolver (pinfo? (module-name? . -> . (or/c module-binding? false/c))
                                                        . -> . pinfo?)]
                  [pinfo-update-module-path-resolver (pinfo? (module-path? module-path? . -> . (or/c module-name? false/c))

                                                             . -> . pinfo?)]
                  [pinfo-update-current-module-path (pinfo? module-path? . -> . pinfo?)]
                  
                  
                  [pinfo-gensym (pinfo? symbol? . -> . (list/c pinfo? symbol?))]
                  [pinfo-permissions (pinfo? . -> . (listof permission?))]
 
                  [pinfo-get-exposed-bindings (pinfo? . -> . (listof binding?))]
                  
                  [struct provide-binding:id ([stx stx?])]
                  [struct provide-binding:struct-id ([stx stx?])]
                  [provide-binding? (any/c . -> . boolean?)]
                  [provide-binding-stx (provide-binding? . -> . stx?)])
