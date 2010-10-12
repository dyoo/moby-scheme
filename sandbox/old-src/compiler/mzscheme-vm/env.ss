#lang scheme/base

(require scheme/contract
         scheme/match
         scheme/list)

;; Representation of the stack environment of the mzscheme vm, so we know where
;; things live.

(define-struct env () #:transparent)
(define-struct (empty-env env) () #:transparent)
(define-struct (local-env env) (name boxed? parent-env) #:transparent)
(define-struct (global-env env) (names parent-env) #:transparent)
(define-struct (unnamed-env env) (parent-env) #:transparent)

(define EMPTY-ENV (make-empty-env))




;; env-push-globals: env (listof symbol) -> env
(define (env-push-globals env names)
  (make-global-env names env))


;; env-push-local: env symbol -> env
(define (env-push-local env name)
  (make-local-env name #f env))


;; env-push-local: env symbol -> env
(define (env-push-local/boxed env name)
  (make-local-env name #t env))


;; env-push-unnamed: env -> env
(define (env-push-unnamed env)
  (make-unnamed-env env))


;; env-pop: env -> env
(define (env-pop env)
  (match env
    [(struct empty-env ())
     (error 'env-pop "empty env")]
    [(struct local-env (name boxed? parent-env))
     parent-env]
    [(struct global-env (names parent-env))
     parent-env]
    [(struct unnamed-env (parent-env))
     parent-env]))



(define-struct stack-reference () #:transparent)
(define-struct (local-stack-reference stack-reference) (name boxed? depth) #:transparent)
(define-struct (global-stack-reference stack-reference) (name depth pos) #:transparent)
(define-struct (unbound-stack-reference stack-reference) (name) #:transparent)



;; position: symbol (listof symbol) -> (or number #f)
;; Find position of element in list; return false if we can't find the element.
(define (position x L)
  (let loop ([i 0]
             [L L])
    (cond
      [(empty? L)
       #f]
      [(eq? x (first L))
       i]
      [else
       (loop (add1 i)
             (rest L))])))


;; env-lookup: env symbol -> stack-reference
(define (env-lookup env a-name)
  (let loop ([env env]
             [depth 0])
    (match env
      [(struct empty-env ())
       (make-unbound-stack-reference a-name)]
      
      [(struct local-env (name boxed? parent-env))
       (cond
         [(eq? a-name name)
          (make-local-stack-reference name boxed? depth)]
         [else 
          (loop parent-env (add1 depth))])]
      
      [(struct global-env (names parent-env))
       (cond [(position a-name names)
              =>
              (lambda (pos)
                (make-global-stack-reference a-name depth pos))]
             [else
              (loop parent-env (add1 depth))])]
      
      [(struct unnamed-env (parent-env))
       (loop parent-env (add1 depth))])))


;; env-peek: env number -> env
(define (env-peek env depth)
  (let loop ([env env]
             [depth depth])
    (cond
      [(= depth 0)
       env]
      [else
       (match env
         [(struct empty-env ())
          (error 'env-peek)]
         
         [(struct local-env (name boxed? parent-env))
          (loop parent-env (sub1 depth))]

         [(struct global-env (names parent-env))
          (loop parent-env (sub1 depth))]
         [(struct unnamed-env (parent-env))
          (loop parent-env (sub1 depth))])])))
                 
         



(provide/contract [env? (any/c . -> . boolean?)]
                  [rename EMPTY-ENV empty-env env?]
                  [env-push-globals (env? (listof (or/c false/c symbol?)) . -> . env?)]
                  [env-push-local (env? symbol? . -> . env?)]
                  [env-push-local/boxed (env? symbol? . -> . env?)]
                  [env-push-unnamed (env? . -> . env?)]
                  [env-pop (env? . -> . env?)]

                  [env-lookup (env? symbol? . -> . stack-reference?)]

                  [env-peek (env? number? . -> . env?)]
                  
                  [struct global-env ([names (listof (or/c false/c symbol?))]
                                      [parent-env (listof env?)])]
                  
                  [struct stack-reference ()]
                  [struct (local-stack-reference stack-reference) 
                          [(name symbol?)
                           (boxed? boolean?)
                           (depth number?)]]
                  [struct (global-stack-reference stack-reference)
                          [(name symbol?)
                           (depth number?)
                           (pos number?)]]
                  [struct (unbound-stack-reference stack-reference)
                          [(name symbol?)]])