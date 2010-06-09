#lang scheme/base

(require scheme/contract
         scheme/match
         scheme/list)

;; Representation of the stack environment of the mzscheme vm, so we know where
;; things live.

(define-struct empty-env ())
(define-struct local-env (name parent-rib))
(define-struct global-env (names parent-rib))




;; env-push-globals: env (listof symbol) -> env
(define (env-push-globals env names)
  (make-global-env names env))


;; env-push-local: env symbol -> env
(define (env-push-local env name)
  (make-local-env name env))


;; env-pop: env -> env
(define (env-pop env)
  (match env
    [(struct empty-env ())
     (error 'env-pop "empty env")]
    [(struct local-env (name parent-env))
     parent-env]
    [(struct global-env (names parent-env))
     parent-env]))



;; stack-reference?: any -> boolean
(define (stack-reference? x)
  (or (local-stack-reference? x)
      (global-stack-reference? x)))

(define-struct local-stack-reference (depth))
(define-struct global-stack-reference (depth pos))



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
       (error 'env-lookup (format "Couldn't find ~s in the environment" a-name))]
      [(struct local-env (name parent-env))
       (cond
         [(eq? a-name name)
          (make-local-stack-reference depth)]
         [else 
          (loop parent-env (add1 depth))])]
      [(struct global-env (names parent-env))
       (cond [(position a-name names)
              =>
              (lambda (pos)
                (make-global-stack-reference depth pos))]
             [else
              (loop parent-env (add1 depth))])])))
