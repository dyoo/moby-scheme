#lang s-exp "lang.ss"

;; Red black trees.
;; Most of this comes from the code in:
;; http://programmingpraxis.com/2009/10/02/red-black-trees/2/
(define-struct rbtree (color key value lkid rkid))

(define empty-rbtree
  (make-rbtree 'black 'nil 'nil 'nil 'nil))

;; rbtree-empty?: (treeof X Y) -> boolean
(define (rbtree-empty? t) 
  (eq? t empty-rbtree))

;; red?: color -> boolean
(define (rbtree-color-red? c) 
  (eq? c 'red))

;; black?: color -> boolean
(define (rbtree-color-black? c) 
  (eq? c 'black))


;; rbtree-lookup: (X X -> boolean) (treeof X Y) X -> (or/c false (list X Y))
(define (rbtree-lookup lt? t k)
  (cond [(rbtree-empty? t) 
         #f]
        [(lt? k (rbtree-key t)) 
         (rbtree-lookup lt? (rbtree-lkid t) k)]
        [(lt? (rbtree-key t) k)
         (rbtree-lookup lt? (rbtree-rkid t) k)]
        [else 
         (list (rbtree-key t) (rbtree-value t))]))


;; rbtree-insert: (X X -> boolean) (treeof X Y) X Y -> (treeof X Y)
(define (rbtree-insert lt? t k v)
  (local [(define (ins t)
            (cond [(rbtree-empty? t) (make-rbtree 'red k v empty-rbtree empty-rbtree)]
                  [(lt? k (rbtree-key t))
                   (rbtree-balance (rbtree-color t) (rbtree-key t) (rbtree-value t) (ins (rbtree-lkid t)) (rbtree-rkid t))]
                  [(lt? (rbtree-key t) k)
                   (rbtree-balance (rbtree-color t) (rbtree-key t) (rbtree-value t) (rbtree-lkid t) (ins (rbtree-rkid t)))]
                  [else
                   (make-rbtree (rbtree-color t) k v (rbtree-lkid t) (rbtree-rkid t))]))]
    (let ([z (ins t)])
      (make-rbtree 'black (rbtree-key z) (rbtree-value z) (rbtree-lkid z) (rbtree-rkid z)))))


;; rbtree-balance: color X Y (treeof X Y) (treeof X Y) -> (treeof X Y)
(define (rbtree-balance c k v l r)
  (cond [(and (rbtree-color-black? c) (rbtree-color-red? (rbtree-color l)) (rbtree-color-red? (rbtree-color (rbtree-lkid l))))
         (make-rbtree 'red (rbtree-key l) (rbtree-value l)
                    (make-rbtree 'black (rbtree-key (rbtree-lkid l)) (rbtree-value (rbtree-lkid l))
                               (rbtree-lkid (rbtree-lkid l)) (rbtree-rkid (rbtree-lkid l)))
                    (make-rbtree 'black k v (rbtree-rkid l) r))]
        [(and (rbtree-color-black? c) (rbtree-color-red? (rbtree-color l)) (rbtree-color-red? (rbtree-color (rbtree-rkid l))))
         (make-rbtree 'red (rbtree-key (rbtree-rkid l)) (rbtree-value (rbtree-rkid l))
                    (make-rbtree 'black (rbtree-key l) (rbtree-value l) (rbtree-lkid l) (rbtree-lkid (rbtree-rkid l)))
                    (make-rbtree 'black k v (rbtree-rkid (rbtree-rkid l)) r))]
        [(and (rbtree-color-black? c) (rbtree-color-red? (rbtree-color r)) (rbtree-color-red? (rbtree-color (rbtree-lkid r))))
         (make-rbtree 'red (rbtree-key (rbtree-lkid r)) (rbtree-value (rbtree-lkid r))
                    (make-rbtree 'black k v l (rbtree-lkid (rbtree-lkid r)))
                    (make-rbtree 'black (rbtree-key r) (rbtree-value r) (rbtree-rkid (rbtree-lkid r)) (rbtree-rkid r)))]
        [(and (rbtree-color-black? c) (rbtree-color-red? (rbtree-color r)) (rbtree-color-red? (rbtree-color (rbtree-rkid r))))
         (make-rbtree 'red (rbtree-key r) (rbtree-value r)
                    (make-rbtree 'black k v l (rbtree-lkid r))
                    (make-rbtree 'black (rbtree-key (rbtree-rkid r)) (rbtree-value (rbtree-rkid r))
                               (rbtree-lkid (rbtree-rkid r)) (rbtree-rkid (rbtree-rkid r))))]
        [else (make-rbtree c k v l r)]))


;; rbtree->list: (treeof X Y) -> (listof (list X Y))
(define (rbtree->list t)
  (local [(define (enlist t xs)
            (cond [(rbtree-empty? t) xs]
                  [(and (rbtree-empty? (rbtree-lkid t)) (rbtree-empty? (rbtree-rkid t)))
                   (cons (list (rbtree-key t) (rbtree-value t)) xs)]
                  [else (enlist (rbtree-lkid t)
                                (cons (list (rbtree-key t) (rbtree-value t))
                                      (enlist (rbtree-rkid t) xs)))]))]
    (enlist t empty)))



(provide/contract [empty-rbtree 
                   rbtree?]
                  
                  [rbtree-insert 
                   ((any/c any/c . -> . boolean?) rbtree? any/c any/c
                                                  . -> . rbtree?)]
                  [rbtree-lookup 
                   ((any/c any/c . -> . boolean?) rbtree? any/c 
                                                  . -> . (or/c false/c (list/c any/c any/c)))]
                  
                  [rbtree->list
                   (rbtree? . -> . (listof (list/c any/c any/c)))])
