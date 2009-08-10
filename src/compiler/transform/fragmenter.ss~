#lang s-exp "lang.ss"

(require "lift-locals.ss")
(require "anormalize.ss")

(define frag-prepend "f")
(define statement-name 'statement)

(define-struct finfo (return fragments gensym))

;; fragment-help: s-expr number -> linfo
;; consumes a symbolic expression and a gensym counter
;; returns a fragmented symbolic expression
(define (fragment-help expr args name gensym frag-locals?)
  (cond
    [(cons? expr)
     (cond
       [(equal? (first expr) 'local)
        (local [(define definitions (if frag-locals?
                                        (foldr (lambda (statement rest-statements)
                                                 (append (get-fragments statement)
                                                         rest-statements))
                                               empty
                                               (second expr))
                                        (second expr)))
                (define first-def-id (if (cons? (second (first definitions)))
                                         (first (second (first definitions)))
                                         (second (first definitions))))
                (define rec-rest
                  (fragment-help (list 'define
                                       (list* (string->symbol
                                               (string-append frag-prepend
                                                              (number->string gensym)
                                                              "_"
                                                              (symbol->string name)))
                                              first-def-id
                                              args)
                                       (if (empty? (rest definitions))
                                           (third expr)
                                           (list 'local
                                                 (rest definitions)
                                                 (third expr))))
                                 (cons first-def-id args)
                                 name
                                 (add1 gensym)
                                 false))]
          (make-finfo (list 'local
                            (list (first definitions))
                            (if (and (cons? (finfo-return rec-rest))
                                     (equal? (first (finfo-return rec-rest)) 'define))
                                (second (finfo-return rec-rest))
                                (finfo-return rec-rest)))
                      (if (and (cons? (finfo-return rec-rest))
                               (equal? (first (finfo-return rec-rest)) 'define))
                          (cons (finfo-return rec-rest)
                                (finfo-fragments rec-rest))
                          (finfo-fragments rec-rest))
                      (finfo-gensym rec-rest)))]
       [(equal? (first expr) 'quote) (make-finfo expr empty gensym)]
       [(equal? (first expr) 'define)
        (local [(define filtered-args
                  (if (cons? (second expr))
                      (append (rest (second expr))
                              (filter (lambda (elt)
                                        (not (contains? elt (rest (second expr)))))
                                      args))
                      args))
                (define rec-rest (fragment-help (third expr)
                                                filtered-args
                                                name
                                                gensym
                                                frag-locals?))]
          (make-finfo (list 'define
                            (second expr)
                            (finfo-return rec-rest))
                      (finfo-fragments rec-rest)
                      (finfo-gensym rec-rest)))]
       [else (foldl (lambda (an-expr rest-info)
                      (local [(define rec-info
                                (fragment-help an-expr
                                               args
                                               name
                                               (finfo-gensym rest-info)
                                               true))]
                        (make-finfo (append (finfo-return rest-info)
                                            (list (finfo-return rec-info)))
                                    (append (finfo-fragments rest-info)
                                            (finfo-fragments rec-info))
                                    (finfo-gensym rec-info))))
                    (make-finfo empty empty gensym)
                    expr)])]
    [else (make-finfo expr empty gensym)]))

;; get-fragments: s-expr -> (listof s-expr)
;; consumes a toplevel symbolic expression that is the output of anormalize
;; fragments the expression into mini-methods
(define (get-fragments expr)
  (local [(define name (if (and (cons? expr)
                                (equal? (first expr) 'define))
                           (if (cons? (second expr))
                               (first (second expr))
                               (second expr))
                           statement-name))
          (define frag-info (fragment-help expr empty name 0 true))]
    (reverse (cons (finfo-return frag-info)
                   (finfo-fragments frag-info)))))

;; fragment: (listof s-expr) -> (listof s-expr)
;; consumes a toplevel list of symbolic expressions representing a program
;; returns the program with each statement fragmented
(define (fragment program)
  (foldr (lambda (statement rest-statements)
           (append (get-fragments statement) rest-statements))
         empty
         (anormalize program)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; STORAGE

#|
        ;(begin
          #;(printf "in local\n")

                  ;(begin
                    #;(printf "calling fragment-help (~a) on\n ~a\n"
                            frag-locals?
                            (list 'define
                                  (list* (string->symbol
                                          (string-append frag-prepend
                                                         (number->string gensym)
                                                         "_"
                                                         (symbol->string name)))
                                         first-def-id
                                         args)
                                  (if (empty? (rest definitions))
                                      (third expr)
                                      (list 'local
                                            (rest definitions)
                                            (third expr)))))

          ;(begin
            #;(printf "In a local statement, name '~a', args is\n ~a\n"
                    name
                    args)

        ;(begin
          #;(printf "in define: ~a\n"
                  (if (cons? (second expr))
                      (first (second expr))
                      (second expr)))

          ;(begin
            #;(printf "In define, new name is '~a'\n";, returning\n ~a\n"
                    (if (cons? (second expr))
                        (first (second expr))
                        (second expr))
                    #;(list 'define
                          (second expr)
                          (finfo-return rec-rest)))

        ;(begin
          #;(printf "in else: ~a\n"
                  (first expr))
|#