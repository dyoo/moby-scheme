#lang s-exp "../../../private/restricted-runtime-scheme.ss"


;; Syntax objects
(define-struct stx:atom (datum loc context))
(define-struct stx:list (elts loc context))

(define-struct Loc (offset line column span id))



;; stx?: any -> boolean
(define (stx? x)
  (or (stx:atom? x)
      (stx:list? x)))


;; stx-e: stx -> any
;; Shallow unwrap of the element out of the syntax.
(define (stx-e a-stx)
  (cond
    [(stx:atom? a-stx)
     (stx:atom-datum a-stx)]
    [(stx:list? a-stx)
     (stx:list-elts a-stx)]))


;; stx-context: stx -> env
(define (stx-context a-stx)
  (cond
    [(stx:atom? a-stx)
     (stx:atom-context a-stx)]
    [(stx:list? a-stx)
     (stx:list-context a-stx)]))


;; stx-update-context: stx env -> stx
(define (stx-update-context a-stx a-ctx)
  (cond
    [(stx:atom? a-stx)
     (make-stx:atom (stx:atom-datum a-stx)
                    (stx:atom-loc a-stx)
                    a-ctx)]
    [(stx:list? a-stx)
     (make-stx:list (stx:list-elts a-stx)
                    (stx:list-loc a-stx)
                    a-ctx)]))


;; stx-loc: stx -> loc
(define (stx-loc a-stx)
  (cond
    [(stx:atom? a-stx)
     (stx:atom-loc a-stx)]
    [(stx:list? a-stx)
     (stx:list-loc a-stx)]))


;; Loc->sexp: Loc -> sexp
(define (Loc->sexp a-loc)
  `(make-Loc ,(Loc-offset a-loc)
             ,(Loc-line a-loc)
             ,(Loc-column a-loc)
             ,(Loc-span a-loc)
             ,(Loc-id a-loc)))


;; sexp->Loc: sexp -> Loc
(define (sexp->Loc an-sexp)
  (cond [(and (= 6 (length an-sexp))
              (symbol? (list-ref an-sexp 0))
              (number? (list-ref an-sexp 1))
              (number? (list-ref an-sexp 2))
              (number? (list-ref an-sexp 3))
              (number? (list-ref an-sexp 4))
              (string? (list-ref an-sexp 5)))
         (make-Loc (list-ref an-sexp 1)
                   (list-ref an-sexp 2)
                   (list-ref an-sexp 3)
                   (list-ref an-sexp 4)
                   (list-ref an-sexp 5))]))






;; stx-begins-with?: stx symbol -> boolean
(define (stx-begins-with? a-stx a-sym)
  (cond
    [(stx:atom? a-stx)
     #f]
    [(stx:list? a-stx)
     (and (not (empty? (stx:list-elts a-stx)))
          (symbol? (stx-e (first (stx:list-elts a-stx))))
          (symbol=? (stx-e (first (stx:list-elts a-stx)))
                    a-sym))]))


;; datum->stx: any loc -> stx
;; Converts a datum into a syntax object deeply.  Every stx will
;; share the same loc value.  Pre-existing datum objects will
;; be left alone.
(define (datum->stx context-stx a-datum a-loc)
  (cond
    [(stx? a-datum)
     a-datum]
    [(or (pair? a-datum) (empty? a-datum))
     (make-stx:list (map (lambda (x) (datum->stx context-stx x a-loc)) a-datum)
                    a-loc
                    (if (stx? context-stx)
                        (stx-context context-stx)
                        #f))]
    [else
     (make-stx:atom a-datum a-loc 
                    (if (stx? context-stx)
                        (stx-context context-stx)
                        context-stx))]))


;; stx->datum: stx -> any
;; Rip out the location information
(define (stx->datum a-stx)
  (cond
    [(stx:atom? a-stx)
     (stx:atom-datum a-stx)]
    [(stx:list? a-stx)
     (map stx->datum (stx:list-elts a-stx))]))



;; stx->sexp: stx -> sexp
;; Translates the syntax to something that can be read.
;; WARNING: lexical context will be lost!
(define (stx->sexp a-stx)
  (cond
    [(stx:atom? a-stx)
     `(make-stx:atom ,(stx:atom-datum a-stx)
                     ,(Loc->sexp (stx:atom-loc a-stx))
                     #f)]
    [(stx:list? a-stx)
     `(make-stx:list ,(map stx->sexp (stx:list-elts a-stx))
                     ,(Loc->sexp (stx:list-loc a-stx))
                     #f)]))


;; sexp->stx: sexp -> stx
;; Translates s-expressions back to stx objects.
(define (sexp->stx sexp)
  (cond
    [(and (list? sexp)
          (equal? 'make-stx:atom (first sexp))
          (= (length sexp) 4)
          (boolean? (fourth sexp)))
     (make-stx:atom (second sexp)
                    (sexp->Loc (third sexp))
                    (fourth sexp))]
    [(and (list? sexp)
          (equal? 'make-stx:list (first sexp))
          (= (length sexp) 4)
          (boolean? (fourth sexp)))
     (make-stx:list (map sexp->stx (second sexp))
                    (sexp->Loc (third sexp))
                    (fourth sexp))]))


;; program->sexp: program -> sexp
;; Translates a program to an s-expression
(define (program->sexp a-program)
  (map stx->sexp a-program))



;; sexp->program: sexp -> program
;; Translates an sexp back to a program.
(define (sexp->program an-sexp)
  (map sexp->stx an-sexp))



  

(provide/contract [stx:atom? (any/c . -> . boolean?)]
                  [stx:list? (any/c . -> . boolean?)]
                  
                  [struct Loc ([offset number?]
                               [line number?]
                               [column number?]
                               [span number?]
                               [id string?])]
                  [Loc->sexp (Loc? . -> . any/c)]
                  [sexp->Loc (any/c . -> . Loc?)]
                  
                  [stx? (any/c . -> . boolean?)]
                  [stx-e (stx? . -> . any)]
                  [stx-loc (stx? . -> . any)]
                  [stx-context (stx? . -> . (or/c false/c any/c))]
                  
                  [stx-begins-with? (stx? symbol? . -> . boolean?)]
                  [datum->stx ((or/c false? stx?) any/c Loc? . -> . stx?)]
                  [stx->datum (stx? . -> . any)]
                  
                  [stx->sexp (stx? . -> . any)]
                  [sexp->stx (any/c . -> . stx?)]
                  
                  [program->sexp ((listof stx?) . -> . any)]
                  [sexp->program (any/c . -> . (listof stx?))]
                  
                  [stx-update-context (stx? any/c . -> . stx?)])