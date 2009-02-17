#lang scheme/base

(require scheme/match
         scheme/string)

(provide (all-defined-out))

;; A program is a (listof (or/c defn? expr? test-case? library-require?))


;; program: any -> boolean
;; Returns true if the datum is a program.
(define (program? datum)
  (and (list? datum)
       (andmap (lambda (x) 
                 (or (defn? x)
                     (expression? x)
                     (test-case? x)
                     (library-require? x)))
               datum)))


;; expression?: any -> boolean
;; Returns true if the datum is an expression.
(define (expression? an-expr)
  (and (not (defn? an-expr))
       (not (test-case? an-expr))
       (not (library-require? an-expr))))


;; defn?: s-expression -> boolean
(define (defn? an-s-exp)
  (match an-s-exp
    [(list 'define rest ...)
     #t]
    [(list 'define-struct rest ...)
     #t]
    [else
     #f]))

;; test-case?: s-expression -> boolean
(define (test-case? an-sexp)
  (match an-sexp
    [(list 'check-expect e ...)
     #t]
    [(list 'check-within e ...)
     #t]
    [(list 'check-error e ...)
     #t]
    [else
     #f]))

;; library-require?: s-expression -> boolean
(define (library-require? an-sexp)
  (match an-sexp
    [(list 'require s ...)
     #t]
    [else
     #f]))





;; identifier->munged-java-identifier: symbol -> symbol
(define (identifier->munged-java-identifier an-id)
  ;; Special character mappings for identifiers
  (define char-mappings 
    #hash((#\- . "_dash_")
          (#\_ . "_underline_")
          (#\? . "_question_")
          (#\! . "_bang_")
          (#\. . "_dot_")
          (#\: . "_colon_")
          (#\= . "_equal_")
          (#\# . "_pound_")
          (#\$ . "_dollar_")
          (#\% . "_percent_")
          (#\^ . "_tilde_")
          (#\& . "_and_")
          (#\* . "_star_")
          (#\+ . "_plus_")
          (#\* . "_star_")
          (#\/ . "_slash_")
          (#\< . "_lessthan_")
          (#\> . "_greaterthan_")
          (#\~ . "_tilde_")))
  (let* ([chars (string->list (symbol->string an-id))]
         [translated-chunks 
          (map (lambda (ch) (hash-ref char-mappings ch (string ch))) chars)]
         [translated-id
          (string->symbol
           (string-join translated-chunks ""))])
    translated-id))