#lang s-exp "../../../private/restricted-runtime-scheme.ss"

(require "dom-parameters.ss")

(define-struct label (n ;; number
                      ))





;; separate-with-spaces: (listof dom) -> (listof dom)
;; Introduces string space elements between the elts.
(define (separate-with-spaces elts)
  (cond
    [(empty? elts)
     empty]
    [else
     (reverse
      (rest
       (foldl (lambda (x acc)
                (cons " "
                      (cons x acc)))
              empty
              elts)))]))


;; scheme-value-to-dom-sexp: any -> dom
;; Converts a value to a DOM.
;; FIXME: make this extensible so structures can define how they'd like to be DOMified.
(define (scheme-value->dom-sexp val maybe-dom-parameters)
  (local [;; Keeps track of the values we've domified so far.
          (define labeled-vals (make-hasheq))
          
          ;; Keeps track of the set of values that have been shared within the val.
          (define shared (make-hasheq))
          
          (define counter 0)
          
          (define (initialize-shared-hash! x)
            (local [(define h (make-hasheq))
                    (define (loop x)
                      (cond
                        [(undefined? x)
                         (void)]
                        
                        [(hash-ref h x false)
                         (begin
                           (hash-set! shared x true)
                           (void))]
                        
                        [(string? x)
                         (void)]
                        
                        [(number? x)
                         (void)]
                        
                        [(boolean? x)
                         (void)]
                        
                        [(char? x)
                         (void)]
                        
                        [(symbol? x)
                         (void)]
                        
                        [(list? x)
                         (begin (hash-set! h x true)
                                (for-each loop x))]
                        
                        [(vector? x)
                         (begin (hash-set! h x true)
                                (for-each loop (vector->list x)))]
                        
                        [(struct? x)
                         ;; FIXME as soon as we expose structure inspection
                         (void)]
                        
                        [(box? x)
                         (begin (hash-set! h x true)
                                (loop (unbox x)))]
                        
                        [(hash? x)
                         (void)]
                        
                        [(procedure? x)
                         (void)]
                        
                        [else
                         (void)]))]
              (loop x)))
          
          (define (custom-dom-converter-applies? val)
            (and (dom-parameters? maybe-dom-parameters)
                 ((dom-parameters-scheme-value->dom? maybe-dom-parameters) val)))
          
          (define (apply-custom-dom-converter val)
            ((dom-parameters-scheme-value->dom maybe-dom-parameters) val ->dom))
          

          (define (->dom val)
            (cond              
              [(undefined? val)
               `(span ((class "SchemeValue-Undefined"))
                      "<undefined>")]
              
              [;; If this is the first time we're encountering the shared value,
               ;; label it.
               (and (hash-ref shared val false)
                    (not (label? (hash-ref labeled-vals val false))))
               (begin
                 (set! counter (add1 counter))
                 (hash-set! labeled-vals val (make-label counter))
                 `(span ((class "SchemeValue-SharedLabel"))
                        (span ((class "SchemeValue-SharedLabel.label")) 
                              ,(string-append "#" (number->string counter) "=")
                              )
                        (span ((class "SchemeValue-SharedLabel.item"))
                              ,(->dom* val false))))]
              [else
               (->dom* val true)]))
          
          
          (define (->dom* val allow-labeling?)
            (cond
              [(and allow-labeling? 
                    (label? (hash-ref labeled-vals val #f)))
               (local [(define a-label (hash-ref labeled-vals val #f))]
                 `(span ((class "SchemeValue-SharedReference"))
                        ,(string-append "#" (number->string (label-n a-label)))))]
              ;;;;
              
              [(custom-dom-converter-applies? val)
               (apply-custom-dom-converter val)]
              
              [(string? val)
               `(span ((class "SchemeValue-String"))
                      ,(string-append "\"" val "\""))]
              
              [(number? val)
               `(span ((class "SchemeValue-Number"))
                      ,(number->string val))]
              
              [(boolean? val)
               `(span ((class "SchemeValue-Boolean"))
                      ,(if val "true" "false"))]
              
              [(char? val)
               `(span ((class "SchemeValue-Character"))
                      ,(string #\# #\\ val))]
              
              [(symbol? val)
               `(span ((class "SchemeValue-Symbol"))
                      ,(symbol->string val))]
              
              [(list? val)
               (begin
                 `(span ((class "SchemeValue-List"))
                        (span ((class "SchemeValue-List.lparen")) "(")
                        ,@(separate-with-spaces (map (lambda (x)
                                                       `(span ((class "SchemeValue-List.item"))
                                                              ,(->dom x)))
                                                     val))
                        #;,@(separate-with-spaces (cons 
                                                   `(span ((class "SchemeValue-List.keyword")) "list")
                                                   (map (lambda (x)
                                                          `(span ((class "SchemeValue-List.item"))
                                                                 ,(->dom x)))
                                                        val)))
                        (span ((class "SchemeValue-List.rparen")) ")")))]

              [(vector? val)
               (begin
                 `(span ((class "SchemeValue-Vector"))
                        (span ((class "SchemeValue-Vector.lparen")) "(")
                        ,@(separate-with-spaces (cons
                                                 `(span ((class "SchemeValue.Vector.keyword")) "vector")
                                                 (map (lambda (x)
                                                        `(span ((class "SchemeValue-Vector.item"))
                                                              ,(->dom x)))
                                                      (vector->list val))))
                        (span ((class "SchemeValue-Vector.rparen")) ")")))]
              
              [(struct? val)
               `(span ((class "SchemeValue-Structure"))
                      ;; FIXME: we need primitives to get at structure contents.
                      "<struct>"
                      )]
              
              [(box? val)
               (begin
                 `(span ((class "SchemeValue-Box"))
                        (span ((class "SchemeValue-Box.lparen")) "(")
                        (span ((class "SchemeValue-Box.keyword")) "box")
                        " "
                        (span ((class "SchemeValue-Box.item")) 
                              ,(->dom (unbox val)))
                        (span ((class "SchemeValue-Box.rparen")) ")")))]
              
              [(hash? val)
               `(span ((class "SchemeValue-Hash"))
                      ;; FIXME: we should show contents.
                      "<hash>")]
              
              [(procedure? val)
               `(span ((class "SchemeValue-Function"))
                      ;; FIXME
                      ;; We need to get at the function name, if we can get at it.
                      ;; We also would like inspectors for the arity and
                      ;; the location the function's defined.
                      "<function>")]
                            
              [else
               `(span ((class "SchemeValue-DisplayedObject"))
                      ,(format "~a" val))]))]
    (begin
      (initialize-shared-hash! val)
      (->dom val))))



(provide/contract
 [scheme-value->dom-sexp (any/c (or/c false/c dom-parameters?) . -> . any)])