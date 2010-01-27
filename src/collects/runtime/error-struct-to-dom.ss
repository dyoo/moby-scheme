#lang s-exp "private/restricted-runtime-scheme.ss"

(require "error-struct.ss")
(require "stx.ss")

;; Error structure to dom code.
;; These functions produce DOMs out of the values in error-struct,
;; ready to be styled.



;; error-struct-to-dom-sexp: dom -> sexp
(define (moby-error-struct-to-dom-sexp an-error)
  (local [(define embedded-location (moby-error-location an-error))
          (define error-type (moby-error-error-type an-error))
          (define (add-toplevel-dom-error-wrapper a-dom)
            `(span ((class "Error"))
                   ,a-dom
                   (span ((class "Error.location"))
                         ,(Loc->dom-sexp embedded-location)
                   )))]
    
    (add-toplevel-dom-error-wrapper
     (cond
       [(moby-error-type:unclosed-lexical-token? error-type)
        `(span ((class "Error:UnclosedLexicalToken"))
               (span ((class "Error.reason"))
                     "I saw "
                     ,(symbol->string 
                       (moby-error-type:unclosed-lexical-token-opener error-type))
                     " to start a "
                     ,(moby-error-type:unclosed-lexical-token-type error-type)
                     ", but no "
                     (span ((class "MobyLexicalToken"))
                           ,(symbol->string 
                             (moby-error-type:unclosed-lexical-token-closer error-type)))
                     "to close it.")
               
               (span ((class "Error:UnclosedLexicalToken.type"))
                     ,(symbol->string (moby-error-type:unclosed-lexical-token-type error-type)))
               (span ((class "Error:UnclosedLexicalToken.opener"))
                     ,(symbol->string (moby-error-type:unclosed-lexical-token-opener error-type)))
               (span ((class "Error:UnclosedLexicalToken.closer"))
                     ,(symbol->string (moby-error-type:unclosed-lexical-token-closer error-type))))]
       
       
       [(moby-error-type:unrecognized-lexical-token? error-type)
        `(span ((class "Error:UnrecognizedLexicalToken"))
               (span ((class "Error.reason"))
                     "I saw "
                     ,(symbol->string (moby-error-type:unrecognized-lexical-token-token error-type))
                     " which I don't recognize as a program element.")
               (span ((class "Error:UnrecognizedLexicalToken.token"))
                     (symbol->string (moby-error-type:unrecognized-lexical-token-token error-type))))]
       
       [(moby-error-type:unsupported-lexical-token? error-type)
        `(span ((class "Error:UnsupportedLexicalToken"))
               (span ((class "Error.reason"))
                     ,(symbol->string (moby-error-type:unsupported-lexical-token-token error-type))
                     " is currently not supported.")
               (span ((class "Error:UnsupportedLexicalToken.token"))
                     ,(symbol->string (moby-error-type:unsupported-lexical-token-token error-type))))]
       
       [(moby-error-type:unsupported-expression-form? error-type)
        `(span ((class "Error:UnsupportedExpressionForm"))
               (span ((class "Error.reason"))
                     ,(stx-to-dom-sexp (moby-error-type:unsupported-expression-form-expr error-type))
                     " is currently not supported.")
               (span ((class "Error:UnsupportedExpressionForm.expr"))
                     ,(stx-to-dom-sexp (moby-error-type:unsupported-expression-form-expr error-type))))]
       

       
       
       [(moby-error-type:unclosed-parentheses? error-type)
        `(span ((class "Error:UnclosedParentheses"))
               (span ((class "Error.reason"))
                     "I saw "
                     (symbol->string (moby-error-type:unclosed-parentheses error-type))
                     " to start an expression, but no "
                     (symbol->string (moby-error-type:unclosed-parentheses error-type))
                     "to close it.")
               (span ((class "Error:UnclosedParentheses.opener"))
                     (symbol->string (moby-error-type:unclosed-parentheses-opener error-type)))
               (span ((class "Error:UnclosedParentheses.closer"))
                     (symbol->string (moby-error-type:unclosed-parentheses-closer error-type))))]
       
       [(moby-error-type:missing-expression? error-type)
        `(span ((class "Error:MissingExpression"))
               (span ((class "Error.reason"))
                     "I expected an expression following "
                     ,(symbol->string (moby-error-type:missing-expression-token error-type))
                     " but did not find one."))]
       
       [(moby-error-type:duplicate-identifier? error-type)
        `(span ((class "Error:DuplicateIdentifier"))
               (span ((class "Error.reason"))
                     "The identifier "
                     ,(symbol->string (moby-error-type:duplicate-identifier-id error-type))
                     " has been duplicated.")
               (span ((class "Error:DuplicateIdentifier.secondLocation"))
                     ,(Loc->dom-sexp (moby-error-type:duplicate-identifier-second-location error-type))))]

       [(moby-error-type:expected-identifier? error-type)
        `(span ((class "Error:ExpectedIdentifier"))
               (span ((class "Error.reason"))
                     "I expected an identifier but received "
                     ,(stx-to-dom-sexp (moby-error-type:expected-identifier-observed error-type))
                     " instead."))]
       
       [(moby-error-type:undefined-identifier? error-type)
        `(span ((class "Error:UndefinedIdentifier"))
               (span ((class "Error.reason"))
                     "I don't know what "
                     ,(scheme-value-to-dom-sexp (moby-error-type:undefined-identifier-id error-type))
                     " is; it's not defined as an input or a primitive."))]

       
       [(moby-error-type:structure-identifier-not-expression? error-type)
        `(span ((class "Error:StructureIdentifierNotExpression"))
               (span ((class "Error.reason"))
                     "The structure name "
                     ,(scheme-value-to-dom-sexp (moby-error-type:structure-identifier-not-expression-id
                                                 error-type))
                     " can't be used as an expression."))]

       
       [(moby-error-type:provided-name-not-defined? error-type)
        `(span ((class "Error:ProvidedNameNotDefined"))
               (span ((class "Error.reason"))
                     "The provided name "
                     ,(scheme-value-to-dom-sexp (moby-error-type:provided-name-not-defined-id error-type))
                     "is not defined in the program."))]

       [(moby-error-type:provided-structure-not-structure? error-type)
        `(span ((class "Error:ProvidedStructureNotStructure"))
               (span ((class "Error.reason"))
                     "The provided name "
                     ,(scheme-value-to-dom-sexp 
                       (moby-error-type:provided-structure-not-structure-id error-type))
                     "is defined in the program, but is not the name of a structure."))]

       [(moby-error-type:unknown-module? error-type)
        `(span ((class "Error:UnknownModule"))
               (span ((class "Error.reason"))
                     "I see a require of the module "
                     `(scheme-value-to-dom-sexp (moby-error-type:unknown-module-path error-type))
                     ", but I don't yet know what this module is."))]
       
       [(moby-error-type:application-arity? error-type)
        `(span ((class "Error:ApplicationArity"))
               (span ((class "Error.reason"))
                     "The function "
                     ,(scheme-value-to-dom-sexp (moby-error-type:application-arity-who error-type))
                     " expects "
                     ,(arity-to-dom-sexp (moby-error-type:application-arity-expected error-type))
                     " but instead I see "
                     ,(scheme-value-to-dom-sexp (moby-error-type:application-arity-observed error-type))
                     ))]

       [(moby-error-type:application-operator-not-a-function? error-type)
        `(span ((class "Error:ApplicationOperatorNotAFunction"))
               (span ((class "Error.reason"))
                     "The operator "
                     ,(scheme-value-to-dom-sexp (moby-error-type:application-operator-not-a-function-who error-type))
                     " is a value, but it isn't a function."))]

       
       [(moby-error-type:type-mismatch? error-type)
        `(span ((class "Error:TypeMismatch"))
               (span ((class "Error.reason"))
                     "The function "
                     ,(scheme-value-to-dom-sexp 
                       (moby-error-type:type-mismatch-who error-type))
                     " expects"
                     ,(expected-value-to-dom-sexp
                       (moby-error-type:type-mismatch-expected error-type))
                     "as its "
                     ,(scheme-value-to-dom-sexp
                       (moby-error-type:type-mismatch-position error-type))
                     " argument, but instead I see "
                     ,(scheme-value-to-dom-sexp 
                       (moby-error-type:type-mismatch-observed error-type))
                     "."))]

       [(moby-error-type:index-out-of-bounds? error-type)
        `(span ((class "Error:IndexOutOfBounds"))
               (span ((class "Error.reason"))
                     "The index "
                     ,(scheme-value-to-dom-sexp
                       (moby-error-type:index-out-of-bounds-observed error-type))
                     " is not within the expected boundary ["
                     ,(scheme-value-to-dom-sexp 
                       (moby-error-type:index-out-of-bounds-minimum error-type))
                     ", "
                     ,(scheme-value-to-dom-sexp 
                       (moby-error-type:index-out-of-bounds-maximum error-type))
                     "]"
                     ))]
       
       [(moby-error-type:conditional-exhausted? error-type)
        `(span ((class "Error:ConditionalExhausted"))
               (span ((class "Error.reason"))
                     "All of the questions inside a cond were false, "
                     "and at least one of them has to be true."))]
       
       [(moby-error-type:generic-runtime-error? error-type)
        `(span ((class "Error:GenericRuntimeError"))
               (span ((class "Error.reason"))
                     ,(moby-error-type:generic-runtime-error-reason error-type)))]

       [(moby-error-type:generic-syntactic-error? error-type)
        `(span ((class "Error:GenericSyntacticError"))
               (span ((class "Error.reason"))
                     ,(moby-error-type:generic-syntactic-error-reason error-type))
               (span ((class "Error:GenericSyntacticError.otherLocations"))
                     ,@(map Loc->dom-sexp 
                            (moby-error-type:generic-syntactic-error-other-locations error-type))))]))))
  


;; Loc->dom-sexp: loc -> sexp
;; Given a location, produce a dom representation of that location.
(define (Loc->dom-sexp a-loc)
  `(span ((class "Location"))
         (span ((class "Location.offset")) ,(number->string (Loc-offset a-loc)))
         (span ((class "Location.line")) ,(number->string (Loc-line a-loc)))
         (span ((class "Location.column")) ,(number->string (Loc-column a-loc)))
         (span ((class "Location.span")) ,(number->string (Loc-span a-loc)))
         (span ((class "Location.id")) ,(Loc-id a-loc))))
         



(define-struct label (n ;; number
                      ))

;; scheme-value-to-dom-sexp: any -> dom
;; Converts a value to a DOM.
;; FIXME: make this extensible so structures can define how they'd like to be DOMified.
(define (scheme-value-to-dom-sexp val)
  (local [;; Keeps track of the values we've domified so far.
          (define labeled-vals (make-hasheq))
          
          ;; Keeps track of the set of values that have been shared within the val.
          (define shared (make-hasheq))
          
          (define counter 0)
            
          (define (initialize-shared-hash! x)
            (local [(define h (make-hasheq))
                    (define (loop x)
                      (cond
                        [(hash-ref h x false)
                         (begin
                           (hash-set! shared h true)
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

          
          
          (define (->dom val)
            (cond
              [;; If this is the first time we're encountering the shared value,
               ;; label it.
               (and (hash-ref shared val false)
                    (not (label? (hash-ref labeled-vals val false))))
               (begin
                 (printf "Labeling ~s~n" val)
                 (set! counter (add1 counter))
                 (hash-set! labeled-vals val (make-label counter))
                 `(span ((class "SchemeValue:SharedLabel"))
                        (span ((class "SchemeValue:SharedLabel.label")) 
                              ,(string-append "#" (number->string counter) "=")
                              )
                        (span ((class "SchemeValue:SharedLabel.item"))
                              ,(->dom* val false))))]
              [else
               (->dom* val true)]))
      
          
          (define (->dom* val allow-labeling?)
            (cond
              [(and allow-labeling? 
                    (label? (hash-ref labeled-vals val #f)))
               (local [(define a-label (hash-ref labeled-vals val #f))]
                 `(span ((class "SchemeValue:SharedReference"))
                        ,(string-append "#" (number->string (label-n a-label)))))]
              ;;;;
              
              [(string? val)
               `(span ((class "SchemeValue:String"))
                      ,val)]
              
              [(number? val)
               `(span ((class "SchemeValue:Number"))
                      ,(number->string val))]
              
              [(boolean? val)
               `(span ((class "SchemeValue:Boolean"))
                      ,(if val "true" "false"))]
              
              [(char? val)
               `(span ((class "SchemeValue:Character"))
                      (string val))]
              
              [(symbol? val)
               `(span ((class "SchemeValue:Symbol"))
                      (symbol->string val))]
              
              [(list? val)
               (begin
                 `(span ((class "SchemeValue:List"))
                        (span ((class "SchemeValue:List.lparen")) "(")
                        (span ((class "SchemeValue:List.keyword")) "list")
                        ,@(map (lambda (x)
                                 `(span ((class "SchemeValue:List.item"))
                                        ,(->dom x)))
                               val)
                        (span ((class "SchemeValue:List.rparen")) ")")))]
              
              [(vector? val)
               (begin
                 `(span ((class "SchemeValue:Vector"))
                        (span ((class "SchemeValue:Vector.lparen")) "(")
                        (span ((class "SchemeValue.Vector.keyword")) "vector")
                        ,@(map (lambda (x)
                                 `(span ((class "SchemeValue:Vector.item"))
                                        ,(->dom x)))
                               (vector->list val))
                        (span ((class "SchemeValue:Vector.rparen")) ")")))]
              
              [(struct? val)
               `(span ((class "SchemeValue:Structure"))
                      ;; FIXME: we need primitives to get at structure contents.
                      "<struct>"
                      )]
              
              [(box? val)
               (begin
                 `(span ((class "SchemeValue:Box"))
                        (span ((class "SchemeValue:Box.lparen")) "(")
                        (span ((class "SchemeValue:Box.keyword")) "box")
                        (span ((class "SchemeValue:Box.item")) 
                              ,(->dom (unbox val)))
                        (span ((class "SchemeValue:Box.rparen")) ")")))]
              
              [(hash? val)
               `(span ((class "SchemeValue:Hash"))
                      ;; FIXME: we should show contents.
                      "<hash>")]
              
              [(procedure? val)
               `(span ((class "SchemeValue:Function"))
                      ;; FIXME
                      ;; We need to get at the function name, if we can get at it.
                      ;; We also would like inspectors for the arity and
                      ;; the location the function's defined.
                      "<function>")]
              
              #;[(undefined? val)
                 ...]
              
              [else
               `(span ((class "SchemeValue:DisplayedObject"))
                      (format "~a" val))]))]
    (begin
      (initialize-shared-hash! val)
      (->dom val))))
          


;; stx-to-dom-sexp: stx -> dom
(define (stx-to-dom-sexp a-stx)
  (scheme-value-to-dom-sexp 
   (stx->datum a-stx)))




(define (expected-value-to-dom-sexp expected)
  (cond 
    [(moby-expected:string? expected)
     `(span ((class "Expected:String"))
            "<string>")]
    [(moby-expected:integer? expected)
     `(span ((class "Expected:Integer"))
            "<integer>")]
    [(moby-expected:natural? expected)
     `(span ((class "Expected:Natural"))
            "<natural>")]
    [(moby-expected:rational? expected)
     `(span ((class "Expected:Rational"))
            "<rational>")]
    [(moby-expected:real? expected)
     `(span ((class "Expected:Real"))
            "<real>")]
    [(moby-expected:complex? expected)
    `(span ((class "Expected:Complex"))
           "<complex>")]
    [(moby-expected:number? expected)
     `(span ((class "Expected:Number"))
            "<number>")]
    [(moby-expected:boolean? expected)
     `(span ((class "Expected:Boolean"))
            "<boolean>")]
    [(moby-expected:char? expected)
     `(span ((class "Expected:Char"))
            "<char>")]
    [(moby-expected:symbol? expected)
     `(span ((class "Expected:Symbol"))
            "<symbol>")]
    [(moby-expected:list? expected)
     `(span ((class "Expected:List"))
            "<list>")]
    [(moby-expected:vector? expected)
     `(span ((class "Expected:Vector"))
            "<vector>")]
    [(moby-expected:struct? expected)
     `(span ((class "Expected:Struct"))
            "<struct>")]
    [(moby-expected:box? expected)
     `(span ((class "Expected:Box"))
            "<box>")]
    [(moby-expected:hash? expected)
     `(span ((class "Expected:Hash"))
            "<hash>")]
    [(moby-expected:function? expected)
     `(span ((class "Expected:Function"))
            "<function>")]
    [(moby-expected:something? expected)
     `(span ((class "Expected:Something"))
            "<something>")]))




(define (arity-to-dom-sexp an-arity)
  "fixme")

    
  

(provide moby-error-struct-to-dom-sexp)