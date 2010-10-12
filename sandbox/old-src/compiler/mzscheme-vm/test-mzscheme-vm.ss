#lang scheme/base

(require test-engine/scheme-tests
         "mzscheme-vm.ss"
         "env.ss"
         "../pinfo.ss"
         "../../collects/moby/runtime/stx.ss"
         "../../stx-helpers.ss"
         (except-in "../../../support/externals/mzscheme-vm/src/bytecode-structs.ss" stx?))

(require (for-syntax scheme/base))


(define-syntax (s stx)
  (syntax-case stx ()
    [(_ e)
     (syntax/loc stx 
       (syntax->stx #'e))]))

(define-syntax (c stx)
  (syntax-case stx ()
    [(_ e)
     (syntax/loc stx 
       (let-values ([(bytecode pinfo)
                     (compile-expression (s e)
                                         empty-env
                                         empty-pinfo)])
         bytecode))]))



(check-expect (c 42) 42)
(check-expect (c "hello") "hello")
(check-expect (c #t) #t)
(check-expect (c #f) #f)
(check-expect (c #\z) #\z)
(check-expect (c (if #t 3 4))
              (make-branch #t 3 4))
(check-expect (c (begin "hello" "world"))
              (make-seq (list "hello" "world")))


;; identifiers and lambdas
(check-error 
 (c x)
 "compile-identifier-expression: Couldn't find x in the environment")


(check-expect (c (lambda () 42))
              (make-lam '()
                        '()
                        0
                        '()
                        #f
                        #()
                        '()
                        0
                        42))


(check-expect (c (lambda (x y) x))
              (make-lam '() 
                        '() 
                        2 
                        '(val val) 
                        #f 
                        #() 
                        '() 
                        0
                        (make-localref #f 0 #f #f #f)))


(check-expect (c (lambda (x y z) z))
              (make-lam '() 
                        '() 
                        3 
                        '(val val val) 
                        #f 
                        #() 
                        '() 
                        0
                        (make-localref #f 2 #f #f #f)))


(check-expect (c (lambda (x y) 
                   (lambda (z) x)))
              (make-lam '() 
                        '() 
                        2 
                        '(val val) 
                        #f 
                        #() 
                        '() 
                        0
                        (make-lam '() 
                                  '() 
                                  1 
                                  '(val) 
                                  #f 
                                  #(0) 
                                  '(val/ref) 
                                  0
                                  (make-localref #f 0 #f #f #f))))



(check-expect (free-variables (s x) empty-env) '(x))
(check-expect (free-variables (s (if x y z)) empty-env) '(x y z))
(check-expect (free-variables (s 42) empty-env) '())


(check-expect (c (lambda (x y) 
                   (lambda (z) z)))
              (make-lam '() 
                        '() 
                        2 
                        '(val val) 
                        #f 
                        #() 
                        '() 
                        0
                        (make-lam '() 
                                  '() 
                                  1 
                                  '(val) 
                                  #f 
                                  #() 
                                  '() 
                                  0
                                  (make-localref #f 0 #f #f #f))))



(check-expect (c (lambda (x y) 
                   (lambda (z) y)))
              (make-lam '() 
                        '() 
                        2 
                        '(val val) 
                        #f 
                        #() 
                        '() 
                        0
                        (make-lam '() 
                                  '() 
                                  1 
                                  '(val) 
                                  #f 
                                  #(1) 
                                  '(val/ref) 
                                  0
                                  (make-localref #f 0 #f #f #f))))


;; Quotations
(check-expect (c (quote test))
            'test)

(check-expect (c (quote (3 4 five)))
            '(3 4 five))



;; Application

(check-expect (c (lambda (x y) (x y y)))
              (make-lam '()
                        '()
                        2
                        '(val val)
                        #f
                        #()
                        '()
                        0
                        (make-application (make-localref #f 2 #f #f #f)
                                          (list (make-localref #f 3 #f #f #f)
                                                (make-localref #f 3 #f #f #f)))))





;; Module compilation
;; FIXME: currently not implmented correctly yet
#;(check-expect (let-values ([(compiled-module pinfo)
                            (compile-compilation-top 
                             (stx-e (s ((define (f x) (* x x))
                                        (f 42))))
                             (get-base-pinfo 'moby)
                             #:name 'my-program)])
                compiled-module)
              
              (make-compilation-top
               0
               (make-prefix 0 '() '())
               (make-mod 'my-program 
                         (module-path-index-join #f #f)
                         (make-prefix 0 
                                      (list #f 
                                            (make-module-variable 
                                             (module-path-index-join 
                                              "moby/toplevel"
                                              (module-path-index-join #f #f))
                                             '*
                                             -1
                                             0))
                                      '())
                         '()  ; provides
                         '()  ; requires
                         (list (make-application (make-toplevel 1 0 #f #f)
                                                 (list 42)))  ; body
                         '()  ; syntax-body
                         '(()()())  ; unexported
                         0    ; max-let-depth
                         (make-toplevel 0 0 #f #f) ; dummy
                         #f ; lang-info
                         #f ; internal-context
                         )))
                                      



;; local: implemented as let-void
(check-expect (c (local [] 42))
              42)

(check-expect (c (local [(define x 42)] x))
              (make-let-void 1 #t
                             (make-install-value 
                              1
                              0
                              #t
                              42
                              (make-localref  #t 0 #f #f #f))))



(check-expect (c (local [(define x 42)
                         (define y 44)] 
                   x))
              (make-let-void 2 #t
                             (make-install-value 
                              1
                              0
                              #t
                              42
                              (make-install-value
                               1
                               1
                               #t
                               44
                               (make-localref #t 0 #f #f #f)))))

(check-expect (c (local [(define x 42)
                         (define-values (y z) 44)] 
                   x))
              (make-let-void 3 #t
                             (make-install-value 
                              1
                              0
                              #t
                              42
                              (make-install-value
                               2
                               1
                               #t
                               44
                               (make-localref #t 0 #f #f #f)))))


(check-expect (c (local [(define x 42)
                         (define-values (y z) 44)] 
                   y))
              (make-let-void 3 #t
                             (make-install-value 
                              1
                              0
                              #t
                              42
                              (make-install-value
                               2
                               1
                               #t
                               44
                               (make-localref #t 1 #f #f #f)))))



(check-expect (c (local [(define x 42)
                         (define-values (y z) 44)] 
                   z))
              (make-let-void 3 #t
                             (make-install-value 
                              1
                              0
                              #t
                              42
                              (make-install-value
                               2
                               1
                               #t
                               44
                               (make-localref #t 2 #f #f #f)))))


(check-expect (c (local [(define (f x) 
                           (f x))]
                   (f 3)))
              (make-let-void 1 #t
                             (make-install-value 
                              1
                              0
                              #t
                              (make-lam 'f
                                        '()
                                        1
                                        '(val)
                                        #f
                                        #(0)
                                        '(val/ref)
                                        0
                                        (make-application
                                         (make-localref #t 1 #f #f #f)
                                         (list     
                                          (make-localref #f 2 #f #f #f))))
                              (make-application
                               (make-localref #t 1 #f #f #f)
                               (list 3)))))


(test)