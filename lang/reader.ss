(module reader syntax/module-reader
  -ignored-
  
  #:wrapper1
  (lambda (t stx?)
    (let* ([body (t)]
           [wrapped-body `((#%module-begin . ,body))])
      (if stx?
          (datum->syntax #f wrapped-body)
          wrapped-body)))
  
  #:wrapper2
  (lambda (in rd stx?)
    (let* ([lang `(file ,(path->string (resolved-module-path-name moby-lang-path)))]
           [mod
            (parameterize ([read-decimal-as-inexact #f])
              (rd in))]
           [mod  (if stx? mod (datum->syntax #f mod))]
           [r 
            (syntax-case mod ()
              [(module name lang* . body)
               (with-syntax ([lang (datum->syntax #'lang* lang #'lang*)])
                 (syntax/loc mod (module name lang . body)))])])
      (if stx? r (syntax->datum r))))
  
  (require scheme/runtime-path)
  (define-runtime-module-path moby-lang-path "../moby-lang.ss"))