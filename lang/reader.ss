(module reader syntax/module-reader
  -ignored-
  #:wrapper2
  (lambda (in rd stx?)
    (let* ([lang `(file ,(path->string (resolved-module-path-name moby-lang-path)))]
           [mod  (rd in)]
           [mod  (if stx? mod (datum->syntax #f mod))]
           [r (syntax-case mod ()
                [(module name lang* . body)
                 (with-syntax ([lang (datum->syntax
                                      #'lang* lang #'lang*)]
                               [sentinel (datum->syntax #'lang* '(void) #'lang*)])
                   ;; A hack to force the expansion of module to go to module-begin.
                   (syntax/loc mod (module name lang . (sentinel . body))))])])
      (if stx? r (syntax->datum r))))
  
  (require scheme/runtime-path)
  (define-runtime-module-path moby-lang-path "../moby-lang.ss"))