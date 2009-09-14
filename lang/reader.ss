(module reader syntax/module-reader
  -ignored-
  #:wrapper2
  (lambda (in rd stx?)
    (let* ([lang `(file ,(path->string (resolved-module-path-name moby-lang-path)))]
           [mod  (rd in)]
           [mod  (if stx? mod (datum->syntax #f mod))]
           [r (syntax-case mod ()
                [(module name lang* . body)
                 (cond
                   [(not (null? (cdr (syntax-e #'body))))
                    (with-syntax ([lang (datum->syntax
                                         #'lang* lang #'lang*)])
                      (syntax/loc mod (module name lang . body)))]
                   [else
                    ;; A hack to force the expansion of module to go to module-begin.
                    ;; We just add another (void) expression at the top, just to ensure that
                    ;; module will introduce the #%module-begin at the toplevel.
                    (with-syntax ([lang (datum->syntax
                                         #'lang* lang #'lang*)]
                                  [sentinel (datum->syntax #'lang* '(void) #'lang*)])
                      (syntax/loc mod (module name lang . (sentinel . body))))])])])
      
      (if stx? r (syntax->datum r))))
  
  (require scheme/runtime-path)
  (define-runtime-module-path moby-lang-path "../moby-lang.ss"))