#lang scheme/base
(require scheme/class
         drscheme/tool
         string-constants/string-constant
         scheme/unit)

;; Moby language hooks.


;; Copied from the setup in Simply Scheme

             
(provide tool@)

  
(define-unit tool@
  (import drscheme:tool^)
  (export drscheme:tool-exports^)
  
  (define language-base%
    (class* drscheme:language:simple-module-based-language%
      (drscheme:language:simple-module-based-language<%>)
      
      (define (get-language-numbers)
          '(-500 500))

      (define (get-language-position)
        (list (string-constant teaching-languages)
              "Moby"))
      (define (get-module)
        '(lib "moby-lang.ss" "moby"))

      (define (get-one-line-summary)
        "Moby")
      
      (super-new [module (get-module)]
                 [language-position (get-language-position)]
                 [language-numbers (get-language-numbers)]
                 [one-line-summary (get-one-line-summary)]
                 [documentation-reference #f])))
  
  (define language%
    (class (drscheme:language:module-based-language->language-mixin
            (drscheme:language:simple-module-based-language->module-based-language-mixin
             language-base%))
      ;; We need to flag use-namespace-require/copy to prevent
      ;; a weird bug.  See:
      ;; http://list.cs.brown.edu/pipermail/plt-scheme/2007-February/016390.html
      (define/override (use-namespace-require/copy?) #t)
      (super-instantiate ())))
  
  
  (define (phase1) (void))
  (define (phase2)
    (drscheme:language-configuration:add-language
     (make-object ((drscheme:language:get-default-mixin) language%)))))
