; valid-types-helper.ss

(module valid-types-helper mzscheme
  
  (require "xml-helper.ss")
  
  (provide type-exists?
           type-exists/ext?
           attr-exists?)
  
  (define (type-exists? tag)
    (type->tag tag))
  
  (define (type-exists/ext? tag)
    (if (memq (syntax-e tag) '(*element* *comment* *text* *data*))
        tag
        (type->tag tag)))
  
  (define (attr-exists? tag)
    (attribute->tag tag))
  
  (define (keyword-exists? tag)
    (syntax-local-value tag (lambda () #f)))
  
  )
