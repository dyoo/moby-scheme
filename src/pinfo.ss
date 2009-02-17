#lang scheme

(require "env.ss"
         "toplevel.ss"
         "helpers.ss")

;; pinfo (program-info) captures the information we get from analyzing 
;; the program.
(define-struct pinfo (env) #:transparent)
(define empty-pinfo (make-pinfo empty-env))

;; pinfo-accumulate-binding: binding pinfo -> pinfo
(define (pinfo-accumulate-binding a-binding a-pinfo)
  (make-pinfo
   (env-extend (pinfo-env a-pinfo) a-binding)))


;; program-analyze: program [program-info] -> program-info
;; Collects which identifiers are free or definition-bound by the program.
(define (program-analyze a-program [pinfo empty-pinfo])
  ;; fixme to do free variable analysis.  We want to error early if the user
  ;; tries to use an identifier that hasn't been bound.
  (program-analyze-collect-definitions a-program pinfo))


;; program-analyze-collect-definitions: program pinfo -> pinfo
(define (program-analyze-collect-definitions a-program pinfo)
  (cond [(empty? a-program)
         pinfo]
        [else
         (let ([updated-pinfo
                (cond [(defn? (first a-program))
                       (definition-analyze-collect-definitions (first a-program) pinfo)]
                      [(test-case? (first a-program))
                       ;; Test cases don't introduce any new definitions, so just return.
                       pinfo]
                      [(library-require? (first a-program))
                       ;; Fixme!
                       (error 'program-top-level-identifiers 
                              "I don't know how to handle require yet")]
                      [(expression? (first a-program))
                       ;; Expressions don't introduce any new definitions, so just return.
                       pinfo])])
           (program-analyze-collect-definitions (rest a-program)
                                                updated-pinfo))]))


;; definition-analyze-collect-definitions: definition program-info -> program-info
(define (definition-analyze-collect-definitions a-definition pinfo)
  (match a-definition
    [(list 'define (list id args ...) body)
     (pinfo-accumulate-binding (make-binding:function id
                                                      #f
                                                      (length args) 
                                                      #f 
                                                      (identifier->munged-java-identifier id))
                               pinfo)]
    [(list 'define (? symbol? id) (list 'lambda (list args ...) body))
     (pinfo-accumulate-binding (make-binding:function id
                                                      #f
                                                      (length args) 
                                                      #f 
                                                      (identifier->munged-java-identifier id))
                               pinfo)]
    [(list 'define (? symbol? id) body)
     (pinfo-accumulate-binding (make-binding:constant id
                                                      (identifier->munged-java-identifier id))
                               pinfo)]
    
    
    [(list 'define-struct id (list fields ...))
     (let* ([constructor-id 
             (string->symbol (format "make-~a" id))]
            [constructor-binding 
             (make-binding:function constructor-id #f (length fields) #f
                                    (identifier->munged-java-identifier id))]
            
            [selector-ids
             (map (lambda (f)
                    (string->symbol (format "~a-~a" id f)))
                  fields)]
            [selector-bindings
             (map (lambda (sel-id) 
                    (make-binding:function sel-id #f 2 #f 
                                           (identifier->munged-java-identifier sel-id)))
                  selector-ids)])
       (foldl pinfo-accumulate-binding pinfo 
              (cons (cons constructor-binding selector-bindings))))]))
