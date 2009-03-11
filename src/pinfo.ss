#lang scheme

(require "env.ss"
         "toplevel.ss"
         "helpers.ss"
         syntax/modresolve
         scheme/contract
         scheme/runtime-path)


(define-runtime-path mock-lib-path "../stub")


;; pinfo (program-info) captures the information we get from analyzing 
;; the program.
(define-struct pinfo (env) #:transparent)
(define empty-pinfo (make-pinfo empty-env))

;; pinfo-accumulate-binding: binding pinfo -> pinfo
(define (pinfo-accumulate-binding a-binding a-pinfo)
  (make-pinfo
   (env-extend (pinfo-env a-pinfo) a-binding)))


;; pinfo-accumulate-bindings: (listof binding) pinfo -> pinfo
(define (pinfo-accumulate-bindings bindings a-pinfo)
  (foldl pinfo-accumulate-binding
         a-pinfo
         bindings))



;; program-analyze: program [program-info] -> program-info
;; Collects which identifiers are free or definition-bound by the program.
(define (program-analyze a-program [pinfo (make-pinfo (get-toplevel-env))])
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
                       (require-analyze (second (first a-program)) pinfo)]
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
                                                      (symbol->string
                                                       (identifier->munged-java-identifier id)))
                               pinfo)]
    [(list 'define (? symbol? id) (list 'lambda (list args ...) body))
     (pinfo-accumulate-binding (make-binding:function id
                                                      #f
                                                      (length args) 
                                                      #f 
                                                      (symbol->string
                                                       (identifier->munged-java-identifier id)))
                               pinfo)]
    [(list 'define (? symbol? id) body)
     (pinfo-accumulate-binding (make-binding:constant id
                                                      (symbol->string 
                                                       (identifier->munged-java-identifier id)))
                               pinfo)]
    
    
    [(list 'define-struct id (list fields ...))
     (let* ([constructor-id 
             (string->symbol (format "make-~a" id))]
            [constructor-binding 
             (make-binding:function constructor-id #f (length fields) #f
                                    (symbol->string
                                     (identifier->munged-java-identifier constructor-id)))]
            
            [selector-ids
             (map (lambda (f)
                    (string->symbol (format "~a-~a" id f)))
                  fields)]
            [selector-bindings
             (map (lambda (sel-id) 
                    (make-binding:function sel-id #f 2 #f 
                                           (symbol->string
                                            (identifier->munged-java-identifier sel-id))))
                  selector-ids)])
       (foldl pinfo-accumulate-binding pinfo 
              (cons constructor-binding selector-bindings)))]))





;; pinfo-android-permissions: pinfo -> (listof string)
;; Computes the permissions needed.
(define (pinfo-android-permissions a-pinfo)
  ;; Fixme: currently hardcoded.
  '("android.permission.ACCESS_LOCATION"
    "android.permission.ACCESS_GPS"))



(define-struct module-binding (name path bindings))


;; TODO: syntactic abstraction using the stuff in each mock library, to reduce this
;; drudgery.
(define location-module 
  (let ([module-path 
         (build-path mock-lib-path "location.ss")])
    (make-module-binding 'location
                         module-path
                         (list (make-binding:function 'get-latitude module-path 0 #f 
                                                      "org.plt.lib.Location.getLatitude")
                               (make-binding:function 'get-longitude module-path 0 #f 
                                                      "org.plt.lib.Location.getLongitude")
                               (make-binding:function 'get-attitude module-path 0 #f 
                                                      "org.plt.lib.Location.getAttitude")
                               (make-binding:function 'get-bearing module-path 0 #f 
                                                      "org.plt.lib.Location.getBearing")
                               (make-binding:function 'get-speed module-path 0 #f 
                                                      "org.plt.lib.Location.getSpeed")))))

  

(define tilt-module 
  (let ([module-path 
         (build-path mock-lib-path "tilt.ss")])
    (make-module-binding 'tilt
                         module-path
                         (list (make-binding:function 'get-x-tilt module-path 0 #f 
                                                      "org.plt.lib.Tilt.getXTilt")
                               (make-binding:function 'get-y-tilt module-path 0 #f 
                                                      "org.plt.lib.Tilt.getYTilt")
                               (make-binding:function 'get-z-tilt module-path 0 #f 
                                                      "org.plt.lib.Location.getZTilt")))))




;; extend-env/module-binding: env module-binding -> env
;; Extends an environment with the bindings associated to a module.
(define (extend-env/module-binding an-env a-module-binding)
  (let loop ([an-env an-env]
             [contents (module-binding-bindings a-module-binding)])
    (cond
      [(empty? contents)
       an-env]
      [else
       (loop (env-extend an-env (first contents))
             (rest contents))])))

                          
(define known-modules (list location-module
                            tilt-module))
                                             


;; require-analyze: require-path -> pinfo
(define (require-analyze require-path pinfo)
  (let loop ([modules known-modules])
    (cond
      [(empty? modules)
       (error 'require-analyze "Moby doesn't know about module ~s yet"
              require-path)]
      [(path=? (resolve-module-path require-path #f)
               (module-binding-path (first modules)))
       (pinfo-accumulate-bindings (module-binding-bindings (first modules))
                                  pinfo)]
      [else
       (loop (rest modules))])))








(provide/contract [struct pinfo ([env env?])]
                  [program-analyze ((program?) (pinfo?) . ->* . pinfo?)])