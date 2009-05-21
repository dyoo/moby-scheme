#lang s-exp "lang.ss"

(require "env.ss")
(require "toplevel.ss")
(require "helpers.ss")
(require "permission.ss")




;; pinfo (program-info) captures the information we get from analyzing 
;; the program.
(define-struct pinfo (env modules used-bindings))

;; pinfo
(define empty-pinfo
  (make-pinfo empty-env empty (make-immutable-hash empty)))

;; get-base-pinfo: pinfo
(define (get-base-pinfo _)
  (make-pinfo toplevel-env empty (make-immutable-hash empty)))


;; pinfo-update-env: pinfo env -> pinfo
(define (pinfo-update-env a-pinfo an-env)
  (make-pinfo
   an-env
   (pinfo-modules a-pinfo)
   (pinfo-used-bindings a-pinfo)))

;; pinfo-accumulate-binding: binding pinfo -> pinfo
(define (pinfo-accumulate-binding a-binding a-pinfo)
  (make-pinfo
   (env-extend (pinfo-env a-pinfo) a-binding)
   (pinfo-modules a-pinfo)
   (pinfo-used-bindings a-pinfo)))

;; pinfo-accumulate-bindings: (listof binding) pinfo -> pinfo
(define (pinfo-accumulate-bindings bindings a-pinfo)
  (foldl pinfo-accumulate-binding
         a-pinfo
         bindings))

;; pinfo-accumulate-module: module-binding pinfo -> pinfo
(define (pinfo-accumulate-module a-module a-pinfo)
  (make-pinfo (pinfo-env a-pinfo)
              (cons a-module (pinfo-modules a-pinfo))
              (pinfo-used-bindings a-pinfo)))

;; pinfo-accumulate-binding-use: binding pinfo -> pinfo
(define (pinfo-accumulate-binding-use a-binding a-pinfo)
  (make-pinfo (pinfo-env a-pinfo)
              (pinfo-modules a-pinfo)
              (hash-set (pinfo-used-bindings a-pinfo)
                        a-binding
                        true)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;; program-analyze: program [program-info] -> program-info
;; Collects which identifiers are defined by the program, and which identifiers
;; are actively used.

(define (program-analyze a-program)
  (program-analyze/pinfo a-program (get-base-pinfo '_)))
  

(define (program-analyze/pinfo a-program pinfo)
  (local [(define pinfo-1
            (program-analyze-collect-definitions a-program pinfo))]
    (program-analyze-uses a-program pinfo-1)))




;; program-analyze-collect-definitions: program pinfo -> pinfo
;; Collects the definitions either imported or defined by this program.
(define (program-analyze-collect-definitions a-program pinfo)
  (cond [(empty? a-program)
         pinfo]
        [else
         (local [(define updated-pinfo
                   (cond [(defn? (first a-program))
                          (definition-analyze-collect-definitions (first a-program) pinfo)]
                         [(test-case? (first a-program))
                          pinfo]
                         [(library-require? (first a-program))
                          (require-analyze (second (first a-program)) pinfo)]
                         [(expression? (first a-program))
                          pinfo]))]
           (program-analyze-collect-definitions (rest a-program)
                                                updated-pinfo))]))



;; program-analyze-uses: program pinfo -> pinfo
(define (program-analyze-uses a-program pinfo)
  (cond [(empty? a-program)
         pinfo]
        [else
         (local [(define updated-pinfo
                   (cond [(defn? (first a-program))
                          (definition-analyze-uses (first a-program) pinfo)]
                         [(test-case? (first a-program))
                          pinfo]
                         [(library-require? (first a-program))
                          pinfo]
                         [(expression? (first a-program))
                          (expression-analyze-uses (first a-program)
                                                   pinfo 
                                                   (pinfo-env pinfo))]))]
           (program-analyze-uses (rest a-program)
                                 updated-pinfo))]))


(define (bf name module-path arity vararity? java-string)
  (make-binding:function name module-path arity vararity? java-string empty false))


;; definition-analyze-collect-definitions: definition program-info -> program-info
;; Collects the defined names introduced by the definition.
(define (definition-analyze-collect-definitions a-definition pinfo)
  (match a-definition
    [(list 'define (list id args ...) body)
     (pinfo-accumulate-binding (bf id
                                                      false
                                                      (length args) 
                                                      false 
                                                      (symbol->string
                                                       (identifier->munged-java-identifier id)))
                               pinfo)]
    [(list 'define (? symbol? id) (list 'lambda (list args ...) body))
     (pinfo-accumulate-binding (bf id
                                                      false
                                                      (length args) 
                                                      false 
                                                      (symbol->string
                                                       (identifier->munged-java-identifier id)))
                               pinfo)]
    [(list 'define (? symbol? id) body)
     (pinfo-accumulate-binding (make-binding:constant id
                                                      (symbol->string 
                                                       (identifier->munged-java-identifier id))
                                                      empty)
                               pinfo)]
    
    
    [(list 'define-struct id (list fields ...))
     (pinfo-update-env pinfo (extend-env/struct-defns (pinfo-env pinfo) id fields))]))



;; extend-env/struct-defns: env symbol (listof symbol) -> env
(define (extend-env/struct-defns an-env id fields)
  (local [(define constructor-id 
            (string->symbol (format "make-~a" id)))
          (define constructor-binding 
            (bf constructor-id false (length fields) false
                (symbol->string
                 (identifier->munged-java-identifier constructor-id))))
          (define predicate-id
            (string->symbol (format "~a?" id)))
          (define predicate-binding
            (bf predicate-id false 1 false
                (symbol->string
                 (identifier->munged-java-identifier predicate-id))))
          (define selector-ids
            (map (lambda (f)
                   (string->symbol (format "~a-~a" id f)))
                 fields))
          (define selector-bindings
            (map (lambda (sel-id) 
                   (bf sel-id false 1 false 
                       (symbol->string
                        (identifier->munged-java-identifier sel-id))))
                 selector-ids))]
    (foldl (lambda (a-binding an-env)
             (env-extend an-env a-binding))
           an-env
           (list* constructor-binding predicate-binding selector-bindings))))

           



;; definition-analyze-uses: definition program-info -> program-info
;; Collects the used names.
(define (definition-analyze-uses a-definition pinfo)
  (match a-definition
    [(list 'define (list id args ...) body)
     (function-definition-analyze-uses id args body pinfo)]
    [(list 'define (? symbol? id) (list 'lambda (list args ...) body))
     (function-definition-analyze-uses id args body pinfo)]   
    [(list 'define (? symbol? id) body)
     (expression-analyze-uses body pinfo (pinfo-env pinfo))]
    [(list 'define-struct id (list fields ...))
     pinfo]))


;; function-definition-analyze-uses: symbol (listof symbol) expression program-info -> program-info
(define (function-definition-analyze-uses fun args body pinfo)
  (local [(define env-1 (pinfo-env pinfo))
          (define env-2 
            (env-extend env-1 (bf fun false (length args) false
                                  (symbol->string fun))))
          (define env-3
            (foldl (lambda (arg-id env) 
                     (env-extend env (make-binding:constant arg-id 
                                                            (symbol->string
                                                             arg-id)
                                                            empty)))
                   env-2
                   args))]
    (expression-analyze-uses body pinfo env-3)))




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; expression-analyze-uses: expression program-info env -> program-info
(define (expression-analyze-uses an-expression pinfo env)
  (match an-expression
    
    [(list 'local [list defns ...] body)
     (local [(define nested-pinfo (foldl (lambda (a-defn a-pinfo)
                                           (definition-analyze-uses a-defn a-pinfo))
                                         pinfo
                                         defns))]
       (expression-analyze-uses body
                                nested-pinfo
                                (pinfo-env nested-pinfo)))]
    
    [(list 'cond [list questions answers] ... [list 'else answer-last])
     (foldl (lambda (e p)
              (expression-analyze-uses e p env))
            pinfo 
            (cons answer-last (append questions answers)))]
    
    
    [(list 'cond [list questions answers] ... [list question-last answer-last])
     (foldl (lambda (e p)
              (expression-analyze-uses e p env))
            pinfo (cons question-last 
                        (cons answer-last 
                              (append questions answers))))]

    
    [(list 'if test consequent alternative)
     (foldl (lambda (e p) (expression-analyze-uses e p env))
            pinfo 
            (list test consequent alternative))]
    
    [(list 'and exprs ...)
     (foldl (lambda (e p) (expression-analyze-uses e p env))
            pinfo 
            exprs)]
    
    [(list 'or exprs ...)
     (foldl (lambda (e p) (expression-analyze-uses e p env))
            pinfo 
            exprs)]
    
    ;; Numbers
    [(? number?)
     pinfo]
    
    ;; Strings
    [(? string?)
     pinfo]
    
    ;; Characters
    [(? char?)
     pinfo]
    
    ;; Identifiers
    [(? symbol?)
     (cond
       [(env-contains? env an-expression)
        (pinfo-accumulate-binding-use (env-lookup env an-expression) pinfo)]
       [else
        pinfo])]
    
    ;; Quoted symbols
    [(list 'quote datum)
     pinfo]
    
    ;; Function call/primitive operation call
    [(list (? symbol? id) exprs ...)
     (local [(define updated-pinfo
               (foldl (lambda (e p)
                        (expression-analyze-uses e p env))
                      pinfo
                      exprs))]
       (cond
         [(env-contains? env id)
          (pinfo-accumulate-binding-use (env-lookup env id) updated-pinfo)]
         [else
          updated-pinfo]))]))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



(define-struct module-binding (name path bindings))


(define world-config-module 
  (local [(define module-path 
            (resolve-module-path '(lib "world-config.ss" "moby" "stub") false))]
    (make-module-binding 'world-config
                         module-path
                         (list (bf 'on-tick module-path 2 false "org.plt.world.config.Kernel.onTick")
                               (bf 'on-mouse module-path 1 false "org.plt.world.config.Kernel.onMouse")
                               (bf 'on-key module-path 1 false "org.plt.world.config.Kernel.onKey")
                               (bf 'on-message module-path 1 false "org.plt.world.config.Kernel.onMessage")
                               
                               (make-binding:function
                                'on-location-change module-path 1 false
                                "org.plt.world.config.Kernel.onLocationChange"
                                (list PERMISSION:LOCATION)
                                false)
                               
                               (make-binding:function
                                'on-tilt module-path 1 false
                                "org.plt.world.config.Kernel.onTilt"
                                (list PERMISSION:TILT)
                                false)
                               
                               (make-binding:function
                                'on-acceleration module-path 1 false
                                "org.plt.world.config.Kernel.onAcceleration"
                                (list PERMISSION:TILT)
                                false)
                               
                               (bf 'on-redraw module-path 1 false "org.plt.world.config.Kernel.onRedraw")
                               (bf 'stop-when module-path 1 false "org.plt.world.config.Kernel.stopWhen")))))





(define (make-world-module module-path)
    (make-module-binding 'world
                         module-path
                         (append (module-binding-bindings world-config-module)
                                 (list (bf 'big-bang module-path 3 true "org.plt.WorldKernel.bigBang")
                                       (bf 'empty-scene module-path 2 false
                                           "org.plt.WorldKernel.emptyScene")
                                       (bf 'place-image module-path 4 false
                                           "org.plt.WorldKernel.placeImage")
                                       (bf 'circle module-path 3 false
                                           "org.plt.WorldKernel.circle")
                                       (bf 'nw:rectangle module-path 4 false
                                           "org.plt.WorldKernel.nwRectangle")
                                       (bf 'rectangle module-path 4 false
                                           "org.plt.WorldKernel.rectangle")
                                       
                                       (bf 'key=? module-path 2 false
                                           "org.plt.WorldKernel.isKeyEqual")
                                       (bf 'text module-path 3 false
                                           "org.plt.WorldKernel.text")
                                       
                                       ;; Fixme: -kernel-create-image is a special case of a function not in the original language.
                                       ;; We can fix this by extending expression to include a special "magic" identifier.  We should
                                       ;; ensure students don't accidently hit this function.
                                       (bf '-kernel-create-image module-path 1 false
                                           "org.plt.WorldKernel._kernelCreateImage")
                                       (bf 'image-width module-path 1 false
                                           "org.plt.WorldKernel.imageWidth")
                                       (bf 'image-height module-path 1 false
                                           "org.plt.WorldKernel.imageHeight")
                                       (bf 'image? module-path 1 false
                                           "org.plt.WorldKernel.isImage")
                                       (bf 'image=? module-path 2 false
                                           "org.plt.WorldKernel.isImageEqual")
                                       (bf 'image-rotate module-path 2 false
                                           "org.plt.WorldKernel.imageRotate")))))
  

;; world teachpack bindings
(define world-module 
  (local [(define module-path
            (build-path (resolve-module-path '(lib "world.ss" "teachpack" "htdp") false)))]
    (make-world-module module-path)))


;; Alternative world teachpack bindings
(define world-stub-module
  (local [(define module-path                       
         (resolve-module-path '(lib "world.ss" "moby" "stub") false))]
    (make-world-module module-path)))


;; Bootstrap bindings
(define bootstrap-module
  (local [(define module-path
            (resolve-module-path '(lib "bootstrap.ss" "moby" "stub") false))]
    (make-module-binding 'world
                         module-path
                         (append (list (bf 'start module-path 10 false "org.plt.world.Bootstrap.start"))
                                 (module-binding-bindings world-stub-module)))))
                         



;; location library
(define location-module 
  (local [(define module-path 
            (resolve-module-path '(lib "location.ss" "moby" "stub") false))

          (define (bf name module-path arity vararity? java-string)
            (make-binding:function name module-path arity vararity? java-string 
                                   (list PERMISSION:LOCATION)
                                   false))]
    (make-module-binding 'location
                         module-path
                         (list (bf 'get-latitude module-path 0 false 
                                                      "org.plt.lib.Location.getLatitude")
                               (bf 'get-longitude module-path 0 false 
                                                      "org.plt.lib.Location.getLongitude")
                               (bf 'get-attitude module-path 0 false 
                                                      "org.plt.lib.Location.getAttitude")
                               (bf 'get-bearing module-path 0 false 
                                                      "org.plt.lib.Location.getBearing")
                               (bf 'get-speed module-path 0 false 
                                                      "org.plt.lib.Location.getSpeed")
                               (bf 'location-distance module-path 4 false
                                                      "org.plt.lib.Location.getDistanceBetween")))))

  
;; accelerometer library
(define tilt-module 
  (local [(define module-path 
            (resolve-module-path '(lib "tilt.ss" "moby" "stub") false))

          (define (bf name module-path arity vararity? java-string)
            (make-binding:function name module-path arity vararity? java-string
                                   (list PERMISSION:TILT)
                                   true))]
    (make-module-binding 'tilt
                         module-path
                         (list (bf 'get-x-acceleration module-path 0 false 
                                                      "org.plt.lib.Tilt.getXAcceleration")
                               (bf 'get-y-acceleration module-path 0 false 
                                                      "org.plt.lib.Tilt.getYAcceleration")
                               (bf 'get-z-acceleration module-path 0 false 
                                                      "org.plt.lib.Location.getZAcceleration")
                               
                               (bf 'get-azimuth module-path 0 false 
                                                      "org.plt.lib.Tilt.getAzimuth")
                               (bf 'get-pitch module-path 0 false 
                                                      "org.plt.lib.Tilt.getPitch")
                               (bf 'get-roll module-path 0 false 
                                                      "org.plt.lib.Tilt.getRoll")))))


(define gui-world-module
  (local [(define module-path 
            (resolve-module-path '(lib "gui-world.ss" "gui-world")
                                 false))]
    (make-module-binding 'gui-world
                         module-path
                         (append (module-binding-bindings world-config-module)
                                 (list (bf 'big-bang module-path 2 true "org.plt.guiworld.GuiWorld.bigBang")
                                       (bf 'row module-path 0 true "org.plt.guiworld.GuiWorld.row")
                                       (bf 'col module-path 0 true "org.plt.guiworld.GuiWorld.col")
                                       (bf 'message module-path 1 false "org.plt.guiworld.GuiWorld.message")
                                       (bf 'button module-path 2 false "org.plt.guiworld.GuiWorld.button")
                                       (bf 'drop-down module-path 3 false "org.plt.guiworld.GuiWorld.dropDown")
                                       (bf 'text-field module-path 2 false "org.plt.guiworld.GuiWorld.textField")
                                       (bf 'box-group module-path 2 false "org.plt.guiworld.GuiWorld.boxGroup")
                                       (bf 'checkbox module-path 3 false "org.plt.guiworld.GuiWorld.checkBox"))))))


(define sms-module
  (local [(define module-path
            (resolve-module-path 
             '(lib "sms.ss" "moby" "stub") false))]
    (make-module-binding 'sms
                         module-path
                         (list (make-binding:function 'send-text-message
                                                      module-path 
                                                      3 
                                                      false 
                                                      "org.plt.lib.Sms.sendTextMessage"
                                                      (list PERMISSION:SMS)
                                                      false)))))


(define net-module
  (local [(define module-path
            (resolve-module-path 
             '(lib "net.ss" "moby" "stub") false))]
    (make-module-binding 'net
                         module-path
                         (list (make-binding:function 'get-url
                                                      module-path 
                                                      1 
                                                      false 
                                                      "org.plt.lib.Net.getUrl"
                                                      (list PERMISSION:INTERNET)
                                                      false)))))

(define parser-module
  (local [(define module-path
         (resolve-module-path 
          '(lib "parser.ss" "moby" "stub") false))]
    (make-module-binding 'parser
                         module-path
                         (list (make-binding:function 'parse-xml
                                                      module-path 
                                                      1 
                                                      false 
                                                      "org.plt.lib.Parser.parseXml"
                                                      empty
                                                      false)
                               (make-binding:function 'split-whitespace
                                                      module-path
                                                      1
                                                      false
                                                      "org.plt.lib.Parser.splitWhitespace"
                                                      empty
                                                      false)))))

;; extend-env/module-binding: env module-binding -> env
;; Extends an environment with the bindings associated to a module.
(define (extend-env/module-binding an-env a-module-binding)
  (local [(define (loop an-env contents)
            (cond
              [(empty? contents)
               an-env]
              [else
               (loop (env-extend an-env (first contents))
                     (rest contents))]))]
    (loop an-env (module-binding-bindings a-module-binding))))

                          
(define known-modules (list world-module
                            world-stub-module
                            location-module
                            tilt-module
                            gui-world-module
                            sms-module
                            net-module
                            parser-module
                            bootstrap-module))
                                        


;; require-analyze: require-path -> pinfo
(define (require-analyze require-path pinfo)
  (local [(define (loop modules)
            (cond
              [(empty? modules)
               (error 'require-analyze 
                      (format "Moby doesn't know about module ~s yet"
                              require-path))]
              [(path=? (resolve-module-path require-path false)
                       (module-binding-path (first modules)))
               (pinfo-accumulate-module 
                (first modules)
                (pinfo-accumulate-bindings
                 (module-binding-bindings (first modules))
                 pinfo))]
              [else
               (loop (rest modules))]))]
    (loop known-modules)))








(provide/contract [struct pinfo ([env env?]
                                 [modules (listof module-binding?)]
                                 [used-bindings hash?])]
                  [empty-pinfo pinfo?]
                  [get-base-pinfo (any/c . -> . pinfo?)]
                  [pinfo-accumulate-binding (binding? pinfo? . -> . pinfo?)]
                  [pinfo-update-env (pinfo? env? . -> . pinfo?)]
                  
                  [program-analyze (program?  . -> . pinfo?)]
                  [program-analyze/pinfo (program? pinfo? . -> . pinfo?)]
                  
                  [struct module-binding ([name symbol?]
                                          [path path?]
                                          [bindings (listof binding?)])])
