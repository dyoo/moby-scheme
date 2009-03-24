#lang scheme

(require "env.ss"
         "toplevel.ss"
         "helpers.ss"
         "permission.ss"
         syntax/modresolve
         scheme/contract
         scheme/runtime-path)




;; pinfo (program-info) captures the information we get from analyzing 
;; the program.
(define-struct pinfo (env modules used-bindings) #:transparent)

(define (get-base-pinfo)
  (make-pinfo (get-toplevel-env) empty (make-immutable-hash empty)))


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
                        #t)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;; program-analyze: program [program-info] -> program-info
;; Collects which identifiers are defined by the program, and which identifiers
;; are actively used.
(define (program-analyze a-program [pinfo (get-base-pinfo)])
  (let* ([pinfo
          (program-analyze-collect-definitions a-program pinfo)]
         [pinfo
          (program-analyze-uses a-program pinfo)])
    
    pinfo))




;; program-analyze-collect-definitions: program pinfo -> pinfo
;; Collects the definitions either imported or defined by this program.
(define (program-analyze-collect-definitions a-program pinfo)
  (cond [(empty? a-program)
         pinfo]
        [else
         (let ([updated-pinfo
                (cond [(defn? (first a-program))
                       (definition-analyze-collect-definitions (first a-program) pinfo)]
                      [(test-case? (first a-program))
                       pinfo]
                      [(library-require? (first a-program))
                       (require-analyze (second (first a-program)) pinfo)]
                      [(expression? (first a-program))
                       pinfo])])
           (program-analyze-collect-definitions (rest a-program)
                                                updated-pinfo))]))



;; program-analyze-uses: program pinfo -> pinfo
(define (program-analyze-uses a-program pinfo)
  (cond [(empty? a-program)
         pinfo]
        [else
         (let ([updated-pinfo
                (cond [(defn? (first a-program))
                       (definition-analyze-uses (first a-program) pinfo)]
                      [(test-case? (first a-program))
                       pinfo]
                      [(library-require? (first a-program))
                       pinfo]
                      [(expression? (first a-program))
                       (expression-analyze-uses (first a-program)
                                                pinfo 
                                                (pinfo-env pinfo))])])
           (program-analyze-uses (rest a-program)
                                 updated-pinfo))]))


(define (bf name module-path arity vararity? java-string)
  (make-binding:function name module-path arity vararity? java-string empty))


;; definition-analyze-collect-definitions: definition program-info -> program-info
;; Collects the defined names introduced by the definition.
(define (definition-analyze-collect-definitions a-definition pinfo)
  (match a-definition
    [(list 'define (list id args ...) body)
     (pinfo-accumulate-binding (bf id
                                                      #f
                                                      (length args) 
                                                      #f 
                                                      (symbol->string
                                                       (identifier->munged-java-identifier id)))
                               pinfo)]
    [(list 'define (? symbol? id) (list 'lambda (list args ...) body))
     (pinfo-accumulate-binding (bf id
                                                      #f
                                                      (length args) 
                                                      #f 
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
     (let* ([constructor-id 
             (string->symbol (format "make-~a" id))]
            [constructor-binding 
             (bf constructor-id #f (length fields) #f
                                    (symbol->string
                                     (identifier->munged-java-identifier constructor-id)))]
            [predicate-id
             (string->symbol (format "~a?" id))]
            [predicate-binding
             (bf predicate-id #f 1 #f
                                    (symbol->string
                                     (identifier->munged-java-identifier predicate-id)))]
            
            
            [selector-ids
             (map (lambda (f)
                    (string->symbol (format "~a-~a" id f)))
                  fields)]
            [selector-bindings
             (map (lambda (sel-id) 
                    (bf sel-id #f 1 #f 
                                           (symbol->string
                                            (identifier->munged-java-identifier sel-id))))
                  selector-ids)])
       (foldl pinfo-accumulate-binding pinfo 
              (list* constructor-binding predicate-binding selector-bindings)))]))



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


(define (function-definition-analyze-uses fun args body pinfo)
  (let* ([env (pinfo-env pinfo)]
         [env 
          (env-extend env (bf fun #f (length args) #f
                                                 (symbol->string fun)))]
         [env
          (foldl (lambda (arg-id env) 
                   (env-extend env (make-binding:constant arg-id 
                                                          (symbol->string
                                                           arg-id)
                                                          empty)))
                 env
                 args)])
    (expression-analyze-uses body pinfo env)))




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define (expression-analyze-uses an-expression pinfo env)
  (match an-expression
    
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
       [(env-lookup env an-expression)
        =>
        (lambda (binding)
          (pinfo-accumulate-binding-use binding pinfo))]
       [else
        pinfo])]
    
    ;; Quoted symbols
    [(list 'quote datum)
     pinfo]
    
    ;; Function call/primitive operation call
    [(list (? symbol? id) exprs ...)
     (let ([updated-pinfo
            (foldl (lambda (e p)
                     (expression-analyze-uses e p env))
                   pinfo
                   exprs)])
       (cond
         [(env-lookup env id)
          =>
          (lambda (binding)
            (pinfo-accumulate-binding-use binding updated-pinfo))]
         [else
          updated-pinfo]))]))
   
    
    
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



(define-struct module-binding (name path bindings))




(define (make-world-module module-path)
    (make-module-binding 'world
                         module-path
                         (list (bf 'big-bang module-path 4 #f "org.plt.WorldKernel.big_dash_bang")
                               (bf 'on-tick module-path 1 #f
                                                      "org.plt.WorldKernel.on_dash_tick_dash_event")
                               (bf 'on-mouse module-path 1 #f
                                                      "org.plt.WorldKernel.on_dash_mouse_dash_event")
                               (bf 'on-key module-path 1 #f "org.plt.WorldKernel.on_dash_key_dash_event")
                               (bf 'on-message module-path 1 #f
                                                      "org.plt.WorldKernel.on_dash_message_dash_event")
                               
                               (make-binding:function
                                'on-location-change module-path 1 #f
                                "org.plt.WorldKernel.on_dash_location_dash_change_dash_event"
                                (list PERMISSION:LOCATION))
                               
                               (make-binding:function
                                'on-tilt module-path 1 #f
                                "org.plt.WorldKernel.on_dash_orientation_dash_change_dash_event"
                                (list PERMISSION:TILT))
                               
                               (make-binding:function
                                'on-acceleration module-path 1 #f
                                "org.plt.WorldKernel.on_dash_acceleration_dash_change_dash_event"
                                (list PERMISSION:TILT))
                               
                               (bf 'on-redraw module-path 1 #f
                                                      "org.plt.WorldKernel.on_dash_redraw")
                               (bf 'stop-when module-path 1 #f
                                                      "org.plt.WorldKernel.stop_dash_when")
                               
                               (bf 'empty-scene module-path 2 #f
                                                      "org.plt.WorldKernel.empty_dash_scene")
                               (bf 'place-image module-path 4 #f
                                                      "org.plt.WorldKernel.place_dash_image")
                               (bf 'circle module-path 3 #f
                                                      "org.plt.WorldKernel.circle")
                               (bf 'nw:rectangle module-path 4 #f
                                                      "org.plt.WorldKernel.nw_colon_rectangle")
                                                              (bf 'rectangle module-path 4 #f
                                                      "org.plt.WorldKernel.rectangle")
                               
                               (bf 'key=? module-path 2 #f
                                                      "org.plt.WorldKernel.key_equal__question_")
                               (bf 'text module-path 3 #f
                                                      "org.plt.WorldKernel.text")
                               
                               ;; Fixme: -kernel-create-image is a special case of a function not in the original language.
                               ;; We can fix this by extending expression to include a special "magic" identifier.  We should
                               ;; ensure students don't accidently hit this function.
                               (bf '-kernel-create-image module-path 1 #f
                                                      "org.plt.WorldKernel._dash_kernel_dash_create_dash_image")
                               (bf 'image-width module-path 1 #f
                                                      "org.plt.WorldKernel.image_dash_width")
                               (bf 'image-height module-path 1 #f
                                                      "org.plt.WorldKernel.image_dash_height")
                               (bf 'image? module-path 1 #f
                                                      "org.plt.WorldKernel.image_question_")
                               (bf 'image=? module-path 2 #f
                                                      "org.plt.WorldKernel.image_equal__question_")
                               (bf 'image-rotate module-path 2 #f
                                                      "org.plt.WorldKernel.image_dash_rotate"))))


;; world teachpack bindings
(define world-module 
  (let ([module-path
         (build-path (resolve-module-path '(lib "world.ss" "teachpack" "htdp") #f))])
    (make-world-module module-path)))

;; Alternative world teachpack bindings
(define world-stub-module
  (let ([module-path                       
         (resolve-module-path '(lib "world.ss" "moby" "stub") #f)])
    (make-world-module module-path)))


;; location library
(define location-module 
  (let* ([module-path 
          (resolve-module-path '(lib "location.ss" "moby" "stub") #f)]
         [bf (lambda (name module-path arity vararity? java-string)
               (make-binding:function name module-path arity vararity? java-string 
                                      (list PERMISSION:LOCATION)))])
    (make-module-binding 'location
                         module-path
                         (list (bf 'get-latitude module-path 0 #f 
                                                      "org.plt.lib.Location.getLatitude")
                               (bf 'get-longitude module-path 0 #f 
                                                      "org.plt.lib.Location.getLongitude")
                               (bf 'get-attitude module-path 0 #f 
                                                      "org.plt.lib.Location.getAttitude")
                               (bf 'get-bearing module-path 0 #f 
                                                      "org.plt.lib.Location.getBearing")
                               (bf 'get-speed module-path 0 #f 
                                                      "org.plt.lib.Location.getSpeed")
                               (bf 'location-distance module-path 4 #f
                                                      "org.plt.lib.Location.getDistanceBetween")))))

  
;; accelerometer library
(define tilt-module 
  (let* ([module-path 
          (resolve-module-path '(lib "tilt.ss" "moby" "stub") #f)]
         [bf (lambda (name module-path arity vararity? java-string)
               (make-binding:function name module-path arity vararity? java-string
                                      (list PERMISSION:TILT)))])
    (make-module-binding 'tilt
                         module-path
                         (list (bf 'get-x-acceleration module-path 0 #f 
                                                      "org.plt.lib.Tilt.getXAcceleration")
                               (bf 'get-y-acceleration module-path 0 #f 
                                                      "org.plt.lib.Tilt.getYAcceleration")
                               (bf 'get-z-acceleration module-path 0 #f 
                                                      "org.plt.lib.Location.getZAcceleration")
                               
                               (bf 'get-azimuth module-path 0 #f 
                                                      "org.plt.lib.Tilt.getAzimuth")
                               (bf 'get-pitch module-path 0 #f 
                                                      "org.plt.lib.Tilt.getPitch")
                               (bf 'get-roll module-path 0 #f 
                                                      "org.plt.lib.Tilt.getRoll")))))


(define gui-world-module
  (let ([module-path 
         (resolve-module-path '(lib "gui-world.ss" "gui-world")
                              #f)])
    (make-module-binding 'gui-world
                         module-path
                         (list (bf 'big-bang module-path 2 #f "org.plt.guiworld.GuiWorld.bigBang")
                               (bf 'row module-path 0 #t "org.plt.guiworld.GuiWorld.row")
                               (bf 'col module-path 0 #t "org.plt.guiworld.GuiWorld.col")
                               (bf 'message module-path 1 #f "org.plt.guiworld.GuiWorld.message")
                               (bf 'button module-path 2 #f "org.plt.guiworld.GuiWorld.button")
                               (bf 'drop-down module-path 3 #f "org.plt.guiworld.GuiWorld.dropDown")
                               (bf 'text-field module-path 2 #f "org.plt.guiworld.GuiWorld.textField")
                               (bf 'box-group module-path 2 #f "org.plt.guiworld.GuiWorld.boxGroup")
                               (bf 'checkbox module-path 3 #f "org.plt.guiworld.GuiWorld.checkBox")
                               ))))


(define sms-module
  (let ([module-path
         (resolve-module-path 
          '(lib "sms.ss" "moby" "stub") #f)])
    (make-module-binding 'sms
                         module-path
                         (list (make-binding:function 'send-text-message
                                                      module-path 
                                                      3 
                                                      #f 
                                                      "org.plt.lib.Sms.sendTextMessage"
                                                      (list PERMISSION:SMS))))))


(define net-module
  (let ([module-path
         (resolve-module-path 
          '(lib "net.ss" "moby" "stub") #f)])
    (make-module-binding 'net
                         module-path
                         (list (make-binding:function 'get-url
                                                      module-path 
                                                      1 
                                                      #f 
                                                      "org.plt.lib.Net.getUrl"
                                                      (list PERMISSION:INTERNET))))))

(define parser-module
  (let ([module-path
         (resolve-module-path 
          '(lib "parser.ss" "moby" "stub") #f)])
    (make-module-binding 'parser
                         module-path
                         (list (make-binding:function 'parse-xml
                                                      module-path 
                                                      1 
                                                      #f 
                                                      "org.plt.lib.Parser.parseXml"
                                                      (list))
                               (make-binding:function 'split-whitespace
                                                      module-path
                                                      1
                                                      #f
                                                      "org.plt.lib.Parser.splitWhitespace"
                                                      (list))))))

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

                          
(define known-modules (list world-module
                            world-stub-module
                            location-module
                            tilt-module
                            gui-world-module
                            sms-module
                            net-module
                            parser-module))
                                        


;; extend-known-modules!: module-binding -> void
;; Extends to the list of known modules.
(define (extend-known-modules! a-module-binding)
  (set! known-modules (cons a-module-binding known-modules)))



;; require-analyze: require-path -> pinfo
(define (require-analyze require-path pinfo)
  (let loop ([modules known-modules])
    (cond
      [(empty? modules)
       (error 'require-analyze "Moby doesn't know about module ~s yet"
              require-path)]
      [(path=? (resolve-module-path require-path #f)
               (module-binding-path (first modules)))
       (pinfo-accumulate-module 
        (first modules)
        (pinfo-accumulate-bindings (module-binding-bindings (first modules))
                                   pinfo))]
      [else
       (loop (rest modules))])))








(provide/contract [struct pinfo ([env env?]
                                 [modules (listof module-binding?)]
                                 [used-bindings hash?])]
                  [program-analyze ((program?) (pinfo?) . ->* . pinfo?)]
                  
                  [struct module-binding ([name symbol?]
                                          [path path?]
                                          [bindings (listof binding?)])]
                  [extend-known-modules! (module-binding? . -> . any)])