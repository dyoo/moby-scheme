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
                    (make-binding:function sel-id #f 1 #f 
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

;; world teachpack bindings
(define world-module
  (let ([module-path
         (build-path (resolve-module-path '(lib "world.ss" "teachpack" "htdp") #f))])
    (make-module-binding 'world
                         module-path
                         (list (make-binding:function 'big-bang module-path 4 #f
                                                      "org.plt.WorldKernel.big_dash_bang")
                               (make-binding:function 'on-tick-event module-path 1 #f
                                                      "org.plt.WorldKernel.on_dash_tick_dash_event")
                               (make-binding:function 'on-mouse-event module-path 1 #f
                                                      "org.plt.WorldKernel.on_dash_mouse_dash_event")
                               (make-binding:function 'on-key-event module-path 1 #f "org.plt.WorldKernel.on_dash_key_dash_event")
                               (make-binding:function 'on-message-event module-path 1 #f
                                                      "org.plt.WorldKernel.on_dash_message_dash_event")
                               (make-binding:function 'on-location-change-event module-path 1 #f
                                                      "org.plt.WorldKernel.on_dash_location_dash_change_dash_event")
                               (make-binding:function 'on-orientation-change-event module-path 1 #f
                                                      "org.plt.WorldKernel.on_dash_orientation_dash_change_dash_event")
                               (make-binding:function 'on-acceleration-change-event module-path 1 #f
                                                      "org.plt.WorldKernel.on_dash_acceleration_dash_change_dash_event")
                               (make-binding:function 'on-redraw module-path 1 #f
                                                      "org.plt.WorldKernel.on_dash_redraw")
                               (make-binding:function 'stop-when module-path 1 #f
                                                      "org.plt.WorldKernel.stop_dash_when")
                               
                               (make-binding:function 'empty-scene module-path 2 #f
                                                      "org.plt.WorldKernel.empty_dash_scene")
                               (make-binding:function 'place-image module-path 4 #f
                                                      "org.plt.WorldKernel.place_dash_image")
                               (make-binding:function 'circle module-path 3 #f
                                                      "org.plt.WorldKernel.circle")
                               (make-binding:function 'nw:rectangle module-path 4 #f
                                                      "org.plt.WorldKernel.nw_colon_rectangle")
                                                              (make-binding:function 'rectangle module-path 4 #f
                                                      "org.plt.WorldKernel.rectangle")
                               
                               (make-binding:function 'key=? module-path 2 #f
                                                      "org.plt.WorldKernel.key_equal__question_")
                               (make-binding:function 'text module-path 3 #f
                                                      "org.plt.WorldKernel.text")
                               
                               ;; Fixme: -kernel-create-image is a special case of a function not in the original language.
                               ;; We can fix this by extending expression to include a special "magic" identifier.  We should
                               ;; ensure students don't accidently hit this function.
                               (make-binding:function '-kernel-create-image module-path 1 #f
                                                      "org.plt.WorldKernel._dash_kernel_dash_create_dash_image")
                               (make-binding:function 'image-width module-path 1 #f
                                                      "org.plt.WorldKernel.image_dash_width")
                               (make-binding:function 'image-height module-path 1 #f
                                                      "org.plt.WorldKernel.image_dash_height")
                               (make-binding:function 'image? module-path 1 #f
                                                      "org.plt.WorldKernel.image_question_")
                               (make-binding:function 'image=? module-path 2 #f
                                                      "org.plt.WorldKernel.image_equal__question_")
                               (make-binding:function 'image-rotate module-path 2 #f
                                                      "org.plt.WorldKernel.image_dash_rotate")))))


(define world-stub-module
  (let ([module-path                       
         (resolve-module-path '(lib "world.ss" "moby" "stub") #f)])
  (make-module-binding 'world-stub
                       module-path
                       (list (make-binding:function 'big-bang module-path 4 #f
                                                    "org.plt.WorldKernel.big_dash_bang")
                             (make-binding:function 'on-tick-event module-path 1 #f
                                                    "org.plt.WorldKernel.on_dash_tick_dash_event")
                             (make-binding:function 'on-mouse-event module-path 1 #f
                                                    "org.plt.WorldKernel.on_dash_mouse_dash_event")
                             (make-binding:function 'on-key-event module-path 1 #f "org.plt.WorldKernel.on_dash_key_dash_event")

                             (make-binding:function 'on-message-event module-path 1 #f
                                                    "org.plt.WorldKernel.on_dash_message_dash_event")
                             (make-binding:function 'on-location-change-event module-path 1 #f
                                                    "org.plt.WorldKernel.on_dash_location_dash_change_dash_event")
                             (make-binding:function 'on-orientation-change-event module-path 1 #f
                                                    "org.plt.WorldKernel.on_dash_orientation_dash_change_dash_event")
                             (make-binding:function 'on-acceleration-change-event module-path 1 #f
                                                    "org.plt.WorldKernel.on_dash_acceleration_dash_change_dash_event")
                             (make-binding:function 'on-redraw module-path 1 #f
                                                    "org.plt.WorldKernel.on_dash_redraw")
                             (make-binding:function 'stop-when module-path 1 #f
                                                    "org.plt.WorldKernel.stop_dash_when")
                                                          
                             
                             (make-binding:function 'empty-scene module-path 2 #f
                                                    "org.plt.WorldKernel.empty_dash_scene")
                             (make-binding:function 'place-image module-path 4 #f
                                                    "org.plt.WorldKernel.place_dash_image")
                             (make-binding:function 'circle module-path 3 #f
                                                    "org.plt.WorldKernel.circle")
                             (make-binding:function 'nw:rectangle module-path 4 #f
                                                    "org.plt.WorldKernel.nw_colon_rectangle")
                                        
                             (make-binding:function 'rectangle module-path 4 #f
                                                    "org.plt.WorldKernel.rectangle")
                             (make-binding:function 'key=? module-path 2 #f
                                                    "org.plt.WorldKernel.key_equal__question_")
                             (make-binding:function 'text module-path 3 #f
                                                    "org.plt.WorldKernel.text")
                             
                             ;; Fixme: -kernel-create-image is a special case of a function not in the original language.
                             ;; We can fix this by extending expression to include a special "magic" identifier.  We should
                             ;; ensure students don't accidently hit this function.
                             (make-binding:function '-kernel-create-image module-path 1 #f
                                                    "org.plt.WorldKernel._dash_kernel_dash_create_dash_image")
                             (make-binding:function 'image-width module-path 1 #f
                                                    "org.plt.WorldKernel.image_dash_width")
                             (make-binding:function 'image-height module-path 1 #f
                                                    "org.plt.WorldKernel.image_dash_height")
                             (make-binding:function 'image? module-path 1 #f
                                                    "org.plt.WorldKernel.image_question_")
                             (make-binding:function 'image=? module-path 2 #f
                                                    "org.plt.WorldKernel.image_equal__question_")
                             (make-binding:function 'image-rotate module-path 2 #f
                                                    "org.plt.WorldKernel.image_dash_rotate")))))



;; location library
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

  
;; accelerometer library
(define tilt-module 
  (let ([module-path 
         (build-path mock-lib-path "tilt.ss")])
    (make-module-binding 'tilt
                         module-path
                         (list (make-binding:function 'get-x-acceleration module-path 0 #f 
                                                      "org.plt.lib.Tilt.getXAcceleration")
                               (make-binding:function 'get-y-acceleration module-path 0 #f 
                                                      "org.plt.lib.Tilt.getYAcceleration")
                               (make-binding:function 'get-z-acceleration module-path 0 #f 
                                                      "org.plt.lib.Location.getZAcceleration")
                               
                               (make-binding:function 'get-azimuth module-path 0 #f 
                                                      "org.plt.lib.Tilt.getAzimuth")
                               (make-binding:function 'get-pitch module-path 0 #f 
                                                      "org.plt.lib.Tilt.getPitch")
                               (make-binding:function 'get-roll module-path 0 #f 
                                                      "org.plt.lib.Tilt.getRoll")))))




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
                            tilt-module))
                                        


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
       (pinfo-accumulate-bindings (module-binding-bindings (first modules))
                                  pinfo)]
      [else
       (loop (rest modules))])))








(provide/contract [struct pinfo ([env env?])]
                  [program-analyze ((program?) (pinfo?) . ->* . pinfo?)]
                  
                  [struct module-binding ([name symbol?]
                                          [path path?]
                                          [bindings (listof binding?)])]
                  [extend-known-modules! (module-binding? . -> . any)])