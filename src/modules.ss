#lang s-exp "lang.ss"

;; Hardcoded modules known by Moby.
(require "env.ss")
(require "permission.ss")

(define-struct module-binding (name path bindings))




(define world-effects-module 
  (local [;; bf: symbol path number boolean string -> binding:function
          ;; Helper function.
          (define (bf name module-path arity vararity? java-string)
            (make-binding:function name module-path arity vararity? java-string empty false))
          
          (define module-path 
            (resolve-module-path '(lib "world-effects.ss" "moby" "stub" "private") false))]
    (make-module-binding 'world-effects
                         module-path
                         (list (bf 'make-effect:none module-path 0
                                   false "org.plt.world.Kernel.make_dash_effect_colon_none")
                               (bf 'make-effect:beep module-path 0
                                   false "org.plt.world.Kernel.make_dash_effect_colon_beep")
                               (bf 'make-effect:play-sound-url module-path 1
                                   false "org.plt.world.Kernel.make_dash_effect_colon_play_dash_sound_dash_url")
                               (bf 'make-effect:stop-sound-url module-path 1
                                   false "org.plt.world.Kernel.make_dash_effect_colon_stop_dash_sound_dash_url")
                               (bf 'make-effect:pause-sound-url module-path 1
                                   false "org.plt.world.Kernel.make_dash_effect_colon_pause_dash_sound_dash_url")
                               (bf 'make-effect:set-sound-volume module-path 1
                                   false "org.plt.world.Kernel.make_dash_effect_colon_set_dash_sound_dash_volume")
                               (bf 'make-effect:raise-sound-volume module-path 0
                                   false "org.plt.world.Kernel.make_dash_effect_colon_raise_dash_sound_dash_volume")
                               (bf 'make-effect:lower-sound-volume module-path 0
                                   false "org.plt.world.Kernel.make_dash_effect_colon_lower_dash_sound_dash_volume")
                               (bf 'make-effect:play-dtmf-tone module-path 2
                                   false "org.plt.world.Kernel.make_dash_effect_colon_play_dash_dtmf_dash_tone")
                               (bf 'make-effect:set-wake-lock 1
                                   false "org.plt.world.Kernel.make_dash_effect_colon_set_dash_wake_dash_lock")
                               (bf 'make-effect:release-wake-lock 0
                                   false "org.plt.world.Kernel.make_dash_effect_colon_release_dash_wake_dash_lock")
                               (make-binding:function 'make-effect:send-sms module-path 2 false 
                                                      "org.plt.world.Kernel.make_dash_effect_colon_sms"
                                                      (list PERMISSION:SEND-SMS)
                                                      false)))))


(define world-handlers-module 
  (local [;; bf: symbol path number boolean string -> binding:function
          ;; Helper function.
          (define (bf name module-path arity vararity? java-string)
            (make-binding:function name module-path arity vararity? java-string empty false))
          (define module-path 
            (resolve-module-path '(lib "world-handlers.ss" "moby" "stub" "private") false))]
    (make-module-binding 'world-config
                         module-path
                         (list (bf 'on-tick module-path 2 false "org.plt.world.config.Kernel.onTick")
                               (bf 'on-tick* module-path 3 false "org.plt.world.config.Kernel.onTick_star_")
                               (bf 'on-mouse module-path 1 false "org.plt.world.config.Kernel.onMouse")
                               (bf 'on-mouse* module-path 2 false "org.plt.world.config.Kernel.onMouse_star_")
                               
                               (bf 'on-key module-path 1 false "org.plt.world.config.Kernel.onKey")
                               (bf 'on-key* module-path 2 false "org.plt.world.config.Kernel.onKey_star_")
                               
                               (make-binding:function
                                'on-location-change module-path 1 false
                                "org.plt.world.config.Kernel.onLocationChange"
                                (list PERMISSION:LOCATION)
                                false)
                               (make-binding:function
                                'on-location-change* module-path 2 false
                                "org.plt.world.config.Kernel.onLocationChange_star_"
                                (list PERMISSION:LOCATION)
                                false)
                               
                               (make-binding:function
                                'on-tilt module-path 1 false
                                "org.plt.world.config.Kernel.onTilt"
                                (list PERMISSION:TILT)
                                false)
                               (make-binding:function
                                'on-tilt* module-path 2 false
                                "org.plt.world.config.Kernel.onTilt_star_"
                                (list PERMISSION:TILT)
                                false)
                               
                               (make-binding:function
                                'on-acceleration module-path 1 false
                                "org.plt.world.config.Kernel.onAcceleration"
                                (list PERMISSION:TILT)
                                false)
                               
                               (make-binding:function
                                'on-acceleration* module-path 2 false
                                "org.plt.world.config.Kernel.onAcceleration_star_"
                                (list PERMISSION:TILT)
                                false)
                               
                               (make-binding:function
                                'on-shake module-path 1 false
                                "org.plt.world.config.Kernel.onShake"
                                (list PERMISSION:TILT)
                                false)
                               
                               (make-binding:function
                                'on-shake* module-path 2 false
                                "org.plt.world.config.Kernel.onShake_star_"
                                (list PERMISSION:TILT)
                                false)
                               
                               ;; old style
                               (bf 'on-redraw module-path 1 false "org.plt.world.config.Kernel.onRedraw")
                               
                               ;; Supports both draw and css
                               (bf 'on-draw module-path 2 false "org.plt.world.config.Kernel.onDraw")
                               
                               (bf 'stop-when module-path 1 false "org.plt.world.config.Kernel.stopWhen")))))





(define (make-world-module module-path)
  (local [;; bf: symbol path number boolean string -> binding:function
          ;; Helper function.
          (define (bf name module-path arity vararity? java-string)
            (make-binding:function name module-path arity vararity? java-string empty false))]
    (make-module-binding 'world
                       module-path
                       (append (module-binding-bindings world-handlers-module)
                               (module-binding-bindings world-effects-module)
                               (list (bf 'big-bang module-path 3 true "org.plt.world.Kernel.bigBang")
                                     (bf 'empty-scene module-path 2 false
                                         "org.plt.world.Kernel.emptyScene")
                                     (bf 'place-image module-path 4 false
                                         "org.plt.world.Kernel.placeImage")
                                     (bf 'circle module-path 3 false
                                         "org.plt.world.Kernel.circle")
                                     (bf 'nw:rectangle module-path 4 false
                                         "org.plt.world.Kernel.nwRectangle")
                                     (bf 'rectangle module-path 4 false
                                         "org.plt.world.Kernel.rectangle")
                                     
                                     (bf 'key=? module-path 2 false
                                         "org.plt.world.Kernel.isKeyEqual")
                                     (bf 'text module-path 3 false
                                         "org.plt.world.Kernel.text")
                                     
                                     ;; Fixme: -kernel-create-image is a special case of a function not in the original language.
                                     ;; We can fix this by extending expression to include a special "magic" identifier.  We should
                                     ;; ensure students don't accidently hit this function.
                                     (bf '-kernel-create-image module-path 1 false
                                         "org.plt.world.Kernel._kernelCreateImage")
                                     (bf 'image-width module-path 1 false
                                         "org.plt.world.Kernel.imageWidth")
                                     (bf 'image-height module-path 1 false
                                         "org.plt.world.Kernel.imageHeight")
                                     (bf 'image? module-path 1 false
                                         "org.plt.world.Kernel.isImage")
                                     (bf 'image=? module-path 2 false
                                         "org.plt.world.Kernel.isImageEqual")
                                     (bf 'image-rotate module-path 2 false
                                         "org.plt.world.Kernel.imageRotate"))))))


;; world teachpack bindings
(define world-module 
  (local [(define module-path
            (resolve-module-path '(lib "world.ss" "teachpack" "htdp") false))]
    (make-world-module module-path)))


;; Alternative world teachpack bindings
(define world-stub-module
  (local [(define module-path                       
            (resolve-module-path '(lib "world.ss" "moby" "stub") false))]
    (make-world-module module-path)))


;; Bootstrap bindings
(define bootstrap-module
  (local [;; bf: symbol path number boolean string -> binding:function
          ;; Helper function.
          (define (bf name module-path arity vararity? java-string)
            (make-binding:function name module-path arity vararity? java-string empty false))
          (define module-path
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
          
          (define (bf name arity vararity? java-string)
            (make-binding:function name module-path arity vararity? java-string
                                   (list PERMISSION:TILT)
                                   true))]
    (make-module-binding 'tilt
                         module-path
                         (list (bf 'get-x-acceleration 0 false 
                                   "org.plt.lib.Tilt.getXAcceleration")
                               (bf 'get-y-acceleration 0 false 
                                   "org.plt.lib.Tilt.getYAcceleration")
                               (bf 'get-z-acceleration 0 false 
                                   "org.plt.lib.Location.getZAcceleration")
                               
                               (bf 'get-azimuth 0 false 
                                   "org.plt.lib.Tilt.getAzimuth")
                               (bf 'get-pitch 0 false 
                                   "org.plt.lib.Tilt.getPitch")
                               (bf 'get-roll 0 false 
                                   "org.plt.lib.Tilt.getRoll")))))






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
                                                      (list PERMISSION:SEND-SMS)
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

(define jsworld-module 
  (local [(define module-path
            (resolve-module-path 
             '(lib "jsworld.ss" "moby" "stub") false))
          (define (bf name arity java-string)
            (make-binding:function name module-path arity false java-string empty false))]
    (make-module-binding 'jsworld
                         module-path
                         (list (make-binding:function 
                                'js-big-bang
                                module-path
                                2
                                true
                                "org.plt.world.MobyJsworld.bigBang"
                                empty
                                false)
                               (bf 'js-div 0 "org.plt.world.MobyJsworld.div")
                               (bf 'js-button 1 "org.plt.world.MobyJsworld.button")
                               (bf 'js-input 1 "org.plt.world.MobyJsworld.input")
                               (bf 'js-text 1 "org.plt.world.MobyJsworld.text")))))
  


;; The default bindings for moby will include
;; stuff from regular world
;; stuff from jsworld
(define moby-module-binding
  (make-module-binding 'moby
                       (resolve-module-path '(lib "moby.ss" "moby" "stub") false)
                       (append 
                        (module-binding-bindings world-stub-module)
                        (module-binding-bindings jsworld-module))))


                               
                               

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
                            sms-module
                            net-module
                            parser-module
                            bootstrap-module
                            
                            moby-module-binding))



(provide/contract [struct module-binding ([name symbol?]
                                          [path path?]
                                          [bindings (listof binding?)])]
                  [extend-env/module-binding 
                   (env? module-binding? . -> . env?)]
                  [known-modules (listof module-binding?)]
                  [moby-module-binding module-binding?])