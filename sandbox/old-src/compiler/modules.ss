#lang s-exp "lang.ss"

;; Hardcoded modules known by Moby.



;; FIXME: many of these bindings should not be hardcoded here; a module should be self-describing
;; in terms of what bindings it provides.
;; This is a complete mess, and I need to rework this so that a module self-describes its bindings.
;;
;; Part of this is start in the use of the runtime-modules module and the 
;; definition of MOBY-RUNTIME-MODULES.  We just have to continue this work.


(require "env.ss")
(require "helpers.ss")

(require "../collects/moby/runtime/permission-struct.ss")
(require "../collects/moby/runtime/binding.ss")
(require "../collects/moby/runtime/runtime-modules.ss")




(define kernel-misc-module
  (make-module-binding 'moby/runtime/kernel/misc
                       "moby/runtime/kernel/misc"
                       (list (make-binding:function
                              'throw-cond-exhausted-error
                              "moby/runtime/kernel/misc"
                              1
                              false
                              "plt.kernel.misc.throwCondExhaustedError"
                              (list)
                              false)
                             
                             
                             (make-binding:function
                              'verify-boolean-branch-value
                              "moby/runtime/kernel/misc"
                              2
                              false
                              "plt.kernel.misc.verifyBooleanBranchValue"
                              (list)
                              false)
                             
                             (make-binding:function
                              'check-operator-is-function
                              "moby/runtime/kernel/misc"
                              3
                              false
                              "plt.kernel.misc.checkOperatorIsFunction"
                              (list)
                              false)
                             
                             
                             (make-binding:function
                              'print-values
                              "moby/runtime/kernel/misc"
                              0
                              true
                              "plt.kernel.misc.printValues"
                              (list)
                              false))))


(define foreign-module
  (make-module-binding 'moby/foreign
                       "moby/foreign"
                       (list (make-binding:function 
                              'get-js-object
                              "moby/foreign"
                              2
                              false
                              "plt.foreign.get_dash_js_dash_object"
                              (list PERMISSION:FOREIGN-FUNCTION-INTERFACE)
                              false))))




;; FIXME: this should not be here; it should be read off runtime-bindings.ss
;; We need to attach the permissions properly from the scheme source.
(define world-effects-module 
  (local [;; bf: symbol path number boolean string -> binding:function
          ;; Helper function.
          (define (bf name module-path arity vararity? java-string)
            (make-binding:function name module-path arity vararity? java-string empty false))
          
          (define module-path 
            "moby/world-effects")]
    (make-module-binding 'world-effects
                         module-path
                         (list (bf 'make-effect:none module-path 0
                                   false 
                                   "plt.Kernel.invokeModule('moby/runtime/effect-struct').EXPORTS['make-effect:none']")
                               (make-binding:function
				'make-effect:beep module-path 0
                                   false 
                                   "plt.Kernel.invokeModule('moby/runtime/effect-struct').EXPORTS['make-effect:beep']"
				   (list PERMISSION:VIBRATE)
				   false)
                               (bf 'make-effect:play-dtmf-tone module-path 2
                                   false 
                                   "plt.Kernel.invokeModule('moby/runtime/effect-struct').EXPORTS['make-effect:play-dtmf-tone']")
                               (make-binding:function 
                                'make-effect:send-sms module-path 2 false 
                                "plt.Kernel.invokeModule('moby/runtime/effect-struct').EXPORTS['make-effect:send-sms']"
                                (list PERMISSION:SEND-SMS)
                                false)                               
                               (make-binding:function
                                'make-effect:play-sound
                                module-path
                                1
                                false 
                                "plt.Kernel.invokeModule('moby/runtime/effect-struct').EXPORTS['make-effect:play-sound']"
                                (list PERMISSION:INTERNET)
                                false)
                               (bf 'make-effect:stop-sound module-path 1
                                   false 
                                   "plt.Kernel.invokeModule('moby/runtime/effect-struct').EXPORTS['make-effect:stop-sound']")
                               (bf 'make-effect:pause-sound module-path 1
                                   false
                                   "plt.Kernel.invokeModule('moby/runtime/effect-struct').EXPORTS['make-effect:pause-sound']")
                               (bf 'make-effect:set-sound-volume module-path 1
                                   false 
                                   "plt.Kernel.invokeModule('moby/runtime/effect-struct').EXPORTS['make-effect:set-sound-volume']")
                               (bf 'make-effect:set-beep-volume module-path 1
                                   false 
                                   "plt.Kernel.invokeModule('moby/runtime/effect-struct').EXPORTS['make-effect:set-beep-volume']")
                               (bf 'make-effect:raise-sound-volume module-path 0
                                   false
                                   "plt.Kernel.invokeModule('moby/runtime/effect-struct').EXPORTS['make-effect:raise-sound-volume']")
                               (bf 'make-effect:lower-sound-volume module-path 0
                                   false 
                                   "plt.Kernel.invokeModule('moby/runtime/effect-struct').EXPORTS['make-effect:lower-sound-volume']")
                               
                               (make-binding:function 'make-effect:set-wake-lock module-path 1
                                   false 
				   "plt.Kernel.invokeModule('moby/runtime/effect-struct').EXPORTS['make-effect:set-wake-lock']" 
                                   (list PERMISSION:WAKE-LOCK) 
                                   false)
                               (make-binding:function
                                'make-effect:release-wake-lock
                                module-path
                                0
                                false 
				"plt.Kernel.invokeModule('moby/runtime/effect-struct').EXPORTS['make-effect:release-wake-lock']"
                                (list PERMISSION:WAKE-LOCK)
                                false)
                               (bf 'make-effect:pick-playlist module-path 1 false
                                   "plt.Kernel.invokeModule('moby/runtime/effect-struct').EXPORTS['make-effect:pick-playlist']")
                               (bf 'make-effect:pick-random module-path 2 false
                                   "plt.Kernel.invokeModule('moby/runtime/effect-struct').EXPORTS['make-effect:pick-random']")
                               ))))


(define world-handlers-module 
  (local [;; bf: symbol path number boolean string -> binding:function
          ;; Helper function.
          (define (bf name module-path arity vararity? java-string)
            (make-binding:function name module-path arity vararity? java-string empty false))
          (define module-path 
            "moby/world-handlers")]
    (make-module-binding 'world-config
                         module-path
                         (list (bf 'on-tick module-path 2 false "plt.world.config.Kernel.onTick")
                               (bf 'on-tick! module-path 3 false "plt.world.config.Kernel.onTick_star_")
                               (bf 'on-mouse module-path 1 false "plt.world.config.Kernel.onMouse")
                               (bf 'on-mouse! module-path 2 false "plt.world.config.Kernel.onMouse_star_")
                               

			       (bf 'initial-effect module-path 1 false "plt.world.config.Kernel.initialEffect")

                               (bf 'on-key module-path 1 false "plt.world.config.Kernel.onKey")
                               (bf 'on-key! module-path 2 false "plt.world.config.Kernel.onKey_star_")

                               (bf 'on-announce module-path 1 false "plt.world.config.Kernel.onAnnounce")
                               (bf 'on-announce! module-path 2 false "plt.world.config.Kernel.onAnnounce_star_")

                               (make-binding:function
                                'on-location-change module-path 1 false
                                "plt.world.config.Kernel.onLocationChange"
                                (list PERMISSION:LOCATION)
                                false)
                               (make-binding:function
                                'on-location-change! module-path 2 false
                                "plt.world.config.Kernel.onLocationChange_star_"
                                (list PERMISSION:LOCATION)
                                false)
                               
                               (make-binding:function
                                'on-tilt module-path 1 false
                                "plt.world.config.Kernel.onTilt"
                                (list PERMISSION:TILT)
                                false)
                               (make-binding:function
                                'on-tilt! module-path 2 false
                                "plt.world.config.Kernel.onTilt_star_"
                                (list PERMISSION:TILT)
                                false)
                               
                               (make-binding:function
                                'on-acceleration module-path 1 false
                                "plt.world.config.Kernel.onAcceleration"
                                (list PERMISSION:TILT)
                                false)
                               
                               (make-binding:function
                                'on-acceleration! module-path 2 false
                                "plt.world.config.Kernel.onAcceleration_star_"
                                (list PERMISSION:TILT)
                                false)
                               
                               (make-binding:function
                                'on-sms-receive module-path 1 false
                                "plt.world.config.Kernel.onSmsReceive"
                                (list PERMISSION:RECEIVE-SMS)
                                false)

                               (make-binding:function
                                'on-sms-receive* module-path 1 false
                                "plt.world.config.Kernel.onSmsReceive_star"
                                (list PERMISSION:RECEIVE-SMS)
                                false)
                               
                               (make-binding:function
                                'on-shake module-path 1 false
                                "plt.world.config.Kernel.onShake"
                                (list PERMISSION:SHAKE)
                                false)
                               
                               (make-binding:function
                                'on-shake! module-path 2 false
                                "plt.world.config.Kernel.onShake_star_"
                                (list PERMISSION:SHAKE)
                                false)
                               
                               ;; old style
                               (bf 'on-redraw module-path 1 false "plt.world.config.Kernel.onRedraw")
                               
                               ;; Supports both draw and css
                               (bf 'on-draw module-path 2 false "plt.world.config.Kernel.onDraw")
                               
                               (bf 'stop-when module-path 1 false "plt.world.config.Kernel.stopWhen")))))





(define (make-world-module module-path)
  (local [;; bf: symbol path number boolean string -> binding:function
          ;; Helper function.
          (define (bf name module-path arity vararity? java-string)
            (make-binding:function name module-path arity vararity? java-string empty false))]
    (make-module-binding 'world
                       module-path
                       (append (module-binding-bindings world-handlers-module)
                               (module-binding-bindings world-effects-module)
                               (list (bf 'big-bang module-path 3 true "plt.world.Kernel.bigBang")

				     ;; Images
				     (bf 'image? module-path 1 false "plt.world.Kernel.isImage")
				     (bf 'image=? module-path 2 false "plt.world.Kernel.image_equal__question_")

				     (bf 'make-color module-path 3 false "plt.world.Kernel.make_dash_color")

                                     (bf 'empty-scene module-path 2 false
                                         "plt.world.Kernel.emptyScene")
                                     (bf 'place-image module-path 4 false
                                         "plt.world.Kernel.placeImage")
                                     
                                     (bf 'put-pinhole module-path 3 false
                                         "plt.world.Kernel.put_dash_pinhole")

                                     (bf 'circle module-path 3 false
                                         "plt.world.Kernel.circle")
                                     
                                     (bf 'star module-path 5 false
                                         "plt.world.Kernel.star")

                                     (bf 'nw:rectangle module-path 4 false
                                         "plt.world.Kernel.nwRectangle")
                                     (bf 'rectangle module-path 4 false
                                         "plt.world.Kernel.rectangle")
                                     (bf 'triangle module-path 3 false
                                         "plt.world.Kernel.triangle")
                                     (bf 'ellipse module-path 4 false
                                         "plt.world.Kernel.ellipse")
                                     (bf 'line module-path 3 false
                                         "plt.world.Kernel.line")
                                     
				     (bf 'overlay module-path 2 true
					 "plt.world.Kernel.overlay")
				     (bf 'overlay/xy module-path 4 false
					 "plt.world.Kernel.overlay_slash_xy")

                                     (bf 'key=? module-path 2 false
                                         "plt.world.Kernel.isKeyEqual")
                                     (bf 'text module-path 3 false
                                         "plt.world.Kernel.text")
				     (bf 'open-image-url module-path 1 false
                                         "plt.world.Kernel.openImageUrl")
                                     (bf 'image-width module-path 1 false
                                         "plt.world.Kernel.imageWidth")
                                     (bf 'image-height module-path 1 false
                                         "plt.world.Kernel.imageHeight")
                                     (bf 'image-rotate module-path 2 false
                                         "plt.world.Kernel.imageRotate")
                                     
                                     )))))


;; world teachpack bindings
(define world-module 
  (local [(define module-path
            "moby/world")]
    (make-world-module module-path)))


;; Alternative world teachpack bindings
(define world-stub-module
  (local [(define module-path                       
            "moby/world")]
    (make-world-module module-path)))


;; Bootstrap bindings
(define bootstrap-teachpack
  (local [;; bf: symbol path number boolean string -> binding:function
          ;; Helper function.
          (define (bf name module-path arity vararity? java-string)
            (make-binding:function name module-path arity vararity? java-string empty false))
          (define module-path
            "moby/bootstrap-teachpack")
          (define js-module-path
            "plt._MODULES['bootstrap-teachpack.js']")
          ]
    (make-module-binding 'moby/bootstrap-teachpack
                         module-path
                         (append 
                          (list 
                           (bf 'START module-path 14 false (string-append js-module-path ".EXPORTS['START']"))
                           (make-binding:constant 'test-frame 
                                                  "moby/bootstrap-teachpack"
                                                  (string-append js-module-path ".EXPORTS['test-frame']")
                                                  empty)
                           (bf 'sq module-path 1 false (string-append js-module-path ".EXPORTS['sq']"))
                           (bf 'sine module-path 1 false (string-append js-module-path ".EXPORTS['sine']"))
                           (bf 'cosine module-path 1 false (string-append js-module-path ".EXPORTS['cosine']"))
                           (bf 'tangent module-path 1 false (string-append js-module-path ".EXPORTS['tangent']")))
                           (module-binding-bindings world-stub-module)))))

;; Cage teachpack
(define cage-teachpack
  (local [;; bf: symbol path number boolean string -> binding:function
          ;; Helper function.
          (define (bf name module-path arity vararity? java-string)
            (make-binding:function name module-path arity vararity? java-string empty false))
          (define module-path
            "moby/cage-teachpack")
          (define js-module-path
            "plt._MODULES['cage-teachpack.js']")
          ]
    (make-module-binding 'moby/cage-teachpack
                         module-path
                         (append 
                          (list 
                           (bf 'start module-path 1 false (string-append js-module-path ".EXPORTS['start']")))))))

;; Function teachpack
(define function-teachpack
  (local [;; bf: symbol path number boolean string -> binding:function
          ;; Helper function.
          (define (bf name module-path arity vararity? java-string)
            (make-binding:function name module-path arity vararity? java-string empty false))
          (define module-path
            "moby/function-teachpack")
          (define js-module-path
            "plt._MODULES['function-teachpack.js']")
          ]
    (make-module-binding 'moby/function-teachpack
                         module-path
                         (append 
                          (list 
                           (bf 'start module-path 1 false (string-append js-module-path ".EXPORTS['start']")))))))





;; location library
(define location-module 
  (local [(define module-path
            "moby/geolocation")
          
          (define (bf name module-path arity vararity? java-string)
            (make-binding:function name module-path arity vararity? java-string 
                                   (list PERMISSION:LOCATION)
                                   false))]
    (make-module-binding 'location
                         module-path
                         (list (bf 'get-latitude module-path 0 false 
                                   "plt.lib.Location.getLatitude")
                               (bf 'get-longitude module-path 0 false 
                                   "plt.lib.Location.getLongitude")
                               (bf 'get-altitude module-path 0 false 
                                   "plt.lib.Location.getAltitude")
                               (bf 'get-bearing module-path 0 false 
                                   "plt.lib.Location.getBearing")
                               (bf 'get-speed module-path 0 false 
                                   "plt.lib.Location.getSpeed")
                               (bf 'location-distance module-path 4 false
                                   "plt.lib.Location.getDistanceBetween")))))


;; accelerometer library
(define tilt-module 
  (local [(define module-path 
            "moby/tilt")
          
          (define (bf name arity vararity? java-string)
            (make-binding:function name module-path arity vararity? java-string
                                   (list PERMISSION:TILT)
                                   true))]
    (make-module-binding 'tilt
                         module-path
                         (list (bf 'get-x-acceleration 0 false 
                                   "plt.lib.Tilt.getXAcceleration")
                               (bf 'get-y-acceleration 0 false 
                                   "plt.lib.Tilt.getYAcceleration")
                               (bf 'get-z-acceleration 0 false 
                                   "plt.lib.Location.getZAcceleration")
                               
                               (bf 'get-azimuth 0 false 
                                   "plt.lib.Tilt.getAzimuth")
                               (bf 'get-pitch 0 false 
                                   "plt.lib.Tilt.getPitch")
                               (bf 'get-roll 0 false 
                                   "plt.lib.Tilt.getRoll")))))





(define telephony-module
  (local [(define module-path
            "moby/telephony")]

    (make-module-binding 'telephony
                         module-path
                         (list (make-binding:function 'get-signal-strengths
                                                      module-path 
                                                      0 
                                                      false 
                                                      "plt.lib.Telephony.getSignalStrengths"
                                                      (list PERMISSION:TELEPHONY)
                                                      false)))))





(define net-module
  (local [(define module-path
            "moby/net")]

    (make-module-binding 'net
                         module-path
                         (list (make-binding:function 'get-url
                                                      module-path 
                                                      1 
                                                      false 
                                                      "plt.lib.Net.getUrl"
                                                      (list PERMISSION:INTERNET)
                                                      false)))))

(define parser-module
  (local [(define module-path
            "moby/parser")]

    (make-module-binding 'parser
                         module-path
                         (list (make-binding:function 'xml->s-exp
                                                      module-path 
                                                      1 
                                                      false 
                                                      "plt.lib.Parser.parseXml"
                                                      empty
                                                      false)
                               #;(make-binding:function 'split-whitespace
	                                                      module-path
        	                                              1
                	                                      false
                        	                              "plt.lib.Parser.splitWhitespace"
                                	                      empty
                                        	              false)))))

(define jsworld-module
  (local [(define module-path
            "moby/jsworld")

          (define (bf name arity java-string)
            (make-binding:function name module-path arity true java-string empty false))]
    (make-module-binding 'jsworld
                         module-path
                         (list (make-binding:function 
                                'js-big-bang
                                module-path
                                1
                                true
                                "plt.world.MobyJsworld.bigBang"
                                empty
                                false)
                               (make-binding:function 'js-text 
                                                      module-path 
                                                      1 
                                                      false 
                                                      "plt.world.MobyJsworld.text"
                                                      empty
                                                      false)
                               
                               
                               ;; Each of these functions can take an optional
                               ;; (sexpof css-style) argument.

                               (bf 'js-div 0 "plt.world.MobyJsworld.div")
                               (bf 'js-p 0 "plt.world.MobyJsworld.p")
                               (bf 'js-button 1 "plt.world.MobyJsworld.button")
                               (bf 'js-button! 2 "plt.world.MobyJsworld.buttonStar")
                               (bf 'js-input 2 "plt.world.MobyJsworld.input")
			       (make-binding:function 'js-img module-path 1 true 
						      "plt.world.MobyJsworld.img"
						      (list PERMISSION:INTERNET) 
						      false)
                               (bf 'js-node 1 "plt.world.MobyJsworld.rawNode")
			       (bf 'js-select 2 "plt.world.MobyJsworld.select")))))
  








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





(define MOBY-RUNTIME-MODULES
  (map (lambda (sexp)
         (local [(define name (list-ref sexp 0))
                 (define path (list-ref sexp 1))
                 (define bindings (map sexp->binding (list-ref sexp 2)))]
           (make-module-binding name
                                path
                                bindings)))
       MOBY-RUNTIME-MODULE-BINDINGS))



;; The default bindings for moby will include
;; stuff from regular world
;; stuff from jsworld
(define moby-module-binding
  (make-module-binding 'moby
                       "moby/moby"
                       (append 
                        (module-binding-bindings world-stub-module)
                        (module-binding-bindings jsworld-module)
                        (module-binding-bindings telephony-module)
                        (module-binding-bindings location-module)
			(module-binding-bindings net-module))))













;; These modules are hardcoded.
(define known-modules (list* world-module
                             world-stub-module
                             location-module
                             tilt-module
                             net-module
                             parser-module
                             bootstrap-teachpack
                             function-teachpack
                             cage-teachpack
                             telephony-module
                             moby-module-binding
                             
                             foreign-module
                             kernel-misc-module
                             MOBY-RUNTIME-MODULES))


;; default-module-resolver: symbol -> (module-binding | false)
;; Provides a default module resolver.
(define (default-module-resolver a-name)
  (local [(define (loop modules)
            (cond
              [(empty? modules)
               false]
              [(symbol=? (module-binding-name (first modules))
                         a-name)
               (first modules)]
              [else
               (loop (rest modules))]))]
    (loop known-modules)))



;; default-module-path-resolver: module-path module-path -> module-name
(define (default-module-path-resolver a-module-path parent-module-path)
  (local [(define (loop modules)
            (cond
              [(empty? modules)
               (cond
                 [(symbol? a-module-path)
                  a-module-path]
                 [(string? a-module-path)
                  (string->symbol 
                   (module-path-join parent-module-path a-module-path))])]
              [(module-path=? (module-path-join parent-module-path a-module-path)
                              (module-binding-source (first modules)))
               (module-binding-name (first modules))]
              [else
               (loop (rest modules))]))]
    (loop known-modules)))



;; module-path-join: module-path module-path -> module-path
(define (module-path-join p1 p2)
  (local [(define (looks-like-relative-path? p)
            (and (string? p)
                 (> (string-length p) 0)
                 (not (char=? (string-ref p 0) #\/))))
          
          ;; take all but the last element of the list.
          (define (butlast l)
            (reverse (rest (reverse l))))
          
          (define (path-only p)
            (string-join (butlast (path-split p))
                         "/"))

          (define (path-split p)
            (string-split p #\/))
          
          (define (path-simplify p)
            (local [(define (loop current-chunks pieces)
                      (cond
                        [(empty? pieces)
                         (string-join (reverse current-chunks) "/")]
                        [(string=? (first pieces) "..")
                         (cond
                           [(empty? current-chunks)
                            (loop (cons (first pieces) current-chunks)
                                  (rest pieces))]
                           [else
                            (loop (rest current-chunks) (rest pieces))])]
                        [else
                         (loop (cons (first pieces) current-chunks)
                               (rest pieces))]))]
              (loop empty (path-split p))))]
  (cond
    [(symbol? p2)
     p2]
    [(string? p2)
     (cond
       [(and (string? p1)
             (looks-like-relative-path? p2))
        (if (string=? (path-only p1) "")
            p2
            (path-simplify
             (string-append (path-only p1)
                            "/"
                            p2)))]
       [else
        (path-simplify p2)])])))





(provide/contract [extend-env/module-binding 
                   (env? module-binding? . -> . env?)]
                  [moby-module-binding module-binding?]
                  [default-module-resolver (module-name? . -> . (or/c module-binding? false/c))]
                  [default-module-path-resolver (module-path? module-path? . -> . (or/c module-name? false/c))]
                  [module-path-join (module-path? module-path? . -> . module-path?)])