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
;(require "../collects/moby/runtime/runtime-modules.ss")




(define kernel-misc-module
  (make-module-binding 'moby/runtime/kernel/misc
                       "moby/runtime/kernel/misc"
                       (list (make-binding:function
                              'throw-cond-exhausted-error
                              "moby/runtime/kernel/misc"
                              1
                              false
                              (list)
                              false)
                             
                             
                             (make-binding:function
                              'verify-boolean-branch-value
                              "moby/runtime/kernel/misc"
                              2
                              false
                              (list)
                              false)
                             
                             (make-binding:function
                              'check-operator-is-function
                              "moby/runtime/kernel/misc"
                              3
                              false
                              (list)
                              false)
                             
                             
                             (make-binding:function
                              'print-values
                              "moby/runtime/kernel/misc"
                              0
                              true
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
                              (list PERMISSION:FOREIGN-FUNCTION-INTERFACE)
                              false))))




;; FIXME: this should not be here; it should be read off runtime-bindings.ss
;; We need to attach the permissions properly from the scheme source.
(define world-effects-module 
  (local [;; bf: symbol path number boolean string -> binding:function
          ;; Helper function.
          (define (bf name module-path arity vararity?)
            (make-binding:function name module-path arity vararity? empty false))
          
          (define module-path 
            "moby/world-effects")]
    (make-module-binding 'world-effects
                         module-path
                         (list (bf 'make-effect:none module-path 0
                                   false) 
                               (make-binding:function
                                'make-effect:beep module-path 0
                                false 
                                (list PERMISSION:VIBRATE)
                                false)
                               (bf 'make-effect:play-dtmf-tone module-path 2
                                   false)
                               
                               (make-binding:function 
                                'make-effect:send-sms module-path 2 false 
                                (list PERMISSION:SEND-SMS)
                                false)                               
                               (make-binding:function
                                'make-effect:play-sound
                                module-path
                                1
                                false 
                                (list PERMISSION:INTERNET)
                                false)
                               (bf 'make-effect:stop-sound module-path 1
                                   false)
                               (bf 'make-effect:pause-sound module-path 1
                                   false)
                               (bf 'make-effect:set-sound-volume module-path 1
                                   false)
                               (bf 'make-effect:set-beep-volume module-path 1
                                   false)
                               (bf 'make-effect:raise-sound-volume module-path 0
                                   false)
                               (bf 'make-effect:lower-sound-volume module-path 0
                                   false)
                               
                               (make-binding:function 'make-effect:set-wake-lock module-path 1
                                                      false 
                                                      (list PERMISSION:WAKE-LOCK) 
                                                      false)
                               (make-binding:function
                                'make-effect:release-wake-lock
                                module-path
                                0
                                false   
                                (list PERMISSION:WAKE-LOCK)
                                false)
                               (bf 'make-effect:pick-playlist module-path 1 false)
                               (bf 'make-effect:pick-random module-path 2 false)
                               ))))


(define world-handlers-module 
  (local [;; bf: symbol path number boolean string -> binding:function
          ;; Helper function.
          (define (bf name module-path arity vararity?)
            (make-binding:function name module-path arity vararity? empty false))
          (define module-path 
            "moby/world-handlers")]
    (make-module-binding 'world-config
                         module-path
                         (list (bf 'on-tick module-path 1 true)
                               (bf 'on-tick! module-path 2 true)
                               #;(bf 'on-mouse module-path 1 false)
                               #;(bf 'on-mouse! module-path 2 false)
                               
                               
                               (bf 'initial-effect module-path 1 false)
                               
                               (bf 'on-key module-path 1 false)
                               (bf 'on-key! module-path 2 false)
                               
                               #;(bf 'on-announce module-path 1 false)
                               #;(bf 'on-announce! module-path 2 false)
                               
                               #;(make-binding:function
                                'on-location-change module-path 1 false
                                (list PERMISSION:LOCATION)
                                false)
                               #;(make-binding:function
                                'on-location-change! module-path 2 false
                                (list PERMISSION:LOCATION)
                                false)
                               
                               #;(make-binding:function
                                'on-tilt module-path 1 false
                                (list PERMISSION:TILT)
                                false)
                               #;(make-binding:function
                                'on-tilt! module-path 2 false
                                (list PERMISSION:TILT)
                                false)
                               
                               #;(make-binding:function
                                'on-acceleration module-path 1 false
                                (list PERMISSION:TILT)
                                false)
                               
                               #;(make-binding:function
                                'on-acceleration! module-path 2 false
                                (list PERMISSION:TILT)
                                false)
                               
                               #;(make-binding:function
                                'on-sms-receive module-path 1 false
                                (list PERMISSION:RECEIVE-SMS)
                                false)
                               
                               #;(make-binding:function
                                'on-sms-receive* module-path 1 false
                                (list PERMISSION:RECEIVE-SMS)
                                false)
                               
                               #;(make-binding:function
                                'on-shake module-path 1 false
                                (list PERMISSION:SHAKE)
                                false)
                               
                               #;(make-binding:function
                                'on-shake! module-path 2 false
                                (list PERMISSION:SHAKE)
                                false)
                               
                               ;; old style
                               (bf 'on-redraw module-path 1 false)
                               ;; new-style
                               (bf 'to-draw module-path 1 false)
                               
                               ;; Supports both draw and css
                               (bf 'on-draw module-path 2 false)
                               
                               (bf 'stop-when module-path 1 false)))))





(define (make-world-module module-path)
  (local [;; bf: symbol path number boolean string -> binding:function
          ;; Helper function.
          (define (bf name)
            (make-binding:constant name module-path 
                                   (if (member name '(video-url
                                                      bitmap/url
                                                      image-url
                                                      open-image-url))
                                       (list PERMISSION:INTERNET)
                                       empty)))]
   (make-module-binding 'world
                         module-path
                         (append (module-binding-bindings world-handlers-module)
                                 (module-binding-bindings world-effects-module)
                                 (map bf '(key=?
					   
					   big-bang
                                       
					   make-color
					   color-red
					   color-green
					   color-blue
					   empty-scene
					   scene+line
					   place-image
					   place-image/align
					   put-pinhole
					   circle
					   star
					   radial-star
					   star-polygon
					   nw:rectangle
					   rectangle
					   regular-polygon
					   rhombus
					   square
					   triangle
					   right-triangle
					   isosceles-triangle
					   ellipse
					   line
					   add-line
					   overlay
					   overlay/xy
					   overlay/align
					   underlay
					   underlay/xy
					   underlay/align
					   beside
					   beside/align
					   above
					   above/align
					   rotate
					   scale
					   scale/xy
					   crop
					   frame
					   flip-horizontal
					   flip-vertical
					   text
					   text/font
					   video-url  ;; needs network
					   bitmap/url ;; needs network
					   image-url  ;; needs network
					   open-image-url  ;; needs network
					   image?
					   image=?
					   image-width
					   image-height
					   
					   image->color-list
					   color-list->image
					   
					   image-baseline
					   mode?
					   image-color?
					   x-place?
					   y-place?
					   angle?
					   side-count?
					   step-count?
					   ))))))


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
          (define (bf name module-path arity vararity?)
            (make-binding:function name module-path arity vararity? empty false))
          (define module-path
            "bootstrap/bootstrap-teachpack")
          ]
    (make-module-binding 'bootstrap/bootstrap-teachpack
                         module-path
                         (list 
                          (bf 'START module-path 14 false)
                          (make-binding:constant 'test-frame module-path empty)
                          (bf 'sq module-path 1 false)
                          (bf 'sine module-path 1 false)
                          (bf 'cosine module-path 1 false)
                          (bf 'tangent module-path 1 false)))))

;; Cage teachpack
(define cage-teachpack
  (local [;; bf: symbol path number boolean string -> binding:function
          ;; Helper function.
          (define (bf name module-path arity vararity?)
            (make-binding:function name module-path arity vararity? empty false))
          (define module-path
            "bootstrap/cage-teachpack")]
    (make-module-binding 'bootstrap/cage-teachpack
                         module-path
                         (list 
                          (bf 'start module-path 1 false)))))

;; Function teachpack
(define function-teachpack
  (local [;; bf: symbol path number boolean string -> binding:function
          ;; Helper function.
          (define (bf name module-path arity vararity?)
            (make-binding:function name module-path arity vararity? empty false))
          (define module-path
            "bootstrap/function-teachpack")
          ]
    (make-module-binding 'bootstrap/function-teachpack
                         module-path
                         (list 
                          (bf 'start module-path 1 false)))))





;; location library
(define location-module 
  (local [(define module-path
            "moby/geolocation")
          
          (define (bf name module-path arity vararity?)
            (make-binding:function name module-path arity vararity?
                                   (list PERMISSION:LOCATION)
                                   false))]
    (make-module-binding 'location
                         module-path
                         (list (bf 'get-latitude module-path 0 false)
                               (bf 'get-longitude module-path 0 false)
                               (bf 'get-altitude module-path 0 false)
                               (bf 'get-bearing module-path 0 false)
                               (bf 'get-speed module-path 0 false)
                               (bf 'location-distance module-path 4 false)))))


;; accelerometer library
(define tilt-module 
  (local [(define module-path 
            "moby/tilt")
          
          (define (bf name arity vararity?)
            (make-binding:function name module-path arity vararity?
                                   (list PERMISSION:TILT)
                                   true))]
    (make-module-binding 'tilt
                         module-path
                         (list (bf 'get-x-acceleration 0 false)
                               (bf 'get-y-acceleration 0 false)
                               (bf 'get-z-acceleration 0 false)
                               
                               (bf 'get-azimuth 0 false)
                               (bf 'get-pitch 0 false)
                               (bf 'get-roll 0 false)))))





(define telephony-module
  (local [(define module-path
            "moby/telephony")]
    
    (make-module-binding 'telephony
                         module-path
                         (list (make-binding:function 'get-signal-strengths
                                                      module-path 
                                                      0 
                                                      false 
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
                                                      empty
                                                      false)))))

(define jsworld-module
  (local [(define module-path
            "moby/jsworld")
          
          (define (bf name arity)
            (make-binding:function name module-path arity true empty false))]
    (make-module-binding 'jsworld
                         module-path
                         (list (make-binding:function 
                                'js-big-bang
                                module-path
                                1
                                true
                                empty
                                false)
			       (make-binding:function 
                                'big-bang
                                module-path
                                1
                                true
                                empty
                                false)
                               
                               (make-binding:function 'js-text 
                                                      module-path 
                                                      1 
                                                      false 
                                                      empty
                                                      false)
                               
                               
                               ;; Each of these functions can take an optional
                               ;; (sexpof css-style) argument.
                               
                               (bf 'js-div 0)
                               (bf 'js-p 0)
                               (bf 'js-button 1)
                               (bf 'js-button! 2)
                               (bf 'js-input 2)

			       #;(bf 'empty-page 0)
			       #;(make-binding:function 'place-on-page
						      module-path
						      4
						      false
						      (list)
						      false)


                               (make-binding:function 'js-img module-path 1 true 
                                                      (list PERMISSION:INTERNET) 
                                                      false)
                               (bf 'js-node 1)
                               (bf 'js-select 2)))))









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





#;(define MOBY-RUNTIME-MODULES
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
                        #;(module-binding-bindings telephony-module)
                        #;(module-binding-bindings location-module)
                        #;(module-binding-bindings net-module))))













;; These modules are hardcoded.
(define known-modules (list world-module
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
                            #;MOBY-RUNTIME-MODULES
                            ))


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
  #;(printf "looking for ~s, among ~s~n"
          a-module-path
          (map (lambda (a-module) (module-binding-name a-module)) known-modules))
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