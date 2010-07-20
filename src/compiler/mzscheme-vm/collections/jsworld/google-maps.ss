(provide on-map-drag!
         on-map-click!
         on-map-dblclick!
         on-map-rightclick!
         google-map
         make-effect:map:zoom
         make-effect:map:location
         make-effect:map:marker
         make-effect:map:pan
         make-effect:map:clear)


(define document (get-named-js-object "document"))
(define createElement (get-js-object document "createElement"))
(define getElementsByTagName (get-js-object document "getElementsByTagName"))
(define jsworld (get-named-js-object "jsworld"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;GOOGLE-MAPS STUFF
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (list->js-array l)
  (scheme->prim-js (list->vector l)))

(define (js-array->list arr)
  (vector->list (prim-js->scheme arr)))

(define (loadScript url)
  (local [(define elem (js-call createElement document (scheme->prim-js "script")))
          (define tag (get-js-object (js-call getElementsByTagName document "head") "0"))]
    (begin
      (js-set-field! elem "src" url)
      (js-set-field! elem "type" "text/javascript")
      (js-call (get-js-object tag "appendChild") tag elem)
      (void))))

(define (loadGoogleMaps lat lng zoom dom)
  (begin 
    (loadScript "http://www.google.com/jsapi?key=ABQIAAAANuLQS-qn8FP_vP2FRBltExQIToEyJbLNev2L3JVYkG889ZSczxQlHFTiT5TMwWkBl4392LfkmDmJ4A")
    (local [(define phase2 (lambda ()
                             (local [(define google (get-named-js-object "google"))]
                               (if (equal? google js-undefined)
                                   (begin
                                     (js-call (get-named-js-object "setTimeout") 
                                              false 
                                              (procedure->void-js-fun phase2) 
                                              (scheme->prim-js 1000)))
                                   (local [(define load (get-js-object google "load"))
                                           (define callback (make-js-hash))]
                                     (begin 
                                       (js-set-field! callback "callback" (procedure->void-js-fun 
                                                                           (lambda ()
                                                                             (local [(define maps (get-js-object google "maps"))
                                                                                     (define mymap (js-new (get-js-object maps "Map2") dom))]
                                                                               (begin 
                                                                                 (js-call (get-js-object mymap "setCenter")
                                                                                          mymap
                                                                                          (js-new (get-js-object maps "LatLng") 
                                                                                                  (scheme->prim-js lat) 
                                                                                                  (scheme->prim-js lng))
                                                                                          (scheme->prim-js zoom))
                                                                                 (js-call (get-js-object mymap "setUIToDefault") mymap)
                                                                                 (js-set-field! dom "gmap" mymap))))))
                                       (js-call load google (scheme->prim-js "maps") (scheme->prim-js "2.x") callback)))))))]
      
      
      (phase2))))

;gmap: number number number list-of (css-sexp) -> dom-sexp
;gmap acts as the constructor for creating a Google Map
;     It returns a dom-sexp (i.e., a div element holding the map object)
;     
(define (google-map lat lng zoom attribs)
  (local [(define dom (js-div attribs))]
    (begin
      (js-set-field! dom "jsworldOpaque" (scheme->prim-js true))
      (loadGoogleMaps lat lng zoom dom)
      dom)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;GMAP BIG-BANG EVENT HANDLERS
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (on-map-drag! map-dom world-updater effect-updater)
  (local [(define map (get-js-object map-dom "gmap"))]
    (make-world-config
     (lambda (handler)
       (js-call (get-named-js-object "setTimeout")
                false
                (procedure->void-js-fun (lambda ()
                                          (js-call
                                           (get-js-object (get-named-js-object "GEvent") "addListener")
                                           false
                                           (get-js-object map-dom "gmap")
                                           "drag"
                                           handler)))
                (scheme->prim-js 4000))) ;;CHANGE THIS. Should be a timeout
     (lambda (id) (void))
     (lambda (w)
       (local [(define mymap (get-js-object map-dom "gmap"))
               (define get-center (get-js-object mymap "getCenter"))
               (define center (js-call get-center mymap))
               (define lat (js-call (get-js-object center "lat") center))
               (define lng (js-call (get-js-object center "lng") center))]
         (world-with-effects (effect-updater w (prim-js->scheme lat) (prim-js->scheme lng))
                             (world-updater w (prim-js->scheme lat) (prim-js->scheme lng))))))))

(define (on-map-click! map-dom world-updater effect-updater)
  (local [(define mymap (get-js-object map-dom "gmap"))]
    (make-world-config
     (lambda (handler)
       (js-call (get-named-js-object "setTimeout")
                false
                (procedure->void-js-fun (lambda ()
                                          (js-call
                                           (get-js-object (get-named-js-object "GEvent") "addListener")
                                           false
                                           (get-js-object map-dom "gmap")
                                           "click"
                                           handler)))
                (scheme->prim-js 4000))) ;;CHANGE THIS. Should be a timeout
     (lambda (id) (void))
     (lambda (w overlay lat/lng) ;BIG PROBLEM. Use case-lambda (or variable arity). Could be problem for other event types as well
       (local [(define lng (get-js-object lat/lng "x"))
               (define lat (get-js-object lat/lng "y"))]
         (world-with-effects (effect-updater w (prim-js->scheme lat) (prim-js->scheme lng))
                             (world-updater w (prim-js->scheme lat) (prim-js->scheme lng))))))))

(define (on-map-dblclick! map-dom world-updater effect-updater)
  (local [(define mymap (get-js-object map-dom "gmap"))]
    (make-world-config
     (lambda (handler)
       (js-call (get-named-js-object "setTimeout")
                false
                (procedure->void-js-fun (lambda ()
                                          (js-call
                                           (get-js-object (get-named-js-object "GEvent") "addListener")
                                           false
                                           (get-js-object map-dom "gmap")
                                           "dblclick"
                                           handler)))
                (scheme->prim-js 4000))) ;;CHANGE THIS. Should be a timeout
     (lambda (id) (void))
     (lambda (w overlay lat/lng) ;BIG PROBLEM. Use case-lambda (or variable arity). Could be problem for other event types as well
       (local [(define lng (get-js-object lat/lng "x"))
               (define lat (get-js-object lat/lng "y"))]
         (world-with-effects (effect-updater w (prim-js->scheme lat) (prim-js->scheme lng))
                             (world-updater w (prim-js->scheme lat) (prim-js->scheme lng))))))))

(define (on-map-rightclick! map-dom world-updater effect-updater) ;fix: instead of giving the x, y pos on the map, give back the lat/lng
  (local [(define mymap (get-js-object map-dom "gmap"))]
    (make-world-config
     (lambda (handler)
       (js-call (get-named-js-object "setTimeout")
                false
                (procedure->void-js-fun (lambda ()
                                          (js-call
                                           (get-js-object (get-named-js-object "GEvent") "addListener")
                                           false
                                           (get-js-object map-dom "gmap")
                                           "singlerightclick"
                                           handler)))
                (scheme->prim-js 4000))) ;;CHANGE THIS. Should be a timeout
     (lambda (id) (void))
     (lambda (w dom x/y im)
       (local [(define lng (get-js-object x/y "x"))
               (define lat (get-js-object x/y "y"))]
         (world-with-effects (effect-updater w (prim-js->scheme lat) (prim-js->scheme lng))
                             (world-updater w (prim-js->scheme lat) (prim-js->scheme lng))))))))





;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;GMAP EFFECTS - DEFINE-VALUES, EFFECT TYPES
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-values (effect:map make-effect:map effect:map? effect:map-accessor effect:map-mutator)
  (make-effect-type 'effect:map 
                    #f 
                    1 
                    (lambda (map-dom) (error 'effect:map "has no default implementation"))
                    (lambda (map-dom name)
                      (if (not
                           (equal? (get-js-object map-dom "gmap") js-undefined))
                          map-dom
                          (error name "expected type <map (dom-sexp)> as 1st argument, given: ~s" map-dom)))))

(define-values (effect:map:zoom make-effect:map:zoom effect:map:zoom? effect:map:zoom-accessor effect:map:zoom-mutator)
  (make-effect-type 'effect:map:zoom
                    effect:map
                    1
                    (lambda (map-dom zoom-val) 
                      (local [(define mymap (get-js-object map-dom "gmap"))]
                        (js-call (get-js-object mymap "setZoom") mymap zoom-val)))
                    (lambda (map zoom-val name)
                      (if (and (exact? zoom-val)
                               (integer? zoom-val)
                               (positive? zoom-val))
                          (values map zoom-val)
                          (error name "expected type <exact positive integer> as 2nd argument, given: ~s" zoom-val)))))

(define-values (effect:map:location make-effect:map:location effect:map:location? effect:map:location-accessor effect:map:location-mutator)
  (make-effect-type 'effect:map:location
                    effect:map
                    2
                    (lambda (map-dom lat lng) 
                      (local [(define mymap (get-js-object map-dom "gmap"))]
                        (js-call (get-js-object mymap "setCenter") mymap (js-new
                                                                          (get-named-js-object "GLatLng")
                                                                          lat
                                                                          lng))))
                    (lambda (map-dom lat lng name)
                      (if (and 
                           (number? lat)
                           (number? lng))
                          (values map-dom lat lng)
                          (error name "expected type <number> as 2nd and 3rd argument, given: ~s ~s" lat lng)))))

(define-values (effect:map:pan make-effect:map:pan effect:map:pan? effect:map:pan-accessor effect:map:pan-mutator)
  (make-effect-type 'effect:map:pan
                    effect:map
                    2
                    (lambda (map-dom lat lng)
                      (local [(define mymap (get-js-object map-dom "gmap"))]
                        (js-call (get-js-object mymap "panTo") mymap (js-new
                                                                      (get-named-js-object "GLatLng")
                                                                      lat
                                                                      
                                                                      lng))))
                    (lambda (map-dom lat lng name)
                      (if (and 
                           (number? lat)
                           (number? lng))
                          (values map-dom lat lng)
                          (error name "expected type <number> as 2nd and 3rd argument, given: ~s ~s" lat lng)))))

(define-values (effect:map:clear make-effect:map:clear effect:map:clear? effect:map:clear-accessor effect:map:clear-mutator)
  (make-effect-type 'effect:map:clear
                    effect:map
                    0
                    (lambda (map-dom)
                      (local [(define mymap (get-js-object map-dom "gmap"))]
                        (js-call (get-js-object mymap "clearOverlays") mymap )))))


;make variable arity so no text has to be specified!!
(define-values (effect:map:marker make-effect:map:marker effect:map:marker? effect:map:marker-accessor effect:map:marker-mutator)
  (make-effect-type 'effect:map:marker
                    effect:map
                    3
                    (lambda (map-dom lat lng marker-text)
                      (local [(define mymap (get-js-object map-dom "gmap"))
                              (define marker (js-new 
                                              (get-named-js-object "GMarker") 
                                              (js-new (get-named-js-object "GLatLng") 
                                                      (scheme->prim-js lat) 
                                                      (scheme->prim-js lng))))]
                        (begin
                          (js-call
                           (get-js-object (get-named-js-object "GEvent") "addListener")
                           false
                           marker
                           "click"
                           (procedure->void-js-fun (lambda (lat/lng)
                                                     (js-call (get-js-object marker "openInfoWindowHtml") 
                                                              marker 
                                                              (scheme->prim-js marker-text)))))
                           (js-call (get-js-object mymap "addOverlay") mymap marker (scheme->prim-js false)))))
                    
                    (lambda (map-dom lat lng text name)
                      (if (and 
                           (number? lat)
                           (number? lng)
                           (string? text))
                          (values map-dom lat lng text)
                          (error name "expected type <number> as 2nd and 3rd argument and <string> as 4th argument, given: ~s ~s ~s" lat lng text)))))
                        
                        
                    