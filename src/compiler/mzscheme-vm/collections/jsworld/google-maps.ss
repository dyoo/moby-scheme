
(provide on-map-drag!
         on-map-click!
         on-map-dblclick!
         on-map-rightclick!
         google-map
         make-effect:map:zoom
         make-effect:map:location
         make-effect:map:marker
         make-effect:map:pan
         make-effect:map:clear
         make-effect:script
         make-effect:reverse-geocode)



(define document (js-get-named-object "document"))
(define createElement (js-get-field document "createElement"))
(define getElementsByTagName (js-get-field document "getElementsByTagName"))
(define jsworld (js-get-named-object "jsworld"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;GOOGLE-MAPS LIBRARY
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (list->js-array l)
  (scheme->prim-js (list->vector l)))

(define (js-array->list arr)
  (vector->list (prim-js->scheme arr)))

(define (loadScript url)
  (local [(define elem (js-call createElement document (scheme->prim-js "script")))
          (define tag (js-get-field (js-call getElementsByTagName document "head") "0"))]
    (begin
      (js-set-field! elem "src" url)
      (js-set-field! elem "type" "text/javascript")
      (js-call (js-get-field tag "appendChild") tag elem)
      (void))))

(define (loadGoogleMaps lat lng zoom dom)
  (begin 
    (loadScript "http://www.google.com/jsapi?key=ABQIAAAANuLQS-qn8FP_vP2FRBltExQIToEyJbLNev2L3JVYkG889ZSczxQlHFTiT5TMwWkBl4392LfkmDmJ4A")
    (local [(define phase2 (lambda ()
                             (local [(define google (js-get-named-object "google"))]
                               (if (equal? google js-undefined)
                                   (begin
                                     (js-call (js-get-named-object "setTimeout") 
                                              false 
                                              (procedure->void-js-fun phase2) 
                                              (scheme->prim-js 500)))
                                   (local [(define load (js-get-field google "load"))
                                           (define callback (js-make-hash))]
                                     (begin 
                                       (js-set-field! callback "callback" (procedure->void-js-fun 
                                                                           (lambda ()
                                                                             (local [(define maps (js-get-field google "maps"))
                                                                                     (define mymap (js-new (js-get-field maps "Map2") dom))]
                                                                               (begin 
                                                                                 (js-call (js-get-field mymap "setCenter")
                                                                                          mymap
                                                                                          (js-new (js-get-field maps "LatLng") 
                                                                                                  (scheme->prim-js lat) 
                                                                                                  (scheme->prim-js lng))
                                                                                          (scheme->prim-js zoom))
                                                                                 (js-call (js-get-field mymap "setUIToDefault") mymap)
                                                                                 (js-call (js-get-field mymap "disableDoubleClickZoom") mymap) ;GET RID OF THIS LATER
                                                                                 (js-set-field! dom "gmap" mymap))))))
                                       (js-call load google (scheme->prim-js "maps") (scheme->prim-js "2.x") callback)))))))]
      
      
      (phase2))))

(define (GLatLng lat lng)
  (js-new (js-get-named-object "GLatLng") (scheme->prim-js lat) (scheme->prim-js lng)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;GOOGLE MAP CONSTRUCTOR
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

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

(require-permission google-map "PERMISSION:INTERNET")


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;GMAP BIG-BANG EVENT HANDLERS
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(define (on-map-drag! map-dom world-updater effect-updater)
  (local [(define map (js-get-field map-dom "gmap"))]
    (make-world-config
     (lambda (handler)
       (js-call (js-get-named-object "setTimeout")
                false
                (procedure->void-js-fun (lambda ()
                                          (js-call
                                           (js-get-field (js-get-named-object "GEvent") "addListener")
                                           false
                                           (js-get-field map-dom "gmap")
                                           "drag"
                                           handler)))
                (scheme->prim-js 4000))) ;;CHANGE THIS. Should be a timeout
     (lambda (id) (void))
     (lambda (w)
       (local [(define mymap (js-get-field map-dom "gmap"))
               (define get-center (js-get-field mymap "getCenter"))
               (define center (js-call get-center mymap))
               (define lat (js-call (js-get-field center "lat") center))
               (define lng (js-call (js-get-field center "lng") center))]
         (world-with-effects (effect-updater w (prim-js->scheme lat) (prim-js->scheme lng))
                             (world-updater w (prim-js->scheme lat) (prim-js->scheme lng))))))))

(define (on-map-click! map-dom world-updater effect-updater)
  (local [(define mymap (js-get-field map-dom "gmap"))]
    (make-world-config
     (lambda (handler)
       (js-call (js-get-named-object "setTimeout")
                false
                (procedure->void-js-fun (lambda ()
                                          (js-call
                                           (js-get-field (js-get-named-object "GEvent") "addListener")
                                           false
                                           (js-get-field map-dom "gmap")
                                           "click"
                                           handler)))
                (scheme->prim-js 4000))) ;;CHANGE THIS. Should be a timeout
     (lambda (id) (void))
     (lambda (w overlay lat/lng) ;BIG PROBLEM. Use case-lambda (or variable arity). Could be problem for other event types as well
       (local [(define lng (js-get-field lat/lng "x"))
               (define lat (js-get-field lat/lng "y"))]
         (world-with-effects (effect-updater w (prim-js->scheme lat) (prim-js->scheme lng))
                             (world-updater w (prim-js->scheme lat) (prim-js->scheme lng))))))))

(define (on-map-dblclick! map-dom world-updater effect-updater)
  (local [(define mymap (js-get-field map-dom "gmap"))]
    (make-world-config
     (lambda (handler)
       (js-call (js-get-named-object "setTimeout")
                false
                (procedure->void-js-fun (lambda ()
                                          (js-call
                                           (js-get-field (js-get-named-object "GEvent") "addListener")
                                           false
                                           (js-get-field map-dom "gmap")
                                           "dblclick"
                                           handler)))
                (scheme->prim-js 4000))) ;;CHANGE THIS. Should be a timeout
     (lambda (id) (void))
     (lambda (w overlay lat/lng) ;BIG PROBLEM. Use case-lambda (or variable arity). Could be problem for other event types as well
       (local [(define lng (js-get-field lat/lng "x"))
               (define lat (js-get-field lat/lng "y"))]
         (world-with-effects (effect-updater w (prim-js->scheme lat) (prim-js->scheme lng))
                             (world-updater w (prim-js->scheme lat) (prim-js->scheme lng))))))))

(define (on-map-rightclick! map-dom world-updater effect-updater) ;fix: instead of giving the x, y pos on the map, give back the lat/lng
  (local [(define mymap (js-get-field map-dom "gmap"))]
    (make-world-config
     (lambda (handler)
       (js-call (js-get-named-object "setTimeout")
                false
                (procedure->void-js-fun (lambda ()
                                          (js-call
                                           (js-get-field (js-get-named-object "GEvent") "addListener")
                                           false
                                           (js-get-field map-dom "gmap")
                                           "singlerightclick"
                                           handler)))
                (scheme->prim-js 4000))) ;;CHANGE THIS. Should be a timeout
     (lambda (id) (void))
     (lambda (w dom x/y im)
       (local [(define lng (js-get-field x/y "x"))
               (define lat (js-get-field x/y "y"))]
         (world-with-effects (effect-updater w (prim-js->scheme lat) (prim-js->scheme lng))
                             (world-updater w (prim-js->scheme lat) (prim-js->scheme lng))))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;GMAP EFFECTS + REVERSE-GEOCODING and SCRIPT CALLS
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-values (effect:map make-effect:map effect:map? effect:map-accessor effect:map-mutator)
  (make-effect-type 'effect:map 
                    #f 
                    1 
                    (lambda (map-dom) (error 'effect:map "has no default implementation"))
                    '()
                    (lambda (map-dom name)
                      (if (not
                           (equal? (js-get-field map-dom "gmap") js-undefined))
                          map-dom
                          (error name "expected type <map (dom-sexp)> as 1st argument, given: ~s" map-dom)))))

(define-values (effect:map:zoom make-effect:map:zoom effect:map:zoom? effect:map:zoom-accessor effect:map:zoom-mutator)
  (make-effect-type 'effect:map:zoom
                    effect:map
                    1
                    (lambda (map-dom zoom-val) 
                      (local [(define mymap (js-get-field map-dom "gmap"))]
                        (js-call (js-get-field mymap "setZoom") mymap zoom-val)))
                    '()
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
                      (local [(define mymap (js-get-field map-dom "gmap"))]
                        (js-call (js-get-field mymap "setCenter") mymap (GLatLng lat lng))))
                    '()
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
                      (local [(define mymap (js-get-field map-dom "gmap"))]
                        (js-call (js-get-field mymap "panTo") mymap (GLatLng lat lng))))
                    '()
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
                      (local [(define mymap (js-get-field map-dom "gmap"))]
                        (js-call (js-get-field mymap "clearOverlays") mymap )))
                    '()))

(define-values (effect:reverse-geocode make-effect:reverse-geocode effect:reverse-geocode? effect:reverse-geocode-accessor effect:reverse-geocode-mutator)
  (make-effect-type 'effect:reverse-geocode
                    #f
                    3
                    (lambda (lat lng response)
                      (local [(define geocoder (js-new (js-get-named-object "GClientGeocoder")))]
                        (js-call (js-get-field geocoder "getLocations") geocoder (GLatLng lat lng) response))) 
                    '(2)
                    (lambda (lat lng response name) ;add the actual guard
                      (values lat lng response))))


;make variable arity so no text has to be specified!!
(define-values (effect:map:marker make-effect:map:marker effect:map:marker? effect:map:marker-accessor effect:map:marker-mutator)
  (make-effect-type 'effect:map:marker
                    effect:map
                    3
                    (lambda (map-dom lat lng marker-text)
                      (local [(define mymap (js-get-field map-dom "gmap"))
                              (define marker (js-new 
                                              (js-get-named-object "GMarker") 
                                              (GLatLng lat lng)))]
                        (begin
                          (js-call
                           (js-get-field (js-get-named-object "GEvent") "addListener")
                           false
                           marker
                           "click"
                           (procedure->void-js-fun (lambda (lat/lng)
                                                     (js-call (js-get-field marker "openInfoWindowHtml") 
                                                              marker 
                                                              (scheme->prim-js marker-text)))))
                          (js-call (js-get-field mymap "addOverlay") mymap marker (scheme->prim-js false)))))
                    
                    '()
                    (lambda (map-dom lat lng text name)
                      (if (and 
                           (number? lat)
                           (number? lng)
                           (string? text))
                          (values map-dom lat lng text)
                          (error name "expected type <number> as 2nd and 3rd argument and <string> as 4th argument, given: ~s ~s ~s" lat lng text)))))

(define (my-string-join elts delim) 
  (local [(define s
            (foldr (lambda (s1 s2)
                     (string-append s1 delim s2))
                   ""
                   elts))]
    (if (= (string-length s) 0)
        ""
        (substring s 0 (sub1 (string-length s))))))

(define (escape s)
  (local [(define esc (js-get-named-object "escape"))]
    (prim-js->scheme (js-call esc false s))))

(define (form-url url params)
  (local [
          (define param-string (my-string-join 
                                (map (lambda (l) 
                                       (my-string-join (map escape l) "=")) 
                                     (append params 
                                             (list
                                              (list "output" "json") 
                                              (list "callback" "script_effect_callback"))))
                                "&"))]
    (string-append url "?" param-string)))

(define-values (effect:script make-effect:script effect:script? effect:script-accessor effect:script-mutator)
  (make-effect-type 'effect:script
                    #f
                    3
                    (lambda (url params response) 
                      (local [(define script-url (form-url url params))]
                        (begin
                          (js-set-field! (js-get-named-object "window") "script_effect_callback" response)
                          (loadScript script-url)))) 
                    '(2)
                    (lambda (url params response name)
                      (if (and
                           (list? params)
                           (andmap list? params)
                           (andmap (lambda (x)
                                     (and (= (length x) 2)
                                          (string? (first x))
                                          (string? (second x))))
                                   params)
                           (string? url)
                           (procedure? response))
                          (values url params response)
                          (error name 
                                 "expected type <string> as 1st argument, <list-of (list-of strings)> as 2nd argument, and <procedure> as 3rd argument: given ~s ~s ~s"
                                 url
                                 params
                                 response))))) ;add more stringent guards

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;




