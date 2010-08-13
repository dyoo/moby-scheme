
(provide on-map-drag!
         on-map-drag
         on-map-dragstart!
         on-map-dragstart
         on-map-dragend!
         on-map-dragend
         on-map-click!
         on-map-click
         on-map-dblclick!
         on-map-dblclick
         on-map-rightclick!
         on-map-rightclick
         on-map-zoom!
         on-map-zoom
         on-map-mouseover!
         on-map-mouseover
         on-map-mousemove!
         on-map-mousemove
         on-map-mouseout!
         on-map-mouseout
         make-google-map-marker
         google-map-marker-lat
         google-map-marker-lng
         google-map-marker-text
         make-google-map
         G_NORMAL_MAP
         G_SATELLITE_MAP
         G_AERIAL_MAP
         G_HYBRID_MAP
         G_AERIAL_HYBRID_MAP
         G_PHYSICAL_MAP
         G_MOON_ELEVATION_MAP
         G_MOON_VISIBLE_MAP
         G_MARS_INFRARED_MAP
         G_SKY_VISIBLE_MAP
         zoom?
         make-effect:script!
         make-effect:geocode
         make-effect:reverse-geocode)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;AUXILARY FUNCTIONS/CONSTANTS
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;console-log: js-object -> voidJ
;console-log: In Google Chrome, acts as "console.log" (which logs some Javascript object)
;             Helps mainly with debugging.
(define (console-log obj)
  (let* ([console (js-get-global-value "console")]
         [log (js-get-field console "log")])
    (js-call log console obj)))

;zoom? number->boolean
;consumes a number and returns true if it's a valid as a zoom level, false otherwise
(define (zoom? num)
  (and
   (integer? num)
   (>= num 0)
   (<= num 20)))

(define (js-array->list arr)
  (vector->list
   (prim-js->scheme arr)))

(define (list->js-array l)
  (scheme->prim-js
   (list->vector l)))

;Map type constants
(define G_NORMAL_MAP (box #f))
(define G_SATELLITE_MAP (box #f))
(define G_AERIAL_MAP (box #f))
(define G_HYBRID_MAP (box #f))
(define G_AERIAL_HYBRID_MAP (box #f))
(define G_PHYSICAL_MAP (box #f))
(define G_MOON_ELEVATION_MAP (box #f))
(define G_MOON_VISIBLE_MAP (box #f))
(define G_MARS_INFRARED_MAP (box #f))
(define G_SKY_VISIBLE_MAP (box #f))


;load-script: string -> void
;load-script: given a url representing a script to load, appends the script tag into the document header so that it executes, and returns void
;             Because XMLHttpRequests can't be made cross-domain, this is a way of getting around that problem
(define (load-script url)
  (local [(define document (js-get-global-value "document"))
          (define createElement (js-get-field document "createElement"))
          (define getElementsByTagName (js-get-field document "getElementsByTagName"))
          (define elem (js-call createElement document (scheme->prim-js "script")))
          (define tag (js-get-field (js-call getElementsByTagName document (scheme->prim-js "head")) "0"))]
    (begin
      (js-set-field! elem "src" (scheme->prim-js url))
      (js-set-field! elem "type" (scheme->prim-js "text/javascript"))
      (js-call (js-get-field tag "appendChild") tag elem)
      (void))))

;load-google: -> void
;load-google: loads the Google script, and sets the special map constants defined above
;             Eventually should load from the maps v3 API, but V3 doesn't have support for a few things necessary to make everything work
(define (load-google)
  (begin
    (load-script "http://www.google.com/jsapi?key=ABQIAAAANuLQS-qn8FP_vP2FRBltExQIToEyJbLNev2L3JVYkG889ZSczxQlHFTiT5TMwWkBl4392LfkmDmJ4A") ;load the Google script
    (local [(define onload (lambda ()
                             (local [(define google (js-get-global-value "google"))] ;the Google namespace
                               (if (js-=== google js-undefined) ;if it's undefined, it means it hasn't loaded yet
                                   (begin
                                     (sleep 0.5) ;sleep instead of setTimeout, because it should block
                                     (onload))
                                   (local [(define load (js-get-field google "load"))
                                           (define callback (js-make-hash))]
                                     (begin 
                                       (js-set-field! callback "callback" (procedure->void-js-fun ;a callback for when the maps script is finished loading
                                                                           (lambda ()
                                                                             (begin ;set the map constants. Don't worry about creating the map until the constructor is called
                                                                               (set-box! G_NORMAL_MAP (js-get-global-value "G_NORMAL_MAP"))
                                                                               (set-box! G_SATELLITE_MAP (js-get-global-value "G_SATELLITE_MAP"))
                                                                               (set-box! G_AERIAL_MAP (js-get-global-value "G_AERIAL_MAP"))
                                                                               (set-box! G_HYBRID_MAP (js-get-global-value "G_HYBRID_MAP"))
                                                                               (set-box! G_AERIAL_HYBRID_MAP (js-get-global-value "G_AERIAL_HYBRID_MAP"))
                                                                               (set-box! G_PHYSICAL_MAP (js-get-global-value "G_PHYSICAL_MAP"))
                                                                               (set-box! G_MOON_ELEVATION_MAP (js-get-global-value "G_MOON_ELEVATION_MAP"))
                                                                               (set-box! G_MOON_VISIBLE_MAP (js-get-global-value "G_MOON_VISIBLE_MAP"))
                                                                               (set-box! G_MARS_INFRARED_MAP (js-get-global-value "G_MARS_INFRARED_MAP"))
                                                                               (set-box! G_SKY_VISIBLE_MAP (js-get-global-value "G_SKY_VISIBLE_MAP"))))))
                                       (js-call load google (scheme->prim-js "maps") (scheme->prim-js "2.x") callback)))))))]
      (onload))))


(load-google) ;actually PERFORM the loading before going any further. We have to load the Google scripts before before trying to use their functionality

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;GOOGLE MAPS INTERNAL API (does the behind the scenes work)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;map-state: represents the state of a Google Map
;           A Google Maps has a latitude, longitude, zoom level, markers placed on the map, and a map type
(define-struct map-state (lat ;real?
                          lng ;real?
                          zoom ;zoom? (integer in [0,21])
                          gmarkers ;list-of (google-map-marker)
                          type ;box-of (js-object), where the js-object is a map type - a constant defined under the Google namespace after the maps script has been called
                          ))

;google-map-marker: represents a marker that is placed on a map
;                   The marker has a latitude and longitude, and some text which is displayed when the marker is clicked
(define-struct google-map-marker (title ;string?
                                  lat ;real?
                                  lng ;real?
                                  text ;(or/c string? false?)
                                  image ;url?
                                  ))

;map-from-dom: dom-sexp -> js-object
;map-from-dom: given a dom element (i.e. a div) containing the map, returns the actual map object in the element
(define (map-from-dom map-dom)
  (js-get-field map-dom "gmap"))

;js-marker=?: js-value google-map-marker -> boolean
;js-marker=?: given an actual Javascript marker object and the Racket struct representing a marker, return true if they are equal, false otherwise
(define (js-marker=? js-mark mark)
  (equal? (js-get-field js-mark "scheme-struct")
          mark))

;diff-markers: dom-element map-state -> void
;diff-markers: performs a "diffing" algorithm on the markers. 
;              Rather than clearing the map and redrawing all markers (which would be slow, and could cause markers to flicker),
;              only remove and add those markers which need to be removed or added
(define (diff-markers map-dom next)
  (let ([mymap (map-from-dom map-dom)]
        [mymarkers (js-array->list (js-get-field map-dom "gmarkers"))])
    (begin
      (for-each (lambda (next-marker)
                  (if (not
                       (ormap
                        (lambda (current-marker)
                          (js-marker=? current-marker next-marker))
                        mymarkers))
                      (add-marker map-dom
                                  (google-map-marker-title next-marker)
                                  (google-map-marker-lat next-marker)
                                  (google-map-marker-lng next-marker)
                                  (google-map-marker-text next-marker)
                                  (google-map-marker-image next-marker))
                      (void)))
                (map-state-gmarkers next))
      (let ([removed-markers (box (js-array->list (js-get-field map-dom "gmarkers")))])
        (begin
          (for-each (lambda (current-marker)
                      (if (not
                           (ormap
                            (lambda (next-marker)
                              (js-marker=? current-marker next-marker))
                            (map-state-gmarkers next)))
                          (begin
                            (js-call (js-get-field mymap "removeOverlay")
                                     mymap
                                     current-marker)
                            (set-box! removed-markers (remove current-marker (unbox removed-markers))))
                          (void)))
                    mymarkers)
          (js-set-field! map-dom "gmarkers" (list->js-array (unbox removed-markers))))))))

;render-effect:map: Defines the render effect representing the map object
;                   Since updating the map should appear functional to the user,
;                   render effects are a special type whose implementation is performed at render time only
;                   This allows the map to actually only be updated when the page is rendered, giving the appearance of being functional
(define-values (render-effect:map make-render-effect:map render-effect:map? render-effect:map-accessor render-effect:map-mutator)
  (make-render-effect-type 'render-effect:map
                           #f
                           2
                           (lambda (map-dom next)
                             (let* ([mymap (map-from-dom map-dom)]
                                    [maptype (js-call (js-get-field mymap "getCurrentMapType") mymap)]
                                    [maptypename (js-call (js-get-field maptype "getName") maptype)])
                               (begin ;because operations on the map depend on the CSS of the div in which it's contained, set a timeout to draw AFTER stylings are applied
                                 (js-call (js-get-global-value "setTimeout")
                                          #f
                                          (procedure->void-js-fun
                                           (lambda ()
                                             (begin
                                               (js-call
                                                (js-get-field mymap "checkResize") ;necessary if the map is dynamically resized
                                                mymap)
                                               (if (false? (prim-js->scheme (js-call
                                                                             (js-get-field mymap "doubleClickZoomEnabled")
                                                                             mymap))) ;make sure double clicking doesn't zoom the map
                                                   (void)
                                                   (js-call
                                                    (js-get-field mymap "disableDoubleClickZoom")
                                                    mymap))
                                               (set-center map-dom (map-state-lat next) (map-state-lng next)) ;update the center
                                               (set-zoom map-dom (map-state-zoom next)) ;update the zoom
                                               (if (not
                                                    (equal? maptype (unbox (map-state-type next))))
                                                   (set-map-type map-dom (map-state-type next))
                                                   (void))
                                               (diff-markers map-dom next)))) ;diff the markers
                                          0)
                                 map-dom))) ;finally, return the updated div containing the map
                           (lambda (map-dom next name)
                             (if (and
                                  (not
                                   (equal? (map-from-dom map-dom) js-undefined))
                                  (map-state? next))
                                 (values map-dom next)
                                 (error name 
                                        "expected type <map (dom-sexp)> as 1st argument and <map-state> as 2nd argument, given: ~s ~s" map-dom next)))))

;The accessors for elements in the render-effect (so we can grab at them)
(define render-effect:map-map-dom (make-struct-field-accessor render-effect:map-accessor 0))
(define render-effect:map-next (make-struct-field-accessor render-effect:map-accessor 1))

;GLatLng: number number -> js-object
;GLatLng: given a latitude and longitude, returns a GLatLng object (a js-object) representing a location to be used for a Google Map
(define (GLatLng lat lng)
  (js-new (js-get-global-value "GLatLng") (scheme->prim-js lat) (scheme->prim-js lng)))

;update-location: render-effect:map number number -> render-effect:map
;update-location: given a render-effect:map, a latitude, and longitude, updates the "next" state of the render effect to reflect the new position at which to center the map
;                 during render time
(define (update-location map-to-render lat lng)
  (make-render-effect:map 
   (render-effect:map-map-dom map-to-render)
   (make-map-state lat 
                   lng 
                   (map-state-zoom (render-effect:map-next map-to-render)) 
                   (map-state-gmarkers (render-effect:map-next map-to-render))
                   (map-state-type (render-effect:map-next map-to-render)))))

;update-zoom: render-effect:map zoom -> render-effect:map
;update-zoom: given a render-effect:map and a zoom level, updates the "next" state of the render effect to reflect the new level at which to zoom the map
;                 during render time
(define (update-zoom map-to-render zoom)
  (make-render-effect:map 
   (render-effect:map-map-dom map-to-render)
   (make-map-state (map-state-lat (render-effect:map-next map-to-render)) 
                   (map-state-lng (render-effect:map-next map-to-render)) 
                   zoom 
                   (map-state-gmarkers (render-effect:map-next map-to-render))
                   (map-state-type (render-effect:map-next map-to-render)))))

;update-markers: render-effect:map list-of-google-map-marker -> render-effect:map
;update-markers: given a render-effect:map and a list of markers, updates the "next" state of the render effect to reflect the markers to be displayed on the map
(define (update-markers map-to-render gmarkers)
  (make-render-effect:map 
   (render-effect:map-map-dom map-to-render)
   (make-map-state (map-state-lat (render-effect:map-next map-to-render)) 
                   (map-state-lng (render-effect:map-next map-to-render)) 
                   (map-state-zoom (render-effect:map-next map-to-render)) 
                   gmarkers
                   (map-state-type (render-effect:map-next map-to-render)))))

;update-map-type: render-effect:map box-of(js-object) -> render-effect:map
;update-map-type: given a render-effect:map and a box holding a map type, updates the "next" state of the render effect to reflect the new map type
(define (update-map-type map-to-render type)
  (make-render-effect:map 
   (render-effect:map-map-dom map-to-render)
   (make-map-state (map-state-lat (render-effect:map-next map-to-render)) 
                   (map-state-lng (render-effect:map-next map-to-render)) 
                   (map-state-zoom (render-effect:map-next map-to-render)) 
                   (map-state-gmarkers (render-effect:map-next map-to-render))
                   type)))

;set-center: dom-sexp number number -> void
;set-center: given a dom-sexp holding a map object, a latitude, and a longitude, sets the map's location to the new center
(define (set-center map-dom lat lng)
  (let ([my-map (map-from-dom map-dom)])
    (js-call (js-get-field my-map "setCenter") 
             my-map 
             (GLatLng lat lng))))

;set-zoom: dom-sexp zoom -> void
;set-zoom: given a dom-sexp holding a map object and a zoom level (integer in [0,21]), sets the map's zoom level
(define (set-zoom map-dom zoom)
  (let ([my-map (map-from-dom map-dom)])
    (js-call (js-get-field my-map "setZoom") my-map (scheme->prim-js zoom))))

;add-marker: dom-sexp number number string -> void
;add-marker: given a dom-sexp holding a map object, a latitude, a longitude, and some text, places a marker on the map
(define (add-marker map-dom title lat lng marker-text img)
  (let* ([mymap (map-from-dom map-dom)]
         [icon-options (js-make-hash)]
         [marker (if (false? img)
                     (begin
                       (js-set-field! icon-options "title" (scheme->prim-js title))
                       (js-new
                        (js-get-global-value "GMarker")
                        (GLatLng lat lng)
                        icon-options))
                     (let* ([icon (js-new (js-get-global-value "GIcon"))]
                            [the-img (open-image-url img)]
                            [width (image-width the-img)]
                            [height (image-height the-img)])
                       (begin
                         (js-set-field! icon "image" (scheme->prim-js img))
                         (js-set-field! icon "iconSize" (js-new (js-get-global-value "GSize")
                                                                width
                                                                height))
                         (js-set-field! icon "shadow" (scheme->prim-js "http://chart.apis.google.com/chart?chst=d_map_pin_shadow"))
                         (js-set-field! icon "shadowSize" (js-new (js-get-global-value "GSize")
                                                                  (scheme->prim-js 22)
                                                                  (scheme->prim-js 20)))
                         (js-set-field! icon "iconAnchor" (js-new (js-get-global-value "GPoint")
                                                                  (scheme->prim-js 6)
                                                                  (scheme->prim-js 20)))
                         (js-set-field! icon "infoWindowAnchor" (js-new (js-get-global-value "GPoint")
                                                                        (scheme->prim-js 5)
                                                                        (scheme->prim-js 1)))
                         (js-set-field! icon-options "icon" icon)
                         (js-set-field! icon-options "title" (scheme->prim-js title))
                         (js-new
                          (js-get-global-value "GMarker")
                          (GLatLng lat lng)
                          icon-options))))]
         [mymarkers (js-get-field map-dom "gmarkers")])
    (begin
      (js-set-field! marker
                     "scheme-struct"
                     (make-google-map-marker title lat lng marker-text img))
      (js-call (js-get-field mymarkers "push")
               mymarkers
               marker)
      (js-call (js-get-field mymap "addOverlay") mymap marker (scheme->prim-js false))
      (if (false? marker-text) 
          (void)
          (js-call
           (js-get-field (js-get-global-value "GEvent") "addListener")
           false
           marker
           "click"
           (procedure->void-js-fun (lambda (lat/lng)
                                     (js-call (js-get-field marker "openInfoWindowHtml") 
                                              marker 
                                              (scheme->prim-js marker-text)))))))))

;set-map-type: dom-sexp box-of(js-object) -> void
;set-map-type: given a dom-sexp holding a map object, and a box holding a map type, sets the type of the map
(define (set-map-type map-dom type)
  (let ([my-map (map-from-dom map-dom)])
    (js-call (js-get-field my-map "setMapType")
             my-map
             (unbox type))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;GOOGLE MAPS EXTERNAL API (what the end-user will see)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;gmap: number number number box-of(js-object) list-of-(css-sexp) -> render-effect:map
;gmap: acts as the constructor for creating a Google Map
;      It returns a render-effect:map, which contains a div that holds the map, and a "next" state
;      So the map can be updated at render time
(define (make-google-map lat lng zoom attribs)
  (local [(define dom (js-div attribs))
          (define body (js-get-field (js-get-global-value "document") "body"))
          (define appendChild (js-get-field body "appendChild"))]
    (if (not
         (and
          (real? lat)
          (real? lng)
          (zoom? zoom)
          (list? attribs)))
        (error "make-google-map expected type <real> as 1st argument, <real> as 2nd argument, <zoom> as 3rd argument, and <(listof css-sexp)> as 4th argument: given ~s ~s ~s ~s" lat lng zoom attribs)
        (begin
          (js-set-field! dom "jsworldOpaque" (scheme->prim-js true)) ;sets the opacity so that the css stylings of the map won't be screwed with by jsworld
          (js-call appendChild body dom) ;the div holding the map must be part of the dom tree before calling the Google Map functions. Weird resizing error
          (let* ([maps (js-get-field (js-get-global-value "google") "maps")]
                 [mymap (js-new (js-get-field maps "Map2") dom)])
            (begin
              (js-set-field! dom "gmap" mymap)
              (js-set-field! dom "gmarkers" (js-new (js-get-global-value "Array")))
              (js-call (js-get-field mymap "setCenter")
                       mymap
                       (GLatLng lat lng)
                       (scheme->prim-js zoom))
              (js-call (js-get-field mymap "setUIToDefault") mymap)
              (js-call (js-get-field mymap "disableDoubleClickZoom") mymap)
              (make-render-effect:map dom (make-map-state lat lng zoom '() G_NORMAL_MAP))))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;GOOGLE MAPS BIG-BANG EVENT HANDLERS;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (map-stimulus-handler a-map type impl)
  (make-world-config
   (lambda (bb)
     (js-call
      (js-get-field (js-get-global-value "GEvent") "addListener")
      false
      a-map 
      type
      (impl bb)))
   (lambda (listener) (js-call
                       (js-get-field 
                        (js-get-global-value "GEvent")
                        "removeListener")
                       false
                       listener))))


(define (on-map-drag-type! map-dom world-updater effect-updater type)
  (let ([mymap (map-from-dom (render-effect:map-map-dom map-dom))])
    (map-stimulus-handler mymap 
                          type 
                          (lambda (bb)
                            (procedure->void-js-fun
                             (lambda ()
                               ((bb-info-change-world bb) 
                                (lambda (w)
                                  (local [(define get-center (js-get-field mymap "getCenter"))
                                          (define center (js-call get-center mymap))
                                          (define lat (js-call (js-get-field center "lat") center))
                                          (define lng (js-call (js-get-field center "lng") center))]
                                    (world-with-effects (effect-updater w (prim-js->scheme lat) (prim-js->scheme lng))
                                                        (world-updater w (prim-js->scheme lat) (prim-js->scheme lng))))))))))))

(define (on-map-mouse-type! map-dom world-updater effect-updater type)
  (let ([mymap (map-from-dom (render-effect:map-map-dom map-dom))])
    (map-stimulus-handler mymap 
                          type 
                          (lambda (bb)
                            (procedure->void-js-fun
                             (lambda (lat/lng)
                               ((bb-info-change-world bb) 
                                (lambda (w)
                                  (local [(define lat (js-call (js-get-field lat/lng "lat") lat/lng))
                                          (define lng (js-call (js-get-field lat/lng "lng") lat/lng))]
                                    (world-with-effects (effect-updater w (prim-js->scheme lat) (prim-js->scheme lng))
                                                        (world-updater w (prim-js->scheme lat) (prim-js->scheme lng))))))))))))

(define (on-map-mouseover! map-dom world-updater effect-updater)
  (on-map-mouse-type! map-dom world-updater effect-updater "mouseover"))

(define (on-map-mouseover map-dom world-updater)
  (on-map-mouseover! map-dom world-updater (lambda (w lat lng) '())))

(define (on-map-mouseout! map-dom world-updater effect-updater)
  (on-map-mouse-type! map-dom world-updater effect-updater "mouseout"))

(define (on-map-mouseout map-dom world-updater)
  (on-map-mouseout! map-dom world-updater (lambda (w lat lng) '())))

(define (on-map-mousemove! map-dom world-updater effect-updater)
  (on-map-mouse-type! map-dom world-updater effect-updater "mousemove"))

(define (on-map-mousemove map-dom world-updater)
  (on-map-mousemove! map-dom world-updater (lambda (w lat lng) '())))

;on-map-drag!: dom-sexp (world number number -> world) (world number number -> effect) -> world-config
;on-map-drag!: given a div holding a map object, a world updating function, and an effect producing function, produces a world config that updates the world
;              when the map is dragged
(define (on-map-drag! map-dom world-updater effect-updater)
  (on-map-drag-type! map-dom world-updater effect-updater "drag"))

;on-map-drag: dom-sexp (world number number -> world)
;on-map-drag: same as on-map-drag!, except without an effect updater (calls on-map-drag!, with an effect-producing function that produces no effect)
(define (on-map-drag map-dom world-updater)
  (on-map-drag! map-dom world-updater (lambda (w lat lng) '())))

;on-map-drag!: dom-sexp (world number number -> world) (world number number -> effect) -> world-config
;on-map-drag!: given a div holding a map object, a world updating function, and an effect producing function, produces a world config that updates the world
;              when the map is dragged
(define (on-map-dragstart! map-dom world-updater effect-updater)
  (on-map-drag-type! map-dom world-updater effect-updater "dragstart"))

;on-map-drag: dom-sexp (world number number -> world)
;on-map-drag: same as on-map-drag!, except without an effect updater (calls on-map-drag!, with an effect-producing function that produces no effect)
(define (on-map-dragstart map-dom world-updater)
  (on-map-dragstart! map-dom world-updater (lambda (w lat lng) '())))

;on-map-drag!: dom-sexp (world number number -> world) (world number number -> effect) -> world-config
;on-map-drag!: given a div holding a map object, a world updating function, and an effect producing function, produces a world config that updates the world
;              when the map is dragged
(define (on-map-dragend! map-dom world-updater effect-updater)
  (on-map-drag-type! map-dom world-updater effect-updater "dragend"))

;on-map-drag: dom-sexp (world number number -> world)
;on-map-drag: same as on-map-drag!, except without an effect updater (calls on-map-drag!, with an effect-producing function that produces no effect)
(define (on-map-dragend map-dom world-updater)
  (on-map-dragend! map-dom world-updater (lambda (w lat lng) '())))

(define (on-map-click! map-dom world-updater effect-updater)
  (let ([mymap (map-from-dom (render-effect:map-map-dom map-dom))])
    (map-stimulus-handler mymap 
                          "click"
                          (lambda (bb)
                            (procedure->void-js-fun
                             (lambda (overlay lat/lng) ;BIG PROBLEM. Use case-lambda (or variable arity). Could be problem for other event types as well
                               ((bb-info-change-world bb) 
                                (lambda (w)
                                  (local [(define lng (js-get-field lat/lng "x"))
                                          (define lat (js-get-field lat/lng "y"))]
                                    (world-with-effects (effect-updater w (prim-js->scheme lat) (prim-js->scheme lng))
                                                        (world-updater w (prim-js->scheme lat) (prim-js->scheme lng))))))))))))

;on-map-click: dom-sexp (world number number -> world)
;on-map-click: same as on-map-click!, except without an effect updater (calls on-map-click!, with an effect-producing function that produces no effect)
(define (on-map-click map-dom world-updater)
  (on-map-click! map-dom world-updater (lambda (w lat lng) '())))


(define (on-map-dblclick! map-dom world-updater effect-updater)
  (let ([mymap (map-from-dom (render-effect:map-map-dom map-dom))])
    (map-stimulus-handler mymap 
                          "dblclick" 
                          (lambda (bb)
                            (procedure->void-js-fun
                             (lambda (overlay lat/lng) ;BIG PROBLEM. Use case-lambda (or variable arity). Could be problem for other event types as well
                               ((bb-info-change-world bb)
                                (lambda (w)
                                  (local [(define lng (js-get-field lat/lng "x"))
                                          (define lat (js-get-field lat/lng "y"))]
                                    (world-with-effects (effect-updater w (prim-js->scheme lat) (prim-js->scheme lng))
                                                        (world-updater w (prim-js->scheme lat) (prim-js->scheme lng))))))))))))

(define (on-map-dblclick map-dom world-updater)
  (on-map-dblclick! map-dom world-updater (lambda (w lat lng) '())))


(define (on-map-rightclick! map-dom world-updater effect-updater) ;fix: instead of giving the x, y pos on the map, give back the lat/lng
  (let ([mymap (map-from-dom (render-effect:map-map-dom map-dom))])
    (map-stimulus-handler mymap 
                          "singlerightclick" 
                          (lambda (bb)
                            (procedure->void-js-fun
                             (lambda (dom x/y im)
                               ((bb-info-change-world bb)
                                (lambda (w)
                                  (local [(define lng (js-get-field x/y "x"))
                                          (define lat (js-get-field x/y "y"))]
                                    (world-with-effects (effect-updater w (prim-js->scheme lat) (prim-js->scheme lng))
                                                        (world-updater w (prim-js->scheme lat) (prim-js->scheme lng))))))))))))

(define (on-map-rightclick map-dom world-updater)
  (on-map-rightclick! map-dom world-updater (lambda (w lat lng) '())))


(define (on-map-zoom! map-dom world-updater effect-updater)
  (let ([mymap (map-from-dom (render-effect:map-map-dom map-dom))])
    (map-stimulus-handler mymap 
                          "zoomend" 
                          (lambda (bb)
                            (procedure->void-js-fun
                             (lambda (old-zoom new-zoom random-arg) ;what is random-arg. GET VARIBLE ARITY FUNCTIONS!
                               ((bb-info-change-world bb)
                                (lambda (w)
                                  (world-with-effects (effect-updater w (prim-js->scheme old-zoom) (prim-js->scheme new-zoom))
                                                      (world-updater w (prim-js->scheme old-zoom) (prim-js->scheme new-zoom)))))))))))

(define (on-map-zoom map-dom world-updater)
  (on-map-zoom! map-dom world-updater (lambda (w old-zoom new-zoom) '())))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;GMAP EFFECTS + REVERSE-GEOCODING and SCRIPT CALLS - MUST BE CHANGED TO MATCH NEW TYPE SIGNATURES
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

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
  (local [(define esc (js-get-global-value "escape"))]
    (prim-js->scheme (js-call esc false s))))

(define (form-url url params)
  (local [(define param-string (my-string-join 
                                (map (lambda (l) 
                                       (my-string-join (map escape l) "=")) 
                                     (append params 
                                             (list
                                              (list "output" "json") 
                                              (list "callback" "script_effect_callback"))))
                                "&"))]
    (string-append url "?" param-string)))

(define-values (effect:script! make-effect:script! effect:script!? effect:script-accessor! effect:script-mutator!)
  (make-effect-type 'effect:script!
                    #f
                    4
                    (lambda (change-world url params world-updater effect-updater) 
                      (local [(define script-url (form-url url params))]
                        (begin
                          (js-set-field! 
                           (js-get-global-value "window") 
                           (scheme->prim-js "script_effect_callback")
                           (procedure->void-js-fun
                            (lambda (data)
                              (change-world 
                               (lambda (w)
                                 (world-with-effects
                                  (effect-updater w data)
                                  (world-updater w data))))))) ;give back the wrapped data
                          (load-script script-url)))) 
                    (lambda (url params world-updater effect-updater name)
                      (if (and
                           (list? params)
                           (andmap list? params)
                           (andmap (lambda (x)
                                     (and (= (length x) 2)
                                          (string? (first x))
                                          (string? (second x))))
                                   params)
                           (string? url)
                           (procedure? world-updater)
                           (procedure? effect-updater))
                          (values url params world-updater effect-updater)
                          (error name 
                                 "expected type <string> as 1st argument, <list-of (list-of string)> as 2nd argument, and <procedure> as 3rd argument: given ~s ~s ~s ~s"
                                 url
                                 params
                                 world-updater
                                 effect-updater)))))


(define-values (effect:reverse-geocode make-effect:reverse-geocode effect:reverse-geocode? effect:reverse-geocode-accessor effect:reverse-geocode-mutator)
  (make-effect-type 'effect:reverse-geocode
                    #f
                    3
                    (lambda (change-world lat lng response)
                      (local [(define geocoder (js-new (js-get-global-value "GClientGeocoder")))]
                        (js-call (js-get-field geocoder "getLocations") 
                                 geocoder 
                                 (GLatLng lat lng) 
                                 (procedure->void-js-fun
                                  (lambda (data)
                                    (change-world 
                                     (lambda (w) (response w (prim-js->scheme data)))))))))
                    (lambda (lat lng response name) 
                      (if (and
                           (number? lat)
                           (number? lng)
                           (procedure? response))
                          (values lat lng response)
                          (error name
                                 "expected type <number> as 1st argument, <number> as 2nd argument, and <procedure> as 3rd argument: given ~s ~s ~s" lat lng response)))))


(define-values (effect:geocode make-effect:geocode effect:geocode? effect:geocode-accessor effect:geocode-mutator)
  (make-effect-type 'effect:geocode
                    #f
                    2
                    (lambda (change-world addr response)
                      (local [(define geocoder (js-new (js-get-global-value "GClientGeocoder")))]
                        (js-call (js-get-field geocoder "getLocations") 
                                 geocoder 
                                 (scheme->prim-js addr)
                                 (procedure->void-js-fun
                                  (lambda (data)
                                    (change-world 
                                     (lambda (w)
                                       (response w (prim-js->scheme data)))))))))
                    (lambda (addr response name)
                      (if (and
                           (string? addr)
                           (procedure? response))
                          (values addr response)
                          (error name
                                 "expected type <string> as 1st argument, <procedure> as 2nd argument: given ~s ~s" addr response)))))
#|
(define-values (effect:map make-effect:map effect:map? effect:map-accessor effect:map-mutator)
  (make-effect-type 'effect:map 
                    #f 
                    1 
                    (lambda (change-world map-dom) (error 'effect:map "has no default implementation"))
                    (lambda (map-dom name)
                      (if (not
                           (equal? (js-get-field map-dom "gmap") js-undefined))
                          map-dom
                          (error name "expected type <map (dom-sexp)> as 1st argument, given: ~s" map-dom)))))

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

(let* ([lat (js-call (js-get-field lat/lng "lat") #f)]
                                         [lng (js-call (js-get-field lat/lng "lng") #f)])
                                    (world-with-effects (effect-updater w (prim-js->scheme lat) (prim-js-scheme lng))
                                                        (world-updater w (prim-js->scheme lat) (prim-js-scheme lng))))



|#

