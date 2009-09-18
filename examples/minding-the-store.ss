#lang s-exp "../moby-lang.ss"

;; Minding the store.
;;
;; Alerts the user whenever he or she gets close to a store with
;; items on their shopping list.
;;
;; The user specifies stores by marking them on a Google Maps and
;; telling the program the RSS feed URL.  The description field is
;; used to grab the list of items at a store.
;;
;; See: http://mapki.com/wiki/Google_Map_Parameters

;; The url to the Google Maps "My Maps" RSS feed.
(define MYMAPS-URL
  (string-append "http://maps.google.com/maps/ms?ie=UTF8&hl=en&vps=1&jsv=151e&msa=0&output=georss&msid="
                 "106933521686950086948.00046579f4b482756abc5"))


;; The world is current location, the places that are nearby that location, and a description
;; of the items we've found at that place.
(define-struct world (loc nearby-places))

;; A location is the latitude/longitude pair.
(define-struct loc (lat long))

;; Our initial world will be in limbo; we'll be relocated as soon as we get a geolocation point.
(define initial-world (make-world (make-loc 0 0) empty))

;; A place is a name string, a location, a radius number, and an item string
(define-struct place (name loc radius item))

;; update-location: world number number -> world
;; Updates the current location.
(define (update-location w lat long)
  (make-world (make-loc lat long) 
              (find-nearby-places ALL-PLACES (make-loc lat long))))

;; find-places: (listof place) loc -> (listof place)
;; Finds places that match the a-loc.
(define (find-nearby-places places a-loc)
  (cond [(empty? places)
         empty]
        [(place-matches? (first places) a-loc)
         (cons (first places) (find-nearby-places (rest places) a-loc))]
        [else
         (find-nearby-places (rest places) a-loc)]))

;; place-matches?: place loc -> boolean
;; Returns true if the place matches the location.
(define (place-matches? a-place a-loc)
  (<= (location-distance (loc-lat a-loc)
                         (loc-long a-loc)
                         (loc-lat (place-loc a-place))
                         (loc-long (place-loc a-place)))
      (place-radius a-place)))

;; draw: world -> dom-sexpr
(define (draw w)
  (list (js-div '(("id" "main")))
        (list (js-div '(("id" "title"))) 
              (list (js-text "Minding the Store")))
        (list (js-div) 
              (list (js-text "Current location: "))
              (list (js-text (loc->string (world-loc w)))))
        (list* (js-div) 
               (list (js-text "Nearby items by location: "))
               (draw-items w))))


;; draw-items: world -> (listof dom-sexpr)
(define (draw-items w)
  (map (lambda (p)
         (list (js-div)
               (list (js-text (place-name p)))
               (list (js-text ": "))
               (list (js-text (place-item p)))))
       (keep-places-with-items (world-nearby-places w))))


;; keep-places-with-items: (listof place) -> (listof place)
;; Keep the places that have items associated to them.
(define (keep-places-with-items places)
  (filter (lambda (p) (not (string-whitespace? (place-item p))))
          places))

;; draw-css: world -> css-sexpr
(define (draw-css w)
  '(("title" ("font-size" "20px"))
    ("main" ("border-style" "solid"))))


;; loc->string: loc -> string
(define (loc->string a-loc)
  (format "~a, ~a" (loc-lat a-loc) (loc-long a-loc)))



;; ignore: world -> world
;; Ignore the world and just return it.
(define (ignore w)
  w)


;; beep-if-near: world -> effect
;; Beeps if we're near a place with items.
(define (beep-if-near w)
  (cond
    [(empty? (keep-places-with-items (world-nearby-places w)))
     empty]
    [else
     (make-effect:beep)]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; RSS Parser Helpers.

;; parse-places: xexpr -> (listof place)
;; Parses out the places from the RSS we get from Google Maps.
(define (parse-places xexpr)
  (parse-items
   (sxml-find-children 'item 
                       (sxml-children (first (sxml-find-children 'channel (sxml-children xexpr)))))))


;; parse-items: (listof xexpr) -> (listof place)
(define (parse-items xexprs)
  (cond
    [(empty? xexprs)
     empty]
    [else
     (cons (parse-item (first xexprs))
           (parse-items (rest xexprs)))]))


;; parse-item: xexpr -> place
;; Parses an item from the RSS feed.
(define (parse-item xexpr)
  (local [(define (get-description-text x)
            (cond
              [(string=? x "")
               ""]
              [else
               (sxml-text (parse-xml (string-append "<top>" x "</top>")))]))]
    (make-place (sxml-text (first (sxml-find-children 'title (sxml-children xexpr))))
                
                (cond
                  [(empty? (sxml-find-children 'georss:point (sxml-children xexpr)))
                   (make-loc 0 0)]
                  [else
                   (parse-georss:point 
                    (first (sxml-find-children 'georss:point (sxml-children xexpr))))])
                
                ;; At the moment, we default to a radius of 100 meters.
                100
                
                
                (cond
                  [(empty? (sxml-find-children 'description (sxml-children xexpr)))
                   ""]
                  [else
                   (apply string-append
                          (map get-description-text 
                               (sxml-children (first (sxml-find-children 'description (sxml-children xexpr))))))]))))


;; parse-georss:point: xexpr -> loc
(define (parse-georss:point xexpr)
  (make-loc (string->number (first (split-whitespace (sxml-text xexpr))))
            (string->number (second (split-whitespace (sxml-text xexpr))))))




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; XML Parser Helpers.
;;
;; This code might be absorbed into the Moby library.

;; children: sxml -> (listof sxml)
(define (sxml-children a-sxml)
  (cond
    [(string? a-sxml)
     (error 'children "Can't have children of a string xexpr")]
    [else
     (rest (rest a-sxml))]))


;; get-text: sxml -> string
(define (sxml-text a-sxml)
  (local [;; get-text*: (listof xexpr) -> string
          (define (get-text* xexprs)
            (cond
              [(empty? xexprs)
               ""]
              [else
               (string-append (sxml-text (first xexprs))
                              (get-text* (rest xexprs)))]))]
    (cond
      [(string? a-sxml)
       a-sxml]
      [(pair? a-sxml)
       (get-text* (sxml-children a-sxml))]
      [(empty? a-sxml)
       ""])))


;; sxml-find-children: symbol (listof sxml) -> (listof sxml)
(define (sxml-find-children name children)
  (cond [(empty? children)
         empty]
        [else
         (cond [(string? (first children))
                (sxml-find-children name (rest children))]
               [(pair? (first children))
                (cond
                  [(symbol=? name (first (first children)))
                   (cons (first children)
                         (sxml-find-children name (rest children)))]
                  [else
                   (sxml-find-children name (rest children))])]
               [else
                (error 'find-children children)])]))




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;  


(define ALL-PLACES
  (parse-places 
   (parse-xml (get-url MYMAPS-URL))))


(js-big-bang initial-world
             (on-tick* 30 ignore beep-if-near)
             (on-location-change update-location)

             (on-draw draw draw-css))