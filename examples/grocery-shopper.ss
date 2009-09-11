;; The first three lines of this file were inserted by DrScheme. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname grocery-shopper) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ())))
;; Grocery store shopper
;; See: http://mapki.com/wiki/Google_Map_Parameters

(define WIDTH 320)
(define HEIGHT 480)


;; A location is the latitude/longitude pair.
(define-struct loc (lat long))

;; A place is a name string, a location, and a radius number.
(define-struct place (name loc radius))

;; A grocery item is a place-name and an identifier.
(define-struct item (place-name identifier))

;; Here are a list of the items we're interested in.
(define ALL-ITEMS
  (list (make-item "Stop and Shop" "Soap")
        (make-item "Boynton" "Pizza")))


;; The world is current location.
;; The initial world is one for dyoo's My Saved Places.
(define-struct world (loc nearby-places description))

(define initial-world 
  (make-world (make-loc 0 0) empty ""))


;; update-location: world number number -> world
;; Updates the current location.
(define (update-location w lat long)
  (update-world-description 
   (make-world (make-loc lat long)
	       (find-places ALL-PLACES w)
	       "")))

(define (update-world-description w)
  (make-world (world-loc w)
	      (world-nearby-places w)
	      (compute-description (world-loc w))))
	      


;; filter-matching-items: place (listof item) -> (listof item)
;; Given a place, lists out any of the items whose place-name matches.
(define (filter-matching-items a-place items)
  (cond
    [(empty? items)
     empty]
    [(string=? (place-name a-place)
               (item-place-name (first items)))
     (cons (first items)
           (filter-matching-items a-place (rest items)))]
    [else
     (filter-matching-items a-place (rest items))]))


;; places-matching-items: (listof places) -> (listof item)
;; Reports the list of nearby matching items for the given places.
(define (places-matching-items places)
  (cond
    [(empty? places)
     empty]
    [else
     (append (filter-matching-items (first places) ALL-ITEMS)
             (places-matching-items (rest places)))]))



;; nearby-matching-items: world -> (listof item)
;; Returns all items that are nearby our current location.
(define (nearby-matching-items w)
  (places-matching-items (find-places ALL-PLACES w)))
  

;; find-places: (listof place) loc -> (listof place)
;; Finds places that match the a-loc.
(define (find-places places a-loc)
  (cond
    [(empty? places)
     empty]
    [(place-matches? (first places) a-loc)
     (cons (first places) (find-places (rest places) a-loc))]
    [else
     (find-places (rest places) a-loc)]))

;; place-matches?: place loc -> boolean
;; Returns true if the place matches the location.
(define (place-matches? a-place a-loc)
  (<= (location-distance (loc-lat a-loc)
                         (loc-long a-loc)
                         (loc-lat (place-loc a-place))
                         (loc-long (place-loc a-place)))
      (place-radius a-place)))



;; compute-description: pos -> string
;; Produces a text description.
(define (compute-description p)
  (items->string (nearby-matching-items p)))


;; items->string: (listof item) -> string
(define (items->string items)
  (cond
    [(empty? items)
     ""]
    [else
     (string-append
      (item->string (first items))
      (cond [(empty? (rest items)) ""] [else ", "])
      (items->string (rest items)))]))


;; item->string: item -> string
(define (item->string an-item)
  (string-append "[" 
                 (item-place-name an-item)
                 ": "
                 (item-identifier an-item)
                 "]"))


;; render: world -> scene
(define (render w)
  (place-image
   (text (world-description w) 10 "black")
   20 
   20
   (empty-scene WIDTH HEIGHT)))



;; location-distance: num num num num -> number
;; Given two places on a globe, return the shortest distance between them in meters (uses spherical geometry)
(define (location-distance latA lonA latB lonB)
  (* 6378000
     (* 2
        (asin (min 1
                   (sqrt (+ (expt (sin (/ (- (deg->rad latA) (deg->rad latB)) 2)) 2)
                            (* (cos (deg->rad latA))
                               (cos (deg->rad latB))
                               (expt (sin (/ (- (deg->rad lonA) (deg->rad lonB)) 2)) 2)))))))))


;; deg->rad: number -> number
;; Converts degrees to radians.
(define (deg->rad a-deg)
  (/ (* a-deg pi) 180))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; RSS Parser Helpers.

;; parse-places: xexpr -> (listof place)
;; Parses out the places from the RSS we get from Google Maps.
(define (parse-places xexpr)
  (parse-items
   (find-children 'item 
                  (children (first (find-children 'channel (children xexpr)))))))


;; parse-items: (listof xexpr) -> (listof place)
(define (parse-items xexprs)
  (cond
    [(empty? xexprs)
     empty]
    [else
     (cons (parse-item (first xexprs))
           (parse-items (rest xexprs)))]))


;; parse-item: xexpr -> place
(define (parse-item xexpr)
  (cond [(empty? (find-children 'georss:point (children xexpr)))
	 (make-place (get-text (first (find-children 'title (children xexpr))))      
		     (make-loc 0 0)
		     100)]
	[else
	 (make-place (get-text (first (find-children 'title (children xexpr))))
		     (parse-georss:point 
		      (first (find-children 'georss:point (children xexpr))))
		     ;; At the moment, we default to a radius of 100 meters.
		     100)]))


;; parse-georss:point: xexpr -> loc
(define (parse-georss:point xexpr)
  (make-loc (string->number (first (split-whitespace (get-text xexpr))))
            (string->number (second (split-whitespace (get-text xexpr))))))

  
;; split-words: (listof string) (listof string) -> (listof string)
(define (split-words letters word-letters-so-far)
  (cond
    [(empty? letters)
     (list (implode (reverse word-letters-so-far)))]
    [(string-whitespace? (first letters))
     (cond [(empty? word-letters-so-far)
            (split-words (rest letters) empty)]
           [else
            (cons (implode (reverse word-letters-so-far))
                  (split-words (rest letters) empty))])]
    [else
     (split-words (rest letters) 
                  (cons (first letters)
                        word-letters-so-far))]))

;; split-whitespace: string -> (listof string)
(define (split-whitespace s)
  (split-words (explode s) empty))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; XML Parser Helpers.

;; children: xexpr -> (listof xexpr)
(define (children an-xexpr)
  (cond
    [(string? an-xexpr)
     (error 'children "Can't have children of a string xexpr")]
    [else
     (rest (rest an-xexpr))]))

;; attrs: xexpr -> (listof (list symbol string))
(define (attrs an-xexpr)
  (cond
    [(string? an-xexpr)
     (error 'attrs "Can't get attributes of a string xexpr")]
    [else
     (second an-xexpr)]))

;; get-text: xexpr -> string
(define (get-text an-xexpr)
  (cond
    [(string? an-xexpr)
     an-xexpr]
    [(pair? an-xexpr)
     (get-text* (children an-xexpr))]))

;; get-text*: (listof xexpr) -> string
(define (get-text* xexprs)
  (cond
    [(empty? xexprs)
     ""]
    [else
     (string-append (get-text (first xexprs))
                    (get-text* (rest xexprs)))]))

;; find-children: symbol (listof xexpr) -> (listof xexpr)
(define (find-children name children)
  (cond [(empty? children)
         empty]
        [else
         (cond [(string? (first children))
                (find-children name (rest children))]
               [(pair? (first children))
                (cond
                  [(symbol=? name (first (first children)))
                   (cons (first children)
                         (find-children name (rest children)))]
                  [else
                   (find-children name (rest children))])]
               [else
                (error 'find-children children)])]))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;  


;; The url to the Google Maps "My Maps" RSS feed.
(define mymaps-url
  (string-append "http://maps.google.com/maps/ms?ie=UTF8&hl=en&vps=1&jsv=151e&msa=0&output=georss&msid="
                 "106933521686950086948.00046579f4b482756abc5"))

(define ALL-PLACES
  (parse-places (parse-xml (get-url mymaps-url))))


(js-big-bang initial-world
             ;(on-redraw render)
	     (on-tick 1 (lambda (w) w))
             (on-location-change update-location))