;; The first three lines of this file were inserted by DrScheme. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname grocery-shopper) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ())))
;; Parses out the places.
;; See: http://mapki.com/wiki/Google_Map_Parameters

(require (lib "net.ss" "moby" "stub"))
(require (lib "parser.ss" "moby" "stub"))
(require (lib "location.ss" "moby" "stub"))
(require (lib "gui-world.ss" "gui-world"))


;; A location is the latitude/longitude pair.
(define-struct loc (lat long))

;; A place is a name string, a location, and a radius number.
(define-struct place (name loc radius))

;; The world is the URL to grab places from, a list of the loaded places,
;; a boolean if we're out of sync with the url, and our current location.
(define-struct world (url places out-of-sync? loc))

;; make-mymaps-url: string -> string
;; Given the unique Google Maps "My Maps" identifier, returns a url
;; to get at its RSS feed.
(define (make-mymaps-url msid)
  (string-append "http://maps.google.com/maps/ms?ie=UTF8&hl=en&vps=1&jsv=151e&msa=0&output=georss&msid="
                 msid))

;; The initial world is one for dyoo's My Saved Places.
(define initial-world 
  (make-world (make-mymaps-url "106933521686950086948.00046579f4b482756abc5")
              (list)
              true
              (make-loc 0 0)))

;; update-location: world number number -> world
;; Updates the current location.
(define (update-location w lat long)
  (make-world (world-url w)
              (world-places w)
              (world-out-of-sync? w)
              (make-loc lat long)))

;; update-url: world string -> world
;; Updates the url and marks us as out-of-sync.
(define (update-url w url)
  (make-world url
              (world-places w)
              true
              (world-loc w)))

;; reload: world -> world
;; Reparses the places.
(define (reload w)
  (make-world (world-url w)
              (parse-places (parse-xml (get-url (world-url w))))
              false
              (world-loc w)))



;; choose-smallest: (listof place) -> place
;; Returns the place with the smallest radius.
(define (choose-smallest places)
  (cond
    [(empty? (rest places))
     (first places)]
    [(< (place-radius (first places)) (place-radius (second places)))
     (choose-smallest (cons (first places) (rest (rest places))))]
    [else
     (choose-smallest (rest places))]))

;; find-places: world loc -> (listof place)
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










;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; parse-places: xexpr -> (listof place)
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
  ;; FIXME: wrong!  I need to return a place!
  (make-place (get-text (first (find-children 'title (children xexpr))))              
              (parse-georss:point (first (find-children 'georss:point (children xexpr))))
              ;; fixme!  We need the radius!
              ;; At the moment, we default to a radius of fifty meters.
              50))


;; parse-georss:point: xexpr -> loc
(define (parse-georss:point xexpr)
  (make-loc (second (split-whitespace (get-text xexpr)))
            (third (split-whitespace (get-text xexpr)))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
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

;; place->string: place -> string
(define (place->string a-place)
  (string-append (place-name a-place) 
                 " "
                 (loc->string (place-loc a-place))
                 " "
                 (number->string (place-radius a-place))))

;; loc->string: loc -> string
(define (loc->string a-loc)
  (string-append "(" (loc-lat a-loc) ", " (loc-long a-loc) ")"))
                 
;; places->string: (listof place) -> string
(define (places->string places)
  (cond
    [(empty? places)
     ""]
    [else
     (string-append (place->string (first places))
                    ", "
                    (places->string (rest places)))]))
         
;; description: world -> string
(define (description a-world)
  (cond [(world-out-of-sync? a-world)
         "Out of sync"]
        [else
         (places->string (world-places a-world))]))


(define view 
  (col (message description)
       (button "Load" reload)))


(big-bang initial-world view)