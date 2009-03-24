;; The first three lines of this file were inserted by DrScheme. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname grocery-shopper) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ())))
;; Parses out the places.
;; See: http://mapki.com/wiki/Google_Map_Parameters

(require (lib "net.ss" "moby" "stub"))
(require (lib "parser.ss" "moby" "stub"))
(require (lib "location.ss" "moby" "stub"))
(require (lib "world.ss" "moby" "stub"))


;; A location is the latitude/longitude pair.
(define-struct loc (lat long))

;; A place is a name string, a location, and a radius number.
(define-struct place (name loc radius))

;; The world is current location.
;; The initial world is one for dyoo's My Saved Places.
(define initial-world 
  (make-loc 0 0))


;; update-location: world number number -> world
;; Updates the current location.
(define (update-location w lat long)
  (make-loc lat long))


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


;; The url to the Google Maps "My Maps" RSS feed.
(define mymaps-url
  (string-append "http://maps.google.com/maps/ms?ie=UTF8&hl=en&vps=1&jsv=151e&msa=0&output=georss&msid="
                 "106933521686950086948.00046579f4b482756abc5"))

(define PLACES
  (parse-places (parse-xml (get-url mymaps-url))))