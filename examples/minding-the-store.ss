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



;; The world is current location, the places that are nearby that location, and a description
;; of the items we've found at that place.
(define-struct world (loc nearby-places))

;; A location is the latitude/longitude pair.
(define-struct loc (lat long))

;; Our initial world will be in limbo; we'll be relocated as soon as we get a geolocation point.
(define initial-world 
  (make-world (make-loc 0 0) empty))



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
  (cond
    [(empty? places)
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
;; Parses an item from the RSS feed.
(define (parse-item xexpr)
  (make-place (get-text (first (find-children 'title (children xexpr))))
              
              (cond
                [(empty? (find-children 'georss:point (children xexpr)))
                 (make-loc 0 0)]
                [else
                 (parse-georss:point 
                  (first (find-children 'georss:point (children xexpr))))])
              
              ;; At the moment, we default to a radius of 100 meters.
              100
              
              
              (cond
                [(empty? (find-children 'description (children xexpr)))
                 ""]
                [else
                 (first (find-children 'description (children xexpr)))])))


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
     (get-text* (children an-xexpr))]
    [(empty? an-xexpr)
     ""]))

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
(define MYMAPS-URL
  (string-append "http://maps.google.com/maps/ms?ie=UTF8&hl=en&vps=1&jsv=151e&msa=0&output=georss&msid="
                 "106933521686950086948.00046579f4b482756abc5"))

(define ALL-PLACES
  (parse-places 
   (parse-xml (get-url MYMAPS-URL))))


(js-big-bang ALL-PLACES #;initial-world
             #;(on-redraw render)
             #;(on-location-change update-location))