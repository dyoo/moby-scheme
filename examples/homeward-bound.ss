#lang s-exp "../moby-lang.ss"
;; Homeward bound.
;;
;; A program to keep someone notified of your current location.
;;
;; Every time the position changes to and from a place, an SMS message
;; is sent.

(define HOMEWARD-BOUND-URL
  (string-append "http://maps.google.com/maps/ms?ie=UTF8&hl=en&msa=0&output=georss&"
                 "msid=106933521686950086948.000473bafba93dfb155a0"))


;; A loc is a lat/long pair representing a location.
(define-struct loc (lat long))

;; A place is centered on a location and extends 
;; to a radius measured in meters.
(define-struct place (name loc radius))

;; mile->meter: number -> number
;; Converts miles to meters.
(define (mile->meter miles)
  (* miles 1609.344))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Here are a few places of interest.
;; 100 Institute Rd, in a radius of 500 meters
(define WPI-PLACE 
  (make-place "WPI" (make-loc 42.272824 -71.808207) 500))
;; Parking place.
(define WPI-PARKING-PLACE 
  (make-place "WPI Parking" (make-loc 42.2737222 -71.8058627) 50))
;; Worcester, in a radius of 3 miles.
(define WORCESTER-PLACE 
  (make-place "Worcester" (make-loc 42.274514 -71.798744) (mile->meter 3)))
(define NORTHEASTERN-PLACE
  (make-place "NEU" (make-loc 42.340065 -71.088881) (mile->meter 3)))

;; This is a list of the places.
(define ALL-PLACES
  (list WPI-PLACE
        WPI-PARKING-PLACE
        WORCESTER-PLACE
        NORTHEASTERN-PLACE))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;; The telephone number to send messages to.
(define ADDRESS "5554")
  
;; The world is the current location.
(define initial-world (make-loc 0 0))

;; loc->string: loc -> string
(define (loc->string w)
  (string-append "(" 
		 (number->string (loc-lat w))
		 ", "
		 (number->string (loc-long w))
		 ")"))

;; change-location: world number number -> world
(define (change-location w lat long)
  (make-loc lat long))


;; current-place: world -> place
;; Returns the closest place.
(define (current-place w)
  (cond [(empty? (find-places ALL-PLACES w))
         (make-place "Unknown" w 0)]
        [else
         (choose-smallest
          (find-places ALL-PLACES w))]))

;; send-report: world -> effect
;; Sends out a text message of the world description,
;; and produces the world.
(define (send-report w)
  (make-effect:send-sms ADDRESS 
                        (string-append (description w) 
                                       "\n" 
                                       (maps-url (place-loc (current-place w))))))

;; maps-url: loc -> string
;; Creates the Google maps url for a location.
(define (maps-url a-loc)
  (string-append "http://maps.google.com/maps?q="
                 (number->string 
                  (exact->inexact (loc-lat a-loc)))
                 ",+"
                 (number->string 
                  (exact->inexact (loc-long a-loc)))
                 "&iwloc=A&hl=en"))
  
;; description: world -> string
;; Produces a text description of the current place.
(define (description w)
  (place-name (current-place w)))


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


;; draw: world -> DOM-sexp
(define (draw w)
  (list (js-div)
        (list (js-p '(("id" "aPara")))
	      (list (js-text (description w)))
	      (list (js-text " "))
	      (list (js-text (loc->string w))))))
        


;; draw-css: world -> CSS-sexp
(define (draw-css w)
  '(("aPara" ("font-size" "30px"))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; 


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
               
                (cond
                  [(empty? (sxml-find-children 'description (sxml-children xexpr)))
                   100]
                  [else
                   (string->number 
                    (apply string-append
                           (map get-description-text 
                                (sxml-children (first (sxml-find-children 'description (sxml-children xexpr)))))))]))))


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


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define tick-delay (* 5 60))  ;; wait every five minutes before updates.

(js-big-bang initial-world
             '()
             (on-draw draw draw-css)
             (on-tick* tick-delay identity send-report)
             (on-location-change change-location))