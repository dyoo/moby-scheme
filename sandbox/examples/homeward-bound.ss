#lang s-exp "../moby-lang.ss"
;; Homeward bound.
;;
;; A program to keep someone notified of your current
;; location.
;;
;; Every time the position changes, an SMS message
;; is sent.

(define MYMAPS-URL
  (string-append "http://maps.google.com/maps/ms"
                 "?ie=UTF8&hl=en&msa=0&output=georss&"
                 "msid=106933521686950086948"
                 ".000473bafba93dfb155a0"))

(define UNINITIALIZED 'uninitialized)


;; The world consists of the coordinates and the
;; current closest Place.
;; last-reported is either UNINITIALIZED or the last
;; reported place.
(define-struct world (loc closest last-reported sms enabled?))


;; A loc is a lat/long pair representing a location.
(define-struct loc (lat long))

;; A Place is either:
;;     UNINITIALIZED,
;;     (make-place "Unknown" a-loc a-radius), or
;;     (make-place name a-loc a-radius)

;; A place is centered on a location and extends 
;; to a radius measured in meters.
(define-struct place (name loc radius))



;; place-uninitialized?: Place -> boolean
(define (place-uninitialized? a-place)
  (eq? a-place UNINITIALIZED))


;; place-unknown?: Place -> boolean
;; Returns true if the place is unknown
(define (place-unknown? a-place)
  (cond [(place-uninitialized? a-place)
         false]
        [else
         (string=? (place-name a-place) "Unknown")]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;; The world is the current location.
(define initial-world (make-world (make-loc 0 0)
                                  UNINITIALIZED
                                  UNINITIALIZED
                                  ""
                                  false))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; move: world number number -> world
;; On movement, update to the closest place.
(define (move w lat long)
  (make-world (make-loc lat long)
              (closest-place (make-loc lat long))
              (world-last-reported w)
              (world-sms w)
              (world-enabled? w)))

;; record-reporting: world -> world
;; If we're about to send a report, record that
;; knowledge so we don't repeat a report several times.
(define (record-reporting w)
  (cond
    [(should-send-report? w)
     (make-world (world-loc w)
                 (world-closest w)
                 (world-closest w)
                 (world-sms w)
                 (world-enabled? w))]
    [else
     w]))

;; send-report: world -> effect
;; Sends out a text message of the world
;; description to the sms address in the world.
(define (send-report w)
  (cond [(should-send-report? w)
         (list (make-effect:send-sms 
                (world-sms w)
                (string-append (description w) 
                               "\n" 
                               (maps-url 
                                (world-loc w))))
               (make-effect:beep))]
        [else
         '()]))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; closest-place: loc -> Place
;; Returns the closest place to the given location.
(define (closest-place loc)
  (cond [(empty? (find-places ALL-PLACES loc))
         (make-place "Unknown" loc 0)]
        [else
         (choose-smallest
          (find-places ALL-PLACES loc))]))

;; should-send-report?: world -> boolean
;; We'll send an SMS notification out if the 
;; sms number is registered and we've moved from
;; one place to another.
(define (should-send-report? w)
  (and (not (string-whitespace? (world-sms w)))
       (world-enabled? w)
       (not (eq? (world-closest w) UNINITIALIZED))
       (or (eq? (world-last-reported w) UNINITIALIZED)
           (place-has-transitioned? 
            (world-closest w)
            (world-last-reported w)))))


;; place-has-transitioned?: Place Place -> boolean
;; Returns true if the two places should be treated as distinct.
(define (place-has-transitioned? place-1 place-2)
  (cond
    [(and (place-uninitialized? place-1)
          (place-uninitialized? place-2))
     false]
    [(or (place-uninitialized? place-1)
         (place-uninitialized? place-2))
     true]
    [(and (place-unknown? place-1) 
          (place-unknown? place-2))
     false]
    [(and (place-unknown? place-1)
          (not (place-unknown? place-2)))
     true]
    [(and (not (place-unknown? place-1))
          (place-unknown? place-2))
     true]
    [(and (not (place-unknown? place-1)) 
          (not (place-unknown? place-2)))
     (not (string=? (place-name place-1)
                    (place-name place-2)))]))


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
  (cond [(place-uninitialized? (world-closest w))
         "Uninitialized"]
        [else
         (place-name (world-closest w))]))


;; choose-smallest: (listof place) -> place
;; Returns the place with the smallest radius.
(define (choose-smallest places)
  (cond
    [(empty? (rest places))
     (first places)]
    [(< (place-radius (first places)) 
        (place-radius (second places)))
     (choose-smallest 
      (cons (first places) (rest (rest places))))]
    [else
     (choose-smallest (rest places))]))


;; find-places: world loc -> (listof place)
;; Finds places that match the a-loc.
(define (find-places places a-loc)
  (cond
    [(empty? places)
     empty]
    [(place-matches? (first places) a-loc)
     (cons (first places) 
           (find-places (rest places) a-loc))]
    [else
     (find-places (rest places) a-loc)]))


;; place-matches?: Place loc -> boolean
;; Returns true if the place matches the location.
(define (place-matches? a-place a-loc)
  (cond [(place-uninitialized? a-place)
         false]
        [else
         (<= (location-distance (loc-lat a-loc)
                                (loc-long a-loc)
                                (loc-lat (place-loc a-place))
                                (loc-long (place-loc a-place)))
             (place-radius a-place))]))


;; loc->string: loc -> string
(define (loc->string w)
  (string-append "(" 
                 (number->string (loc-lat w))
                 ", "
                 (number->string (loc-long w))
                 ")"))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; draw: world -> DOM-sexp
(define (draw w)
  (list (js-div '(("id" "main")))
        (list (js-p '(("id" "aPara")))
              (list (js-text "Currently at: "))
              (list (js-text 
                     (description w)))
              (list (js-text " "))
              (list (js-text 
                     (loc->string (world-loc w)))))
        (list (js-div) 
              (list (js-text "Notify SMS #"))
              (list sms-input-dom)
              (list (js-button world-enable)
                    (list (js-text "Use this number"))))
        (list (js-p '(("id" "anotherPara")))
              (list (js-text 
                     (cond 
                       [(world-enabled? w)
                        (format 
                         "~a will be used for notification." (world-sms w))]
                       [else
                        "SMS Number has not been assigned"]))))
        
        (list (js-p '(("id" "lastPara")))
              (list (js-text 
                     (cond [(place-uninitialized?
                             (world-last-reported w))
                            "No notification has been sent yet."]
                           [(place-unknown? 
                             (world-last-reported w))
                            (format "Notification was last sent at ~s ~a."
                                    (place-name (world-last-reported w))
                                    (loc->string (place-loc (world-last-reported w))))]
                           [else
                            (format "Notification was last sent at ~s."
                                    (place-name (world-last-reported w)))]))))))

;; draw-css: world -> CSS-sexp
(define (draw-css w)
  '(("aPara" ("font-size" "30px"))
    ("anotherPara" ("font-size" "25px"))
    ("main" ("border-style" "solid"))))



;; update-world-sms: world -> world
;; Update the world with the value of the sms field.
(define (update-world-sms w sms)
  (make-world (world-loc w)
              (world-closest w)
              (world-last-reported w)
              sms
              (world-enabled? w)))


(define sms-input-dom
  (js-input "text" update-world-sms '(("id" "sms-input"))))



;; world-enable: world -> world
;; Allow the program to start sending sms messages.
(define (world-enable w)
  (make-world (world-loc w)
              (world-closest w)
              (world-last-reported w)
              (world-sms w)
              true))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; RSS Parser Helpers.

;; parse-places: xexpr -> (listof place)
;; Parses out the places from the RSS we get from Google Maps.
(define (parse-places xexpr)
  (cond
    [(or (empty? xexpr)
         (and (string? xexpr)
              (string=? xexpr "")))
     empty]
    [else
     (parse-items
      (sxml-find-children
       'item 
       (sxml-children 
        (first (sxml-find-children
                'channel 
                (sxml-children xexpr))))))]))


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
               (sxml-text (xml->s-exp (string-append "<top>" x "</top>")))]))]
    
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
                   (loosely-parse-number
                    (apply string-append
                           (map get-description-text 
                                (sxml-children (first (sxml-find-children 'description (sxml-children xexpr)))))))]))))


;; loosely-parse-number: string -> number
(define (loosely-parse-number an-str)
  (cond [(empty? (split-whitespace an-str))
         0]
        [(number? (string->number (first (split-whitespace an-str))))
         (string->number (first (split-whitespace an-str)))]
        [else
         0]))




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


;; split-whitespace: string -> (listof string)
(define (split-whitespace str)
  (local [(define (splitter letters-so-far source)
            (cond
              [(empty? source)
               (cond [(empty? letters-so-far)
                      empty]
                     [else
                      (list (list->string (reverse letters-so-far)))])]

              [(char-whitespace? (first source))
               (cond
                 [(empty? letters-so-far)
                  (splitter empty (rest source))]
                 [else
                  (cons (list->string (reverse letters-so-far))
                        (splitter empty (rest source)))])]

              [else
               (splitter (cons (first source) letters-so-far)
                         (rest source))]))]
    (splitter empty (string->list str))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define ALL-PLACES
  (parse-places 
   (xml->s-exp (get-url MYMAPS-URL))))


(define TICK-DELAY (* 5 60))  ;; wait every five minute before updates.

(js-big-bang initial-world
             
             (on-location-change move)
             (on-tick! TICK-DELAY
                       record-reporting send-report)
             
             (on-draw draw draw-css))