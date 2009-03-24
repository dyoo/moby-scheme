#lang scheme
;; Parses out the places.
;; See: http://mapki.com/wiki/Google_Map_Parameters

(require (lib "net.ss" "moby" "stub"))
(require (lib "parser.ss" "moby" "stub"))
(require (lib "gui-world.ss" "gui-world"))

;; A location is the latitude/longitude pair.
(define-struct loc (lat long) #:transparent)

;; A place is a name string, a location, and a radius number.
(define-struct place (name loc radius) #:transparent)

;; The world is the URL for
(define-struct world (url places out-of-sync?) #:transparent)


;; make-mymaps-url: string -> string
;; Given the unique Google Maps "My Maps" identifier, returns a url
;; to get at its RSS feed.
(define (make-mymaps-url msid)
  (string-append "http://maps.google.com/maps/ms?ie=UTF8&hl=en&vps=1&jsv=151e&msa=0&output=georss&msid="
                 msid))

;; The initial world is one for My Saved Places.
(define initial-world 
  (make-world (make-mymaps-url "106933521686950086948.00046579f4b482756abc5")
              (list)
              true))




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


;; parse-item: xexpr -> list
(define (parse-item xexpr)
  (list (first (find-children 'title (children xexpr)))
        (first (find-children 'georss:point (children xexpr)))))



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