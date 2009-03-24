;; The first three lines of this file were inserted by DrScheme. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname get-ip-address) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ())))
;; Shows the current ip address.

(require (lib "net.ss" "moby" "stub"))
(require (lib "parser.ss" "moby" "stub"))
(require (lib "gui-world.ss" "gui-world"))


;; The world is a string.
(define initial-world "unknown")

;; description: world -> string
(define (description w)
  w)


;; refresh: world -> world
(define (refresh w)
  (extract-ip-address
   (parse-xml
    (get-url "http://ip.hashcollision.org/"))))


;; extract-ip-address: xexpr -> string
;; Returns the ip address in the xexpr, or "" if it can't find it.
;; We know the ip address is in the p field.
(define (extract-ip-address an-xexpr)
  (cond
    [(string? an-xexpr) 
     ""]
    [(and (pair? an-xexpr)
          (equal? (first an-xexpr) 'p))
     (third an-xexpr)]
    [else
     (extract-ip-address/children (rest (rest an-xexpr)))]))


;; extract-ip-address/children: (listof xexpr) -> string
;; Returns the ip address found in one of the xs's, or "" otherwise.
(define (extract-ip-address/children xs)
  (cond
    [(empty? xs)
     ""]
    [(string=? (extract-ip-address (first xs)) "")
     (extract-ip-address/children (rest xs))]
    [else
     (extract-ip-address (first xs))]))


(define view
  (col
   (message description)
   (button "Refresh!" refresh)))

(big-bang initial-world view)