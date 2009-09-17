#lang s-exp "../moby-lang.ss"
;; a node is (make-node string string location integer (listof node) (listof number) boolean)
;(define-struct node (names info loc tours index neighbors distances heap-node))

(define-struct info (name description))

(define-struct location (lat lng radius))

(define-struct node (info index tours loc)) 

(define library-tour "A tour of libraries")
(define food-tour "A tour of food")


(define n7 (make-node (make-info "7" "seven") 7 (list library-tour food-tour) (make-location 41.826220 -71.400976 2)))
(define n6 (make-node (make-info "6" "six") 6 (list food-tour) (make-location 41.826422 -71.400995 2)))
(define n5 (make-node (make-info "5" "five") 5 (list food-tour) (make-location 41.826392 -71.401279 2)))
(define n4 (make-node (make-info "4" "four") 4 empty (make-location 41.826228 -71.401354 2)))
(define n1 (make-node (make-info "1" "one") 1 empty (make-location 41.826192 -71.401874 2)))
(define n2 (make-node (make-info "2" "two") 2 (list library-tour) (make-location 41.826366 -71.401896 2)))
(define n3 (make-node (make-info "3" "three") 3 (list food-tour) (make-location 41.826382 -71.401673 2)))
(define n0 (make-node (make-info "0" "zero") 0 (list library-tour) (make-location 41.826194 -71.401654 2)))
(define treeGraph (list n0 n1 n2 n3 n4 n5 n6 n7))



(define (pretend-dijkstra graph node)
  (cond 
    [(eq? node n0) (list false (list 4 0) (list 10 1) (list 7 0) (list 6 0) (list 11 4) (list 15 5) (list 10 4))]
    [(eq? node n1) (list (list 4 1) false (list 6 1) (list 9 2) (list 10 0) (list 15 4) (list 16 2) (list 14 4))]
    [(eq? node n2) (list (list 10 1) (list 6 2) false (list 3 2) (list 16 0) (list 14 6) (list 10 2) (list 18 6))]
    [(eq? node n3) (list (list 7 3) (list 9 2) (list 3 3) false (list 13 0) (list 17 6) (list 13 2) (list 17 4))]
    [(eq? node n4) (list (list 6 4) (list 10 0) (list 16 1) (list 13 0) false (list 5 4) (list 9 5) (list 4 4))]
    [(eq? node n5) (list (list 11 4) (list 17 0) (list 14 6) (list 17 2) (list 5 5) false (list 4 5) (list 9 4))]
    [(eq? node n6) (list (list 15 4) (list 16 2) (list 10 6) (list 13 2) (list 9 5) (list 4 6) false (list 8 6))]
    [(eq? node n7) (list (list 10 4) (list 14 0) (list 18 6) (list 17 0) (list 4 7) (list 9 4) (list 8 7) false)]))

(define-struct world (lat long tours visited visiting? path tovisit in-menu?))

(define init-world (make-world 0 0 empty empty false empty empty true))

(define (reach-node lat long path acc) 
  (cond
    [(empty? path) acc]
    [(< (distance lat long
                  (location-lat (node-loc (first path))) (location-lng (node-loc (first path))))
        (location-radius (node-loc (first path))))
     (if (empty? (rest path)) 
         (first path)
         (rest path))]
    [else (reach-node lat long (rest path) (append acc (list (first path))))]))

(define (location-change-handler world lat long)
  (local
    [(define new-path (reach-node lat long (world-path world) empty))]
    (cond
      [(or (cons? new-path) (empty? new-path))
       (make-world lat long (world-tours world) (world-visited world) (world-visiting? world) new-path (world-tovisit world) (world-in-menu? world))]
      [(node? new-path)
       (local
         [(define new-tovisit (remove new-path (world-tovisit world)))]
         (make-world lat long (world-tours world) (cons new-path (world-visited world)) true (gen-path new-path new-tovisit) new-tovisit (world-in-menu? world)))])))

;

;make path to next stop in tovisit by calling dijkstra
(define (gen-path node unvisited)
  (if (member node unvisited)
      (list node)
      (local
        [(define output (pretend-dijkstra treeGraph node))
         (define endpoint (getShortestSigNode output unvisited -1 5000000 0))]
        (gen-path-helper endpoint output empty))))

(define (getShortestSigNode distances unvisited bestIndex bestTime acc)
  (cond
    [(empty? distances) (nth treeGraph bestIndex)]
    [(and (cons? (first distances)) (member (nth treeGraph acc) unvisited) (or (= -1 bestIndex) (< (first (first distances)) bestTime)))
     (getShortestSigNode (rest distances) unvisited acc (first (first distances)) (add1 acc))]
    [else (getShortestSigNode (rest distances) unvisited bestIndex bestTime (add1 acc))]))

(define (gen-path-helper end out ret)
  (if (false? (nth out (node-index end)))
      ret
      (gen-path-helper (nth treeGraph (second (nth out (node-index end)))) out (cons end ret))))


(define (nth lst n)
  (if (= n 0)
      (first lst)
      (nth (rest lst) (- n 1))))

;; distance: num num num num -> number
;; Given two places on a globe, return the shortest distance between them in meters (uses spherical geometry)
(define (distance latA lonA latB lonB)
  (* 6378000
     (* 2
        (asin (min 1
                   (sqrt (+ (expt (sin (/ (- (deg->rad latA) (deg->rad latB)) 2)) 2)
                            (* (cos (deg->rad latA))                
                               (cos (deg->rad latB))
                               (expt (sin (/ (- (deg->rad lonA) (deg->rad lonB)) 2)) 2)))))))))

(define (deg->rad angle)
  (* angle (/ pi 180)))

;empty -> shortest distance
;if in radius, -> that node
(define (get-nearest-node lat long)
  (second (argmin car (map (lambda (x) (list (distance (location-lat (node-loc x)) (location-lng (node-loc x)) lat long) x)) treeGraph))))

;button functions
(define (tomenu world)
  (make-world (world-lat world) (world-long world) (world-tours world) (world-visited world) (world-visiting? world) (world-path world) (world-tovisit world) true))

(define (moveon world)
  (make-world (world-lat world) (world-long world) (world-tours world) (world-visited world) false (world-path world) (world-tovisit world) (world-in-menu? world)))

(define (redopath world)
  (local 
    [(define nearest (get-nearest-node (world-lat world) (world-long world)))]
    (make-world (world-lat world) (world-long world) (world-tours world) (world-visited world) (world-visiting? world) (consIfNotSame nearest (gen-path nearest (world-tovisit world))) (world-tovisit world) (world-in-menu? world))))

(define (init world) init-world)

(define (todirections world)
  (local 
    [(define nearest (get-nearest-node (world-lat world) (world-long world)))]
    (make-world (world-lat world) (world-long world) (world-tours world) (world-visited world) (world-visiting? world) (consIfNotSame nearest (gen-path nearest (world-tovisit world))) (world-tovisit world) false)))

(define (consIfNotSame a aloa)
  (if (eq? a (first aloa)) aloa (cons a aloa)))

(define (addtour aTour world)
  (make-world (world-lat world) 
              (world-long world) 
              (cons aTour (world-tours world)) 
              (world-visited world) 
              (world-visiting? world) 
              (world-path world) 
              (addTourWithout aTour (world-visited world) (world-tovisit world)) 
              (world-in-menu? world)))

(define (addTourWithout aTour alreadyVisited tovisit)
  (append (filter (lambda (aNode) (and (member aTour (node-tours aNode))
                                       (not (member aNode alreadyVisited))
                                       (not (member aNode tovisit))))
                  treeGraph) tovisit))

(define (removetour aTour world)
  (local [(define newTours (remove aTour (world-tours world)))]
    (make-world (world-lat world) 
                (world-long world) 
                newTours 
                (world-visited world)
                (world-visiting? world)
                (world-path world) 
                (removeObsoleteTours newTours (world-tovisit world)) 
                (world-in-menu? world))))

(define (removeObsoleteTours tours tovisit)
  (filter (lambda (aNode) (foldl (lambda (n al) (or (member n tours) al))
                                 false
                                 (node-tours aNode)))
          tovisit))

(define (testMove aWorld)
  (if (empty? (world-path aWorld))
      aWorld
      (location-change-handler aWorld (+ (world-lat aWorld) (* 0.75 (- (location-lat (node-loc (first (world-path aWorld)))) (world-lat aWorld))))
                               (+ (world-long aWorld) (* 0.75 (- (location-lng (node-loc (first (world-path aWorld)))) (world-long aWorld)))))))



;buttons:

(define menu-button (list (js-button tomenu (list (list "id" "aButton")))
                          (list (js-p (list (list "id" "aPara")))
                                (list (js-text "Menu")))))
(define move-on-button (list (js-button moveon (list (list "id" "aButton")))
                             (list (js-p (list (list "id" "aPara")))
                                   (list (js-text "Next Tour Stop")))))
(define redo-path-button (list (js-button redopath (list (list "id" "aButton")))
                               (list (js-p (list (list "id" "aPara")))
                                     (list (js-text "Recalculate Path")))))
(define reset-button (list (js-button init (list (list "id" "aButton")))
                           (list (js-p (list (list "id" "aPara")))
                                 (list (js-text "Reset")))))
(define ok-button (list (js-button todirections (list (list "id" "aButton")))
                        (list (js-p (list (list "id" "aPara")))
                              (list (js-text "OK")))))
(define (make-add-button tour)
  (list (js-button (lambda (aWorld) (addtour tour aWorld))
                   (list (list "id" "aButton")))
        (list (js-p (list (list "id" "aPara")))
              (list (js-text "Add")))))

(define (make-remove-button tour)
  (list (js-button (lambda (aWorld) (removetour tour aWorld)) 
                   (list (list "id" "aButton")))
        (list (js-p (list (list "id" "aPara")))
              (list (js-text "Remove")))))

(define testMove-button (list (js-button testMove (list (list "id" "aButton")))
                              (list (js-p (list (list "id" "aPara")))
                                    (list (js-text "move")))))

;; draw-css: world -> (sexpof css)
(define (draw-css w)
  (list (list "aButton"
              (list "background-color" "lightblue"))
        (list "aPara"
              (list "font-size" "16pt"))))

(define (draw world)
  (cond
    [(world-in-menu? world)
     (list (js-div)
           testMove-button
           (list (js-p (list (list "id" "aPara")))
                 (list (js-text "Please select at least one tour and then click the OK button")))
           reset-button
           (list (js-p (list (list "id" "aPara")))
                 (list (js-text library-tour)))
           (if (member library-tour (world-tours world))
               (make-remove-button library-tour)
               (make-add-button library-tour))
           (list (js-p (list (list "id" "aPara")))
                 (list (js-text food-tour)))
           (if (member food-tour (world-tours world))
               (make-remove-button food-tour)
               (make-add-button food-tour))
           (list (js-p (list (list "id" "aPara")))
                 (list (js-text "sizeOfPath: "))
                 (list (js-text (number->string (length (world-path world))))))
           (list (js-p (list (list "id" "aPara")))
                 (list (js-text "sizeOftoVisit: "))
                 (list (js-text (number->string (length (world-tovisit world))))))
           ok-button)]
    [(world-visiting? world)
     (list (js-div)
           testMove-button
           move-on-button
           (list (js-p (list (list "id" "aPara")))
                 (list (js-text "---"))
                 (list (js-text (info-name (node-info (first (world-visited world))))))
                 (list (js-text "---")))
           (list (js-p (list (list "id" "aPara")))
                 (list (js-text (info-description (node-info (first (world-visited world))))))))]
    [(empty? (world-tovisit world))
     (list (js-div)
           testMove-button
           (list (js-p (list (list "id" "aPara")))
                 (list (js-text "Thank you for touring Brown university")))
           menu-button)]
    [else
     (list (js-div)
           testMove-button
           menu-button
           (list (js-p (list (list "id" "aPara")))
                 (list (js-text "Go to: "))
                 (list (js-text (path->string (world-path world) (world-lat world) (world-long world) ))))
           redo-path-button)]))


(define (path->string alon lat long)
  (cond
    [(empty? alon) 
     ""]
    [(cons? alon) 
     (string-append (info-name (node-info (first alon))) 
                    " \n" 
                    (get-dir-string lat 
                                    long 
                                    (location-lat (node-loc (first alon))) 
                                    (location-lng (node-loc (first alon)))))]))

    
(define (get-dir-string latA lonA latB lonB)
  (local
    [(define angle (direction latA lonA latB lonB))]
    (cond
      [(or (< (/ pi 8) angle) (> (* 1.875 pi) angle)) "North"]
      [(< (* 3 (/ pi 8)) angle) "North-East"]
      [(< (* 5 (/ pi 8)) angle) "East"]
      [(< (* 7 (/ pi 8)) angle) "South-East"]
      [(< (* 9 (/ pi 8)) angle ) "South"]
      [(< (* 11 (/ pi 8)) angle ) "South-West"]
      [(< (* 13 (/ pi 8)) angle ) "West"]
      [(< (* 15 (/ pi 8)) angle ) "North-West"])))


; mod: number(a) number(b) -> number
;; returns mod_b(a) (will be [0, b) even if a is negative -- b must be positive)
(define (mod a b)
  (cond
    [(< a 0) (mod (+ a b) b)]
    [(< a b) a]
    [else (mod (- a b) b)]))

;; direction: num num num num -> number
;; Given two places on a globe, return the bearing of the shortest distance between them in meters (uses spherical geometry)
(define (direction latA lonA latB lonB)
  (cond
    [(= (cos (deg->rad latA)) 0) (if (latA > 0) pi (* 2 pi))]
    [(and (= latA latB) (= lonA lonB)) -1]
    [else (mod
           (atan2 (* (sin (- (deg->rad lonB)
                             (deg->rad lonA)))
                     (cos (deg->rad latB)))
                  (- (* (cos (deg->rad latA))
                        (sin (deg->rad latB)))
                     (* (sin (deg->rad latA))
                        (cos (deg->rad latB))
                        (cos (- (deg->rad lonB)
                                (deg->rad lonA))))))
           (* 2 pi))]))



(define (atan2 y x)
  (if (= y 0)
      (cond
        [(= x 0) (error 'atan2 "undefined for arguments: 0 0")]
        [(> x 0) 0]
        [(< x 0) pi])
      (cond
        [(> x 0) (* (sgn y) (atan (abs (/ y x))))]
        [(= x 0) (* (sgn y) pi 0.5)]
        [(< x 0) (* (sgn y) (- pi (atan (abs (/ y x)))))])))


(js-big-bang (location-change-handler init-world 41.825721 -71.41478)
             (on-draw draw draw-css)
             (on-location-change location-change-handler))