#lang s-exp "../moby-lang.ss"
;; a node is (make-node string string location integer (listof node) (listof number) boolean)
;(define-struct node (names info loc tours index neighbors distances heap-node))

(define-struct info (name description))

(define-struct location (lat lng radius))

(define-struct node (info index tours loc))

(define-struct world 
  (lat long tours visited visiting? path tovisit in-menu?))

(define init-world 
  (make-world 0 0 empty empty false empty empty true))


(define library-tour "A tour of libraries")
(define food-tour "A tour of food")

(define n1 (make-node (make-info "SCILI" "The front of the scili") 1 (list library-tour) (make-location 41.82680 -71.400210 11)))
(define n2 (make-node (make-info "CIT" "the main entrance to the CIT above the stairs") 2 (list library-tour food-tour) (make-location 41.826640 -71.399840 11)))
(define n0 (make-node (make-info "FIELD" "the middle of a field near MacMillan") 0 (list library-tour) (make-location 41.82640 -71.39982 11)))
(define n3 (make-node (make-info "1 1/2" "A sculpture in the middle of the courtyard") 3 (list food-tour) (make-location 41.826595 -71.400068 11)))
(define treeGraph (list n0 n1 n2 n3))




(define (pretend-dijkstra graph node)
  (cond
    [(eq? node n0) (list false (list 2.5 0) (list 2 0) (list 1.5 0))]
    [(eq? node n1) (list (list 2.5 1) false (list 3 3) (list 1.5 1))]
    [(eq? node n2) (list (list 2 2) (list 3 3) false (list 1.5 2))]
    [(eq? node n3) (list (list 1.5 3) (list 1.5 3) (list 1.5 3) false)]))


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
  (cond
    [(and (number? lat) (number? long)) (local
    [(define new-path (reach-node lat long (world-path world) empty))]
    (cond
      [(or (cons? new-path) (empty? new-path))
       (make-world lat long (world-tours world) (world-visited world) (world-visiting? world) new-path (world-tovisit world) (world-in-menu? world))]
      [(node? new-path)  ;; we've reached a significant node
       (local
         [(define new-tovisit (remove new-path (world-tovisit world)))]
         (if (empty? new-tovisit) (make-world lat long (world-tours world) (cons new-path (world-visited world)) true empty new-tovisit (world-in-menu? world))
             (make-world lat long (world-tours world) (cons new-path (world-visited world)) true (gen-path new-path new-tovisit) new-tovisit (world-in-menu? world))))]))]
    [else world]))

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
      (nth (cdr lst) (- n 1))))

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
  (make-world (world-lat world) (world-long world) (cons aTour (world-tours world)) (world-visited world) (world-visiting? world) (world-path world) (addTourWithout aTour (world-visited world) (world-tovisit world)) (world-in-menu? world)))

(define (addTourWithout aTour alreadyVisited tovisit)
  (append (filter (lambda (aNode) (and (member aTour (node-tours aNode))
                                       (not (member aNode alreadyVisited))
                                       (not (member aNode tovisit))))
                  treeGraph) tovisit))

(define (removetour aTour world)
  (local [(define newTours (remove aTour (world-tours world)))]
    (make-world (world-lat world) (world-long world) newTours (world-visited world) (world-visiting? world) (world-path world) (removeObsoleteTours newTours (world-tovisit world)) (world-in-menu? world))))

(define (removeObsoleteTours tours tovisit)
  (filter (lambda (aNode) (foldl (lambda (n al) (or (member n tours) al))
                                 false
                                 (node-tours aNode)))
          tovisit))


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
  (list (js-button (lambda (aWorld) (addtour tour aWorld)) (list (list "id" "aButton")))
        (list (js-p (list (list "id" "aPara")))
              (list (js-text "Add")))))
(define (make-remove-button tour)
  (list (js-button (lambda (aWorld) (removetour tour aWorld)) (list (list "id" "aButton")))
        (list (js-p (list (list "id" "aPara")))
              (list (js-text "Remove")))))

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
           (list (js-p (list (list "id" "aPara")))
                 (list (js-text "Please select at least one tour and then click the OK button")))
           reset-button
           (if (member library-tour (world-tours world))
               (make-remove-button library-tour)
               (make-add-button library-tour))
           (list (js-p (list (list "id" "aPara")))
                 (list (js-text library-tour)))
           (if (member food-tour (world-tours world))
               (make-remove-button food-tour)
               (make-add-button food-tour))
           (list (js-p (list (list "id" "aPara")))
                 (list (js-text food-tour)))
           (if (empty? (world-tovisit world))
               (list (js-p (list (list "id" "aPara")))
                     (list (js-text "Please select a tour you have not already finished, or press reset.")))
               ok-button)
           (list (js-p (list (list "id" "aPara")))
                 (list (js-text "lat: "))
                 (list (js-text (number->string (world-lat world)))))
           (list (js-p (list (list "id" "aPara")))
                 (list (js-text "lng: "))
                 (list (js-text (number->string (world-long world)))))
           (list (js-p (list (list "id" "aPara")))
                 (list (js-text "sizeOfPath: "))
                 (list (js-text (number->string (length (world-path world))))))
           (list (js-p (list (list "id" "aPara")))
                 (list (js-text "sizeOftoVisit: "))
                 (list (js-text (number->string (length (world-tovisit world))))))
           (list (js-p (list (list "id" "aPara")))
                 (list (js-text "sizeOfVisited: "))
                 (list (js-text (number->string (length (world-visited world)))))))]
    [(world-visiting? world)
     (list (js-div)
           move-on-button
           (list (js-p (list (list "id" "aPara")))
                 (list (js-text "---"))
                 (list (js-text (info-name (node-info (first (world-visited world))))))
                 (list (js-text "---")))
           (list (js-p (list (list "id" "aPara")))
                 (list (js-text (info-description (node-info (first (world-visited world)))))))
           (list (js-p (list (list "id" "aPara")))
                 (list (js-text "lat: "))
                 (list (js-text (number->string (world-lat world)))))
           (list (js-p (list (list "id" "aPara")))
                 (list (js-text "lng: "))
                 (list (js-text (number->string (world-long world))))))]
    [(empty? (world-tovisit world))
     (list (js-div)
           (list (js-p (list (list "id" "aPara")))
                 (list (js-text "Thank you for touring Brown university")))
           (list (js-p (list (list "id" "aPara")))
                 (list (js-text "lat: "))
                 (list (js-text (number->string (world-lat world)))))
           (list (js-p (list (list "id" "aPara")))
                 (list (js-text "lng: "))
                 (list (js-text (number->string (world-long world)))))
           menu-button)]
    [else
     (list (js-div)
           menu-button
           (list (js-p (list (list "id" "aPara")))
                 (list (js-text "Go to: "))
                 (list (js-text (path->string (world-path world) (world-lat world) (world-long world))))
                 (list (js-p (list (list "id" "aPara")))
                       (list (js-text "lat: "))
                       (list (js-text (number->string (world-lat world)))))
                 (list (js-p (list (list "id" "aPara")))
                       (list (js-text "lng: "))
                       (list (js-text (number->string (world-long world)))))
                 (if (not (empty? (world-path world)))
                     (list (js-p (list (list "id" "aPara")))
                           (list (js-text "Distance: "))
                           (list (js-text (number->string (distance (world-lat world) (world-long world) (location-lat (node-loc (first (world-path world)))) (location-lng (node-loc (first (world-path world)))))))))
                     (list (js-p (list (list "id" "aPara")))
                           (list (js-text "No path"))))
                 redo-path-button))]))


(define (path->string alon lat long)
  (cond
    [(empty? alon) ""]
    [(cons? alon) (string-append (info-name (node-info (first alon))) " \n" (get-dir-string lat long (location-lat (node-loc (first alon))) (location-lng (node-loc (first alon))))
                                 )]))

(define (get-dir-string latA lonA latB lonB)
  (local
    [(define angle (direction latA lonA latB lonB))]
    (cond
      [(or (> (/ pi 8) angle) (< (* 1.875 pi) angle)) "North"]
      [(> (* 3 (/ pi 8)) angle) "North-East"]
      [(> (* 5 (/ pi 8)) angle) "East"]
      [(> (* 7 (/ pi 8)) angle) "South-East"]
      [(> (* 9 (/ pi 8)) angle ) "South"]
      [(> (* 11 (/ pi 8)) angle ) "South-West"]
      [(> (* 13 (/ pi 8)) angle ) "West"]
      [(> (* 15 (/ pi 8)) angle ) "North-West"])))




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






;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;; DIJKSTRA'S ALGORITHM


;a node in a heap.
(define-struct heap-node (value size left right parent))


;dijkstra: (listof node) , node -> (vectorof (listpair int node))
;Takes a graph as a list of nodes and one of those nodes to start from.
;Produces a vector of pairs (represented as lists) of total shortest-path distance and previous node. The ith vector entry has the information for the node with index i.
(define (dijkstra graph source)
  (dijkstra-rec 
   (make-heap-rep graph source)
   (make-vector (length graph) (list 'unreachable 'unreachable))))


;make-heap-rep: (listof node) node -> heap
;Takes a graph and a node in it to be the source
;Returns a heap ready to be passed to dijkstra
(define (make-heap-rep graph source)
  (make-heap-node-pointers
   (foldl insert (make-heap-node (list 0 source) 0 false false false)
          (map
           (lambda (x) (make-heap-node (list +inf.0 x) 0 false false false))
           (without source graph)))))

;make-heap-node-pointers: heap -> heap
; sets all the heap-node pointers in the graph representation to the appropriate heap node
(define (make-heap-node-pointers heap)
  (if (equal? false heap) 'nothing
      (begin
;        (set-node-heap-node! (second (heap-node-value heap)) heap)
        (make-heap-node-pointers (heap-node-left heap))
        (make-heap-node-pointers (heap-node-right heap))
        heap)))

;dijkstra-rec: heap , (vectorof (listpair int node))
;recursive helper for dijkstra.
;Takes a heap representation of a graph and a vector of
(define (dijkstra-rec graph prev)
  (if (= (size graph) 0)
      prev
      (let* ([u (find-min graph)]
             [dist (first u)]
             [node (second u)])
        (if (= dist +inf.0)
            prev
            (dijkstra-rec (begin
                            (map (lambda (x y)
                                 (if (< (+ dist y) (first (heap-node-value x)))
                                     (set-key x (+ dist y))
                                     'nothing))
                               (map node-heap-node (node-neighbors node))
                               (node-distances node))
                            (delete-min graph))                        
                          (begin
                            (map (lambda (x)
                                   (vector-set! prev
                                                (node-index (second (heap-node-value x)))
                                                (heap-node-value x)))
                                 (map node-heap-node (node-neighbors node)))
                            prev))))))
                         
            

(define (empty-heap? heap)
 (zero? (heap-node-size heap)))

;find-min: heap -> (listpair int node)
;returns the value of the minimum element in the heap
;as a pair of its key (priority, distance) and the object it represents
(define (find-min heap)
  (heap-node-value heap))

;delete-min: heap -> heap
;removes the minimum element of a heap
(define (delete-min heap)
  (merge (heap-node-left heap) (heap-node-right heap)))

;insert: heapnode , heap -> heap
; returns the heap created by inserting node into heap
(define (insert node heap)
  (begin
    (set-heap-node-size! node 1)
    (set-heap-node-left! node false)
    (set-heap-node-right! node false)
    (set-heap-node-parent! node false)
    (merge node heap)))

;merge: heap heap -> heap
;performs a maxiphobic heap merge
(define (merge h1 h2)
  (cond
    [(false? h1) h2]
    [(false? h2) h1]
    [else  (let*
             ([newsize (+ (size h1) (size h2))]
              [smaller (if (less-than (heap-node-value h1) (heap-node-value h2)) h1 h2)]
              [larger (if (eq? smaller h1) h2 h1)]
              [A (heap-node-left smaller)]
              [B (heap-node-right smaller)]
              [left (if (and (> (size A) (size B))
                             (> (size A) (size larger)))
                        A
                        (if (> (size B) (size larger))
                            B
                            larger))])
           (begin
             (set-heap-node-parent! smaller false)
             (set-heap-node-size! smaller newsize)
             (set-heap-node-left! smaller left)
             (set-heap-node-parent! left smaller)
             (set-heap-node-right! smaller
                                   (let* ([halves (without left (list A B larger))]
                                          [right (merge (first halves) (second halves))])
                                     (begin
                                       (if (not (false? right)) (set-heap-node-parent! right smaller) false)
                                       right)))
             smaller))]))

;set-key: heapnode , int -> heap
;Takes a heapnode and modifies its key
;Returns the heap of which that heapnode is a part, with the heap invariant maintained
(define (set-key node value)
  (begin
    (set-heap-node-value! node (list value (second (heap-node-value node))))
    (cut (heap-node-right node))
    (cut (heap-node-left node))
    (if (not (false? (heap-node-parent node)))
        (insert node
                (merge
                 (merge (heap-node-left node) (heap-node-right node))                 
                 (begin
                   ((if (eq? (heap-node-right (heap-node-parent node)) node)
                        set-heap-node-right!
                        set-heap-node-left!) (heap-node-parent node) false)
                   (find-root (heap-node-parent node)))))
        node)))

;deprecated
(define (delete node)
  (begin
    (cut (heap-node-right node))
    (cut (heap-node-left node))
    ((if (eq? (heap-node-right (heap-node-parent node)) node)
              set-heap-node-right!
              set-heap-node-left!)
          (heap-node-parent node)
          (begin
            (set-heap-node-parent!
             (merge (heap-node-left node)
                    (heap-node-right node))
             (heap-node-parent node)))
         (find-root node))))

;cut: node -> void
;makes this node the root (min) of its own heap
(define (cut node)
  (if (false? node) false
      (set-heap-node-parent! node false)))

;find-root: heapnode ->  heap
;follows parent pointers upward until the root is reached
(define (find-root node)
  (if (false? (heap-node-parent node))
      node
      (find-root (heap-node-parent node))))
                
;size: heap -> int
;takes a heap and returns the number of heapnodes in it
(define (size heap)
  (cond
    [(false? heap) 0]
    [else (heap-node-size heap)]))

;less-than: (listpair int value) , (listpair int values) -> bool
;takes two heapnode values (whose firsts should always be their priorities)
;returns true if the first node's priority is less than the second's and false otherwise.
(define (less-than a b)
  (let ([aval (first a)]
        [bval (first b)])
    (< aval bval)))

;without: 'a (listof 'a) -> (listof 'a)
;takes an element and list and returns the list without the first occurence of that element.
(define (without item list)
  (if (equal? item (car list))
      (cdr list)
      (cons (car list) (without item (cdr list)))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(js-big-bang (location-change-handler init-world 41.825721 -71.41478)

             (on-location-change location-change-handler)

             (on-draw draw draw-css))