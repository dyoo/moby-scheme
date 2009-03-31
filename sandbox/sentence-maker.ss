;; The first three lines of this file were inserted by DrScheme. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname sentence-maker) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ())))
(require (lib "gui-world.ss" "gui-world"))

;; This is a simple sentence-constructing program with a
;; fixed structure: noun verb noun.

;; The world is a
(define-struct world (subj verb obj))
;; where subj, verb, and obj are strings.

;; The define-updaters form of gui-world will construct definitions
;; for extending the world.
(define-updaters world)

(define initial-world (make-world "" "" ""))

(define NOUNS 
  (list ""
        "raindrops"
        "roses"
        "whiskers"
        "kittens"
        "kettles"
        "mittens"
        "packages"
        "strings"
        "ponies"
        "streudels"
        "doorbells"
        "sleighbells"
        "schnitzel"
        "noodles"
        "geese"
        "moon"
        "wings"
        "dresses"
        "sashes"
        "snowflakes"
        "nose"
        "eyelashes"
        "winters"
        "springs"
        "dog"
        "bee"
        "things"))


(define VERBS
  (list ""
        "vomited"
        "tasted"
        "spilled"
        "grabbed"
        "chopped"))


;; sentence: world -> string
;; Builds the sentence from the world.
(define (sentence w)
  (string-append (world-subj w)
                 " "
                 (world-verb w)
                 " "
                 (world-obj w)))
  

;; The view of the world has dropdowns for selecting
(define view
  (col
   (row (col "Noun"
             (drop-down world-subj NOUNS update-world-subj))
        (col "Verb"
             (drop-down world-verb VERBS update-world-verb))
        (col "Object"
             (drop-down world-obj NOUNS update-world-obj)))
   (message sentence)))

(big-bang initial-world view)