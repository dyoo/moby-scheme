#lang setup/infotab
(define name "Moby")

;; Temporarily disabling the command-line launcher.
(define required-core-version "5.0.1")

#;(define drracket-name "moby")
#;(define drracket-tools (list (list "src/tool.rkt")))

(define primary-file "main.rkt")
(define can-be-loaded-with 'all)

(define compile-omit-paths (list "doc"
                                 "examples"
                                 "sandbox"
                                 "stub"
                                 "support"
	                         "tmp"))

(define scribblings '(("manual.scrbl")))

(define categories '(devtools))
(define repositories '("4.x"))

(define version "3.10")

(define blurb '("Provides a compiler from Advanced Student Language+world to Javascript "
                "for mobile smartphones."))


(define release-notes
  '((p "Changes"
       (ul
	
	(li "Jsworld has relaxed constraints on attribute values.  Attribute values can now be strings or booleans.  An additional type check will raise an error if a non-string, non-boolean is given.")
	(li "Fixed bug: world widgets would have their contents erroneously deleted under certain situations.")
	
	(li "Removed whitespace PRE styling on js-text nodes.")
	(li "Fixed low level bug: generated packages could be larger because they contained multiply copies of a module.")))))
