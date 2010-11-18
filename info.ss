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

(define version "3.6")

(define blurb '("Provides a compiler from Advanced Student Language+world to Javascript "
                "for mobile smartphones."))


(define release-notes
  '(
    (p "Fixes:"
       (ul

	(li "check-expect: check-expect now works by
    applying tests after all of the definitions and other expressions
    have evaluated.")
	
	(li "jsworld reentrancy: there was an issue where jsworld behaved badly
    (reverting to old worlds) if events came in at a rate faster than
    it could handle them.")
	
	(li "member: the definition of member can been corrected to return a
    boolean in the WeScheme (Moby) languages.")
	
	(li "adjusted internal " (it "gas") "parameter to improve performance on phones.")
	
	(li "extended on-tilt to allow optional delay argument, similar to on-tick's delay")
	(li "enforced portrait orientation of phone applications")

	(li "miscellaneous fixes: corrected errors involving
    struct-mutator-procedure, exn structures constructors, and some
    arithmetic edge cases.")
	(li "exposed the struct-out form for provides.")))))
