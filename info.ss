#lang setup/infotab
(define name "The Moby Scheme Compiler")

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

(define version "3.1")

(define blurb '("Provides a compiler from Advanced Student Language+world to Javascript "
                "for mobile smartphones."))
