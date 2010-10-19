#lang setup/infotab
(define name "The Moby Scheme Compiler")

;; Temporarily disabling the command-line launcher.
(define required-core-version "5.0.1")

(define tools (list (list "src/tool.ss")))

(define compile-omit-paths (list "doc"
                                 "examples"
                                 "sandbox"
                                 "stub"
                                 "support"
	                         "tmp"))

(define categories '(devtools))
(define repositories '("4.x"))

(define version "3.0")

(define blurb '("Provides a compiler from Advanced Student Language+world to Javascript "
                "for both web browsers and mobile smartphones."))
