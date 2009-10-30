#lang setup/infotab
(define name "The Moby Scheme Compiler")
(define mred-launcher-libraries (list "src/moby.ss"))
(define mred-launcher-names (list "moby"))
(define requires (list (list "mred")))
(define required-core-version "4.2")

(define tools (list (list "src/tool.ss")))

(define compile-omit-paths (list "doc"
                                 "examples"
                                 "sandbox"
                                 "stub"
                                 "support"
	                         "tmp"))

(define categories '(devtools))
(define repositories '("4.x"))

(define scribblings 
  '(("manual.scrbl" ())))

(define blurb '("Provides a compiler from Advanced Student Language+world to Javascript "
                "for both web browsers and mobile smartphones."))

(define release-notes '("Added inexact and exact numeral literals to the reader; added complex numbers to the reader.  Fixed some edge cases with modulo and quotient.  Adjusted error message of on-draw to more specifically pinpoint structure problems."))