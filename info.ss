#lang setup/infotab
(define name "The Moby Scheme Compiler")
(define mred-launcher-libraries (list "src/moby.ss"))
(define mred-launcher-names (list "moby"))
(define requires (list (list "mred")))

(define tools (list (list "src/tool.ss")))

(define compile-omit-paths (list "doc"
                                 "examples"
                                 "sandbox"
                                 "stub"
                                 "support"))

(define categories '(devtools))

(define scribblings 
  '(("manual.scrbl" ())))

(define blurb '("Provides a compiler from Advanced Student Language+world to Javascript "
                "for both web browsers and mobile smartphones."))