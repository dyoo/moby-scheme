#lang setup/infotab
(define name "moby")
(define mred-launcher-libraries (list "src/moby.ss"))
(define mred-launcher-names (list "moby"))
(define requires (list (list "mred")))

(define tools (list (list "src/tool.ss")))

(define compile-omit-paths (list "doc"
                                 "examples"
                                 "sandbox"
                                 "stub"
                                 "support"))

(define scribblings '(("manual.scrbl")))