#lang moby
;; This is a set of basic tests.  A lot of this is copy-and-pasted from PLT-Scheme's
;; test suite.


;; test that all symbol characters are supported.
'(+ - ... !.. $.+ %.- &.! *.: /:. :+. <-. =. >. ?. ~. _. ^.)


(define disjoint-type-functions
  (list boolean? char? null? number? pair? procedure? string? symbol? vector?))