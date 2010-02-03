#lang s-exp "lang.ss"

(define MAJOR "2")
(define MINOR "33")

(define VERSION (format "~a.~a" MAJOR MINOR))


(provide/contract [VERSION string?])