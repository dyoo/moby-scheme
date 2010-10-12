#lang s-exp "lang.ss"


;; A labeled translation consists of a label, and a translation
(define-struct labeled-translation (label  ;; number
                                    translation ;; string
                                    ))


(provide/contract [struct labeled-translation
                          ([label number?]
                           [translation string?])])