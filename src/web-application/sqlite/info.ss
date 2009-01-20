#lang setup/infotab
(define name "SQLite 3 FFI")
(define release-notes
  (list '(p "Updated to PLT 4.0")))
(define repositories
  (list "4.x"))
(define blurb
  (list "Allows access to SQLite databases."))
(define scribblings '(("sqlite.scrbl" ())))
(define primary-file "sqlite.ss")
(define categories '(io))