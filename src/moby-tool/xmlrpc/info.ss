(module info (lib "infotab.ss" "setup")

  (define name "xmlrpc")
  (define required-core-version "4.1.1")
  (define compile-omit-files '("run-tests.ss"))
  
  (define blurb '("Implementation of the XML-RPC protocol."))
  (define release-notes 
    '((p "This is a client- and server-side implementation of the XML-RPC protocol.")
      (p "This release may fix many things, and bring us up to PLT 410 compliance. Tests pass.")))

  (define categories '(net))
  (define repositories '("4.x"))
  (define doc.txt "doc.txt")
  (define primary-file "xmlrpc.ss")
  (define url "http://schematics.sourceforge.net/")
  (define version "2.0"))

