;; This is the WebIt! collection, for XML programming in Scheme.
;; 
;; License
;; -------
;; 
;; WebIt! Collection
;; Copyright (c) 2000-2005 James G. Bender
;; 
;; The WebIt! collection is distributed under the MIT License.
;; 
;; More Information
;; ----------------
;; 
;; The main web site for WebIt! is http://celtic.benderweb.net/webit/.
;; 
;; A mailing list for WebIt! is available from WebIt!'s SourceForge 
;; project page:
;;    http://www.sourceforge.net/projects/webit/
;; 

(module info (lib "infotab.ss" "setup")
  (define doc.txt "doc.txt")
  (define name "WebIt! XML Framework")
  (define blurb '("A system for authoring and transforming XML in Scheme."
                  " "
                  "http://celtic.benderweb.net/webit/"))
  (define help-desk-message  "(require (planet \"xml.ss\" (\"jim\" \"webit.plt\" 1 3) )")
  (define primary-file "xml.ss")
  (define homepage "http://celtic.benderweb.net/webit/")
  (define categories '(xml))
  (define version "2.0"))

