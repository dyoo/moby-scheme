#cs(module poetry mzscheme
  (require "../../xml.ss")
  (define poetry-ns-url '#f)
  (define-element (line #f))
  (define-element (stanza #f))
  (define-element (book #f))
  (define-element (poem #f))
  (define-attribute (poet #f))
  (define-attribute (tag #f))
  (define-attribute (title #f))
  (provide (all-defined)))

