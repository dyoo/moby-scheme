(module scm-markup mzscheme
  
  (require "xml.ss"
           "html.ss"
           "css.ss"
           (lib "string.ss"))
  
  (provide scm->html
           scm-prog->html)
  
  (define keywords
    '(=>
      and
      begin
      begin0
      case
      cond
      define
      define-macro
      define-syntax
      defmacro
      defstruct
      delay
      do
      else
      flet
      fluid-let
      if
      labels
      lambda
      let
      let-syntax
      let-values
      let*
      let*-values
      letrec
      letrec-syntax
      lib
      macrolet
      module
      or
      parameterize
      provide 
      quasiquote
      quote
      require
      set!
      syntax-case
      syntax-rules
      unless
      unquote
      unquote-splicing
      values
      when
      with
      with-handlers
      compound-stylesheet 
      stylesheet
      xml-case
      xml-macro
      xml-micro
      xml-rules
      xml-template
      bind-namespaces 
      define-attribute
      define-element
      define-namespace
      quote-xml quasi-xml 
      css-attr css css/html 
      xml-match xml-match-let xml-match-let*
      sxml-match sxml-match-let sxml-match-let*
      ; additions for the XQuery-style constructs
      for where order by some every))
  
  ; more here.
  (define builtins
    '(car
      cdr
      cons
      list
      map
      append
      assoc
      assq
      assv
      memq
      member
      memv
      +
      -
      *
      /
      <
      <=
      >
      >=
      =
      eq?
      equal?
      eqv?
      string-append
      string?
      string=?
      string<?
      string>?
      string<=?
      string>=?
      string-ref
      list-ref
      ;write
      ;display
      ;read
      ;open-input-file
      ;open-output-file
      ;with-input-from-file
      ;with-output-to-file
      ))
  
  (define (symbol-class s)
    (if (memq s keywords)
        "keyword"
        (if (memq s builtins)
            "builtin"
            "variable")))
  
  (define (parse-fragment str)
    (let ((end (string-length str)))
      (define (parse-comment pos)
        (cond
          ((>= pos end) pos)
          ((char=? #\newline (string-ref str pos)) (+ pos 1))
          ((char=? #\return (string-ref str pos))
           (if (and (< (+ pos 1) end) (char=? #\newline (string-ref str (+ pos 1))))
               (+ pos 2)
               (+ pos 1)))
          (else (parse-comment (+ pos 1)))))
      (define (parse-block-comment pos)
        ;; more here. need to support nested block comments
        (cond
          ((>= pos end) pos)
          ((and (char=? #\| (string-ref str pos))
                (< (+ pos 1) end)
                (char=? #\# (string-ref str (+ pos 1))))
           (+ pos 2))
          (else (parse-block-comment (+ pos 1)))))
      (define (parse-whitespace pos)
        (if (>= pos end)
            pos
            (case (string-ref str pos)
              ((#\space #\newline #\return #\tab)
               (parse-whitespace (+ pos 1)))
              (else pos))))
      (define (parse-string pos)
        (if (>= pos end)
            pos
            (cond
              ((eq? #\\ (string-ref str pos))
               (if (< (+ pos 1) end)
                   (parse-string (+ pos 2))
                   (+ pos 1)))
              ((eq? #\" (string-ref str pos)) (+ pos 1))
              (else (parse-string (+ pos 1))))))
      (define (parse-token pos)
        (if (>= pos end)
            pos
            (case (string-ref str pos)
              ((#\( #\) #\[ #\] #\space #\newline #\return #\tab)
               pos)
              (else (parse-token (+ pos 1))))))
      (define (markup-token tok-str)
        (let ((item (read-from-string tok-str (lambda (x) #f) (lambda () error))))
          (cond
            ((symbol? item)
             (h4:span h4:class: (symbol-class item) tok-str))
            ((number? item)
             (h4:span h4:class: "selfeval" tok-str))
            (else tok-str))))
      (let loop ((beg 0)
                 (pos 0)
                 (acc '()))
        (if (>= pos end)
            (reverse acc)
            (let ((ch (string-ref str pos)))
              (case ch
                ((#\;)
                 (let ((mk (parse-comment (+ pos 1))))
                   (loop mk mk (cons (h4:span h4:class: "comment" (substring str beg mk)) acc))))
                ((#\space #\newline #\return #\tab)
                 (let ((mk (parse-whitespace (+ pos 1))))
                   (loop mk mk (cons (substring str beg mk) acc))))
                ((#\( #\) #\[ #\])
                 (loop (+ pos 1) (+ pos 1) (cons (string ch) acc)))
                ((#\' #\`)
                 (loop (+ pos 1) (+ pos 1) (cons (h4:span h4:class: "keyword" (string ch)) acc)))
                ((#\,)
                 (if (and (< (+ pos 1) end) (eq? #\@ (string-ref str (+ pos 1))))
                     (loop (+ pos 2) (+ pos 2) (cons (h4:span h4:class: "keyword" ",@") acc))
                     (loop (+ pos 1) (+ pos 1) (cons (h4:span h4:class: "keyword" ",") acc))))
                ((#\")
                 (let ((mk (parse-string (+ pos 1))))
                   (loop mk mk (cons (h4:span h4:class: "selfeval" (substring str beg mk)) acc))))
                ((#\#)
                 (let ((mk (+ pos 1)))
                   (if (>= mk end)
                       (loop mk mk (cons (h4:span h4:class: "selfeval" "#") acc))
                       (case (string-ref str mk)
                         ((#\|)
                          (let ((end-mk (parse-block-comment (+ mk 1))))
                            (loop end-mk
                                  end-mk
                                  (cons (h4:span h4:class: "comment" (substring str beg end-mk)) acc))))
                         ((#\&)
                          (loop (+ mk 1) (+ mk 1) (cons (h4:span h4:class: "selfeval" "#&") acc)))
                         ((#\t)
                          (loop (+ mk 1) (+ mk 1) (cons (h4:span h4:class: "selfeval" "#t") acc)))
                         ((#\f)
                          (loop (+ mk 1) (+ mk 1) (cons (h4:span h4:class: "selfeval" "#f") acc)))
                         ((#\( #\[)
                          (loop (+ mk 1) (+ mk 1) (cons "#(" acc)))
                         ((#\\)
                          (if (>= (+ mk 2) end)
                              (loop (+ mk 1) (+ mk 1) (cons (h4:span h4:class: "selfeval" "#\\") acc))
                              (let ((end-mk (parse-token (+ mk 2))))
                                (loop end-mk
                                      end-mk
                                      (cons (h4:span h4:class: "selfeval" (substring str beg end-mk)) acc)))))
                         (else (loop mk mk (cons (h4:span h4:class: "selfeval" "#") acc)))))))
                (else
                 (let ((mk (parse-token (+ pos 1))))
                   (loop mk mk (cons (markup-token (substring str beg mk)) acc))))))))))
  
  (define (scm-prog->html str)
    (h4:pre h4:class: "scheme" (parse-fragment str)))
  
  (define (scm->html str)
    (h4:code h4:class: "scheme" (parse-fragment str)))
  
  )
