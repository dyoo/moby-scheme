;;;
;;; Time-stamp: <06/01/03 14:31:05 nhw>
;;;
;;; Copyright (C) 2005 by Noel Welsh. 
;;;

;;; This library is free software; you can redistribute it
;;; and/or modify it under the terms of the GNU Lesser
;;; General Public License as published by the Free Software
;;; Foundation; either version 2.1 of the License, or (at
;;; your option) any later version.

;;; This library is distributed in the hope that it will be
;;; useful, but WITHOUT ANY WARRANTY; without even the
;;; implied warranty of MERCHANTABILITY or FITNESS FOR A
;;; PARTICULAR PURPOSE.  See the GNU Lesser General Public
;;; License for more details.

;;; You should have received a copy of the GNU Lesser
;;; General Public License along with this library; if not,
;;; write to the Free Software Foundation, Inc., 59 Temple
;;; Place, Suite 330, Boston, MA 02111-1307 USA

;;; Author: Noel Welsh <noelwelsh@yahoo.com>, Matt Jadud <jadudm@gmail.com>
;;
;;
;; Commentary:

(module serialise scheme
  
  (require (planet "xml.ss" ("jim" "webit.plt" 1 4))
           (lib "pregexp.ss")
           (prefix-in c: (lib "contract.ss"))
           (only-in (lib "date.ss") find-seconds)
           (only-in (lib "string.ss") regexp-quote)
           (only-in (lib "base64.ss" "net") base64-decode)
           "util.ss"
           "base.ss")
  
  (provide serialise
           deserialise
           encode-string
           decode-string)
  
  (define replace-&-and-<
    (let ((amp-re (regexp (regexp-quote "&")))
          (lt-re (regexp (regexp-quote "<"))))
      (lambda (str)
        (regexp-replace* lt-re
                         (regexp-replace* amp-re str "\\&amp;")
                         "\\&lt;"))))
  
  (define replace-entities
    (let ((amp-re (regexp (regexp-quote "&amp;")))
          (lt-re (regexp (regexp-quote "&lt;"))))
      (lambda (str)
        (regexp-replace* amp-re
                         (regexp-replace* lt-re str "<")
                         "\\&"))))
  
  (define identity
    (lambda (x) x))
  
  (c:define/contract encode-string-guard
    (c:-> boolean? any)
    (lambda (replace?)
      (if replace?
          replace-&-and-<
          identity)))
  
  (c:define/contract decode-string-guard
    (c:-> boolean? any)
    (lambda (replace?)
      (if replace?
          replace-entities
          identity)))
  
  (define encode-string
    (make-parameter replace-&-and-< encode-string-guard))
  
  (define decode-string
    (make-parameter replace-entities decode-string-guard))
  
  ;; date->iso8601-string : date -> string
  (define (date->iso8601-string date)
    (define (pad number)
      (let ((str (number->string number)))
        (if (< (string-length str) 2)
            (string-append "0" str)
            str)))
    (string-append
     (number->string (date-year date))
     (pad (date-month date))
     (pad (date-day date))
     "T"
     (pad (date-hour date))
     ":"
     (pad (date-minute date))
     ":"
     (pad (date-second date))))
  
  ;; serialise : (U integer string boolean double date hash-table list) -> SXML
  ;;
  ;; Convert the value to its XML-RPC representation
  (define (serialise val)
    (cond
      [(or (eq? +nan.0 val) (eq? +inf.0 val) (eq? -inf.0 val))
       ;; note that +nan.0 = -nan.0 so we don't check this case
       (raise-exn:xmlrpc
        (format "Given ~s to serialise to XML-RPC.  XML-RPC does not allow NaN or infinities; and so this value cannot be serialised" val))]
      [(and (number? val) (inexact? val))
       ;; If I'm correct an inexact number is represented by
       ;; a double, so this should be always in range.
       `(value (double ,(number->string val)))]
      [(integer? val)  
       ;; Integers are bound to 4-byte representations by the protocol.
       (if (and (<= val (expt 2 31))
                (>= val (- (expt 2 31))))
           `(value (int ,(number->string val)))
           (raise-exn:xmlrpc 
            (format "The Scheme number ~s is out of range for an XML-RPC integer" val)))]
      [(string? val)  `(value (string ,val))]
      ;; 20060711 MCJ
      ;; We could encode symbols as strings. However, this breaks
      ;; the semantics of Scheme. Should we force users to send
      ;; symbols as strings, or do an automatic conversion?
      ;; Currently, both lists and vectors map to the same XML-RPC datastructure,
      ;; so this is not unprecedented.
      [(symbol? val)  `(value (string ,((encode-string) (symbol->string val))))]
      [(boolean? val) `(value (boolean ,(if val "1" "0")))]
      [(date? val) `(value (dateTime.iso8601
                            ,(date->iso8601-string val)))]
      [(hash? val)
       `(value (struct ,@(hash-map
                          val
                          (lambda (k v)
                            `(member (name ,(symbol->string k))
                                     ,(serialise v))))))]
      [(list? val)
       `(value (array (data ,@(map serialise val))))]
      [(vector? val)
       `(value (array (data ,@(map serialise (vector->list val)))))]
      [(bytes? val)
       `(value (base64 ,(base64-encode val)))]
      [else
       (raise-exn:xmlrpc
        (format "Cannot convert Scheme value ~s to XML-RPC" val))]))
  
  ;; deserialise-struct : list-of-SXML -> Scheme value
  (define (deserialise-struct member*)
    (let ([h (make-hash)])
      (for-each
       (lambda (member)
         (xml-match member
           ;; They may have shipped the empty string; this is optionally encoded
           ;; as (value) in the API. Perhaps we should deserialize this differently?
           ;; Either way, this is a quick fix for the problem.
           [(member (name ,name) (value))
            (hash-set! h (string->symbol name) "")]
           
           ;; This works if we have a value here...
           [(member (name ,name) (value ,[deserialise -> v]))
            (hash-set! h (string->symbol name) v)]
           [,else
             (raise-exn:xmlrpc
              (format "The XML-RPC struct data ~s is badly formed and cannot be converted to Scheme" else))]))
       member*)
      h))
  
  (define (deserialize-iso8601 v)
    ;;<value><dateTime.iso8601>20051030T22:29:34</dateTime.iso8601></value>
    (let ([pieces (pregexp-match 
                   "(\\d\\d\\d\\d)(\\d\\d)(\\d\\d)T(\\d\\d):(\\d\\d):(\\d\\d)" v)])
      (if pieces
          (let-values ([(all year month day h m s)
                        (apply values (map string->number pieces))])
            (let* ([given-date (seconds->date (find-seconds s m h day month year))]
                   [tzo 
                    (date-time-zone-offset (seconds->date (current-seconds)))])
              (struct-copy date given-date (time-zone-offset tzo))
              ))
          (raise-exn:xmlrpc
           (format 
            "The XML-RPC date ~s badly formatted; cannot be converted to Scheme" v)))))
  
  ;; deserialise : sxml -> (U float boolean integer string date hash list)
  (define (deserialise val)
    (xml-match val
      ;; Our struct deserialiser can dump here with a bare string.
      ;; We need to guard against that and simply return the string.
      [,bare-string (guard (string? bare-string)) bare-string]
      [(value ,type)
       (cond
         [(list? type)
          (deserialise type)]
         [(string? type)
          ;; This is the default case if not type information
          ;; is given
          type])]
      [(value) ""]
      ;; Numbers
      [(int ,v) (string->number v)]
      [(i4 ,v) (string->number v)]
      [(double ,v) (string->number v)]
      ;; Strings
      [(string) ""]
      [(string ,v) v]
     
      ;; Booleans
      [(boolean ,v) (string=? v "1")]
      ;; Date
      [(dateTime.iso8601 ,v) 
       (deserialize-iso8601 v)]
      
      ;; B64
      ;; 20060829 MCJ
      ;; Apparently, the Apache XML-RPC v2 library sends
      ;; an empty Base64 tag if no data is present.
      [(base64) #""]
      [(base64 ,v)
       (base64-decode (string->bytes/utf-8 v))]
      ;; Structs
      [(struct ,member* ...)
       (deserialise-struct member*)]
      ;; Arrays
      [(array (data ,[value*] ...))
       value*]
      [,else
        (raise-exn:xmlrpc
         (format "Cannot convert the XML-RPC type ~s to Scheme" else))]))
  
  )