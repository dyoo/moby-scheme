;;;
;;; Time-stamp: <06/01/04 15:14:35 nhw>
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

;;; Author: Noel Welsh <noelwelsh@yahoo.com>
;;          Matt Jadud <jadudm@gmail.com>
;;
;; Commentary:

(module protocol scheme

  (require (planet "xml.ss" ("jim" "webit.plt" 1 4))
           (planet "xxexpr.ss" ("lshift" "xxexpr.plt" 1))
           (lib "url.ss" "net")
           ;; 20060711 MCJ
           ;; It would appear that a PLaneT require would be 
           ;; more appropriate here
           ;;(lib "ssax.ss" "ssax")
           (prefix-in ssax: (planet "ssax.ss" ("lizorkin" "ssax.plt" 2 0)))
           "base.ss"
           "serialise.ss")
  
  (provide encode-xmlrpc-call
           write-xmlrpc-call
           make-xmlrpc-call
           read-xmlrpc-response
           decode-xmlrpc-response
           ;; Server-side
           decode-xmlrpc-call
           (struct-out rpc-call))

  ;; http-200? : string -> (U #t #f)
  (define (http-200? headers)
    (if (regexp-match #rx"^HTTP/[0-9]*\\.[0-9]* 200" headers)
        #t
        #f))
  
  ;; http-404? : string -> (U #t #f)
  (define (http-404? headers)
    (if (regexp-match #rx"^HTTP/[0-9]*\\.[0-9]* 404" headers)
        #t
        #f))
  
  ;; encode-xmlrpc-call : string any ... -> sxml
  (define (encode-xmlrpc-call method-name . args)
    `(methodCall
      (methodName ,method-name)
      (params
       ,@(map (lambda (val)
                `(param ,(serialise val)))
              args))))

  ;; write-xmlrpc-call-headers : sxml output-port -> #t
  (define (write-xmlrpc-call call op)
    (parameterize
      ((xml-double-quotes-mode #t))
      (let ([result
             (pretty-print-xxexpr (list '(*pi* xml (version "1.0"))
                                        call) op)])
        ;; We don't need to close this port; it's an
        ;; 'ouput-bytes' port. Oops. Closing this breaks things.
        ;;(close-output-port op)
        result)))

  ;; WARNING 20060711 MCJ
  ;; Given a bad hostname, make-xmlrpc-call could fail. Should we 
  ;; catch that and pass it on as an XML-RPC exception, 
  ;; or leave it to the developer?
  #|
../../../../Library/PLT Scheme/350/collects/xmlrpc/protocol.ss::2267: tcp-connect: connection to locahost, port 8080 failed; host not found (at step 1: No address associated with nodename; errno=7)
|#
  ;; make-xmlrpc-call : url sxml -> impure-port
  (define (make-xmlrpc-call url call)
    (let ((op (open-output-bytes)))
      (write-xmlrpc-call call op)
      (post-impure-port url
                        (get-output-bytes op)
                        '("Content-Type: text/xml"
                          "User-Agent: PLT Scheme"))))

  ;; read-xmlrpc-response : input-port -> sxml
  (define (read-xmlrpc-response ip)
    (let ((headers (purify-port ip)))
      ;; Expanding the quality of error message supplied to the 
      ;; programmer developing with the XML-RPC library.
      (cond
        [(http-404? headers)
         (raise-exn:xmlrpc "Server responded with a 404: File not found")]
        [(not (http-200? headers))
         (raise-exn:xmlrpc  
          (format "Server did not respond with an HTTP 200~nHeaders:~n~a~n"
                  headers))])
      ;; 20060731 MCJ
      ;; This input port doesn't seem to get closed. Or, 
      ;; if it does, I don't know where. We'll find out.
      (let ([response (ssax:ssax:xml->sxml ip '())])
        (close-input-port ip)
        response) ))

  ;; decode-xmlrpc-response : input-port -> any
  (define (decode-xmlrpc-response ip)
    (let ((resp (read-xmlrpc-response ip)))
      (xml-match (xml-document-content resp)
        [(methodResponse (params (param ,value)))
         (deserialise value)]
        [(methodResponse (fault ,value))
         (let ((h (deserialise value)))
           (raise
            (make-exn:xmlrpc:fault
             (string->immutable-string
              (hash-ref h 'faultString))
             (current-continuation-marks)
             (hash-ref h 'faultCode))))]
        [,else
         (raise-exn:xmlrpc
          (format "Received invalid XMLRPC response ~a\n" else))])))
  
  
  ;; Server-side
  ;; extract-parameter-values : (list-of `(param ,v)) -> any
  (define (extract-parameter-values param*)
    (map (lambda (p)
           (xml-match p
             [(param ,value) (deserialise value)]
             [,else
               (raise-exn:xmlrpc
                (format "Bad parameter in methodCall: ~a~n" p))]))
         param*))
  
  ;; read-xmlrpc-response : string -> sxml
  (define (read-xmlrpc-call str)
    (let* ([call-ip (open-input-string str)]
           [result (ssax:ssax:xml->sxml call-ip '())])
      (close-input-port call-ip)
      result))
  
  ;; decode-xmlrpc-call : string -> any
  (define-struct rpc-call (name args))
  (define (decode-xmlrpc-call str)
    (let ([docu (read-xmlrpc-call str)])
      (xml-match (xml-document-content docu)
        [(methodCall (methodName ,name) (params ,param* ...))
         (let ([value* (extract-parameter-values param*)])
           (make-rpc-call (string->symbol name) value*))]
        [,else
          (raise-exn:xmlrpc
           (format "Cannot parse methodCall: ~a~n" else))])))
 
  )