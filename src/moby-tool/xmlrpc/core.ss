;;;
;;; Time-stamp: <06/01/03 14:42:28 nhw>
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

;;; Author: Noel Welsh <noelwelsh@yahoo.com> and Matt Jadud
;;
;;
;; Commentary:

(module core mzscheme

  (require (lib "url.ss" "net")
           "serialise.ss"
           "protocol.ss")

  (provide xmlrpc-server
           xml-rpc-server)

  ;; xmlrpc-server : string integer string -> (string -> (any ... -> any))
  ;;               : url -> (string -> (any ... -> any))
  (define xmlrpc-server 
    (case-lambda
      [(host port path)
       (let ([url (string->url
                   (format "http://~a:~a/~a" host port path))])
         (xmlrpc-server url))]
      [(url)
       (lambda (method-name)
         (lambda args
           ;; This port used to go unclosed. Now, I close it.
           ;; However, this is on the client-side. So while this
           ;; does clean up a leak, it doesn't fix the server-leak.
           (let* ([impure-port
                   (make-xmlrpc-call
                    url
                    (apply encode-xmlrpc-call method-name args))]
                  [result (decode-xmlrpc-response impure-port)])
             (close-input-port impure-port)
             result)))]
      ))

  (define xml-rpc-server xmlrpc-server)
  
  )  