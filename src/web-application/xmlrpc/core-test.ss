;;;
;;; Time-stamp: <06/01/03 14:11:07 nhw>
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
;;
;;
;; Commentary:

(module core-test scheme
  
  (require (planet "test.ss" ("schematics" "schemeunit.plt" 2)))
  (require (lib "url.ss" "net")
           (file "core.ss")
           (file "util.ss"))
  
  (provide core-tests)

  (define betty (xmlrpc-server
                 (string->url "http://betty.userland.com/RPC2")))

  (define get-state-name (betty "examples.getStateName"))
  
  (define core-tests
    (test-suite
     "All tests for core"
     
     (test-case
      "Round-trip call works"
      (with-timeout 
       "Round-trip times out." RPC-TIMEOUT
       (check string=? (get-state-name 40) "South Carolina"))
      (with-timeout
       "Round-trip times out." RPC-TIMEOUT
       (check string=? (get-state-name 42) "Tennessee")))

     (test-case
      "xmlrpc-server accepts host port and path"
      (let* ((betty
              (xmlrpc-server "betty.userland.com" 80 "RPC2"))
             (get-state-name (betty "examples.getStateName")))
        (with-timeout 
         "Server accepts host/port/path times out." RPC-TIMEOUT
         (check string=? (get-state-name 40) "South Carolina"))
        (with-timeout 
         "Server accepts host/port/path times out." RPC-TIMEOUT
         (check string=? (get-state-name 42) "Tennessee"))))
     ))
  )
