;;;
;;; Time-stamp: <06/01/03 14:02:20 nhw>
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

(module all-xmlrpc-tests mzscheme

  (require (planet "test.ss" ("schematics" "schemeunit.plt" 2)))
  (require "serialise-test.ss"
           "protocol-test.ss"
           "core-test.ss")
  (provide all-xmlrpc-tests)

  (define all-xmlrpc-tests
    (test-suite 
     "all-xmlrpc-tests"
     serialise-tests
     protocol-tests
     core-tests
     ))
  )
