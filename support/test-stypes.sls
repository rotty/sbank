;;; test-stypes.sls --- stypes for the test suite

;; Copyright (C) 2009 Andreas Rottmann <a.rottmann@gmx.at>

;; Author: Andreas Rottmann <a.rottmann@gmx.at>

;; This library is free software; you can redistribute it and/or
;; modify it under the terms of the GNU Lesser General Public
;; License as published by the Free Software Foundation; either
;; version 3 of the License, or (at your option) any later version.

;; This library is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; Lesser General Public License for more details.

;; You should have received a copy of the GNU Lesser General Public
;; License along with this library. If not, see
;; <http://www.gnu.org/licenses/>.

;;; Commentary:

;; This must be in a library to avoid phasing issues

;;; Code:
#!r6rs

(library (sbank support test-stypes)
  (export test-stypes)
  (import (rnrs base)
          (sbank support stypes))
  
  (define test-stypes
    (stypes-adjoin
     primitive-stypes
     '(record
       (name "Foo")
       (field (name "frobotz") (type "uint"))
       (field (name "val")
              (type (array (element-type (type "double"))
                           (element-count 3))))
       (field (name "data") (type (pointer (type "uint8"))))
       (field (name "tag") (type "uint8") (bits 5))
       (union
        (field (name "u1") (type "double"))
        (field (name "u2") (type "uint16")))
       (field (name "last") (type "boolean")))
   
     '(alias (name "gtype") (target "size_t"))
     '(record (name "GValue")
              (field (name "g_type") (type "gtype"))
              (field
               (name "data")
               (type (array
                      (element-type
                       (type
                        (union (field (name "v_int") (type "int"))
                               (field (name "v_double") (type "double")))))
                      (element-count 2)))))

     '(union (name "SimpleTypeBlob")
             (record
              (field (name "tag") (type "uint") (bits 5))
              (field (name "reserved3") (type "uint") (bits 2))
              (field (name "pointer") (type "uint") (bits 1)))
             (field (name "offset") (type "uint32")))

     '(record (name "RecordWithArrays")
              (field (name "type") (type "uint"))
              (field (name "data")
                     (type (array (element-type (type "uint"))
                                  (element-count 16))))
              (field (name "ptr") (type (pointer (type "uchar"))))))))
