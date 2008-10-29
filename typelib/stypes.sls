;;; stypes.sls --- 

;; Copyright (C) 2008 Andreas Rottmann <a.rottmann@gmx.at>

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

;;; Code:


(library (sbank typelib stypes)
  (export typelib-stypes)
  (import (rnrs base)
          (rnrs control)
          (rnrs lists)
          (rnrs io simple)
          (spells find-file)
          (spells pathname)
          (sbank stypes))

  (define (slurp-types)
    (let* ((relpath '((sbank data) "typelib.scm"))
           (filename (find-file relpath (library-search-paths))))
      (unless filename
        (error 'typelib-stypes
               "typelib GIR data file not found"
               (x->namestring relpath)
               (library-search-paths)))
      (call-with-input-file (x->namestring filename) read)))

  (define extra-types
    '((alias "gtype" "size_t")
      (record (name "GError")
              (field (name "domain") (type "uint32"))
              (field (name "code") (type "int"))
              (field (name "message")
                     (type (array (element-type (type "char"))))))
      (record (name "GValue")
              (field (name "g_type") (type "gtype"))
              (field
               (name "data")
               (type (array (element-type
                             (type (union (field (name "v_int")    (type "int"))
                                          (field (name "v_uint")   (type "uint"))
                                          (field (name "v_long")   (type "long"))
                                          (field (name "v_ulong")  (type "ulong"))
                                          (field (name "v_int64")  (type "int64"))
                                          (field (name "v_uint64") (type "uint64"))
                                          (field (name "v_float") (type "float"))
                                          (field (name "v_double") (type "double"))
                                          (field (name "v_pointer") (type "pointer")))))
                            (size 2)))))))
  
  (define typelib-stypes
    (let ((stypes #f))
      (lambda ()
        (unless stypes
          (set! stypes (fold-left stypes-adjoin
                                  primitive-stypes
                                  (append extra-types (slurp-types)))))
        stypes))))
