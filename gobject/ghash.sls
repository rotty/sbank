;;; ghash.sls --- GHashTable primitives.

;; Copyright (C) 2008 Andreas Rottmann <a.rottmann@gmx.at>

;; Author: Andreas Rottmann <a.rottmann@gmx.at>

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU Lesser General Public License as published by
;; the Free Software Foundation; either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU Lesser General Public License for more details.

;; You should have received a copy of the GNU Lesser General Public
;; License along with this program.  If not, see
;; <http://www.gnu.org/licenses/>.

;;; Commentary:

;;; Code:
#!r6rs

(library (sbank gobject ghash)
  (export g-hash-table-foreach)
  (import (rnrs base)
          (spells foreign)
          (sbank support shlibs)
          (sbank typelib stypes)
          (sbank support stypes))


  (define g-hash-table-foreach
    (let ((callback (make-c-callback 'void '(pointer pointer pointer))))
      (lambda (hash-table proc user-data)
        (g-hash-table-foreach% hash-table (callback proc) user-data))))

  (define-callouts libglib
    (g-hash-table-foreach% 'void "g_hash_table_foreach" '(pointer pointer pointer)))

  )
