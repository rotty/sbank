;;; ghash.sls --- GHashTable primitives.

;; Copyright (C) 2008-2010 Andreas Rottmann <a.rottmann@gmx.at>

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
  (export g-hash-table-ref
          g-hash-table-foreach)
  (import (rnrs base)
          (srfi :8 receive)
          (spells foreign)
          (sbank support callback-pool)
          (sbank support shlibs))

  (define foreach-callback-pool
    (make-callback-pool (make-c-callback 'void '(pointer pointer pointer))))

  (define foreach-callback (callback-pool-getter foreach-callback-pool))

  (define (g-hash-table-foreach hash-table proc user-data)
    (receive (cb-ptr reclaim) (foreach-callback proc)
      (g-hash-table-foreach% hash-table cb-ptr user-data)
      (reclaim)))

  (define-c-callouts libglib
    (g-hash-table-foreach% 'void "g_hash_table_foreach" '(pointer pointer pointer))
    (g-hash-table-ref 'void "g_hash_table_ref" '(pointer)))

  )
