;;; gquark.sls --- GQuark bindings

;; Copyright (C) 2009 Andreas Rottmann <a.rottmann@gmx.at>

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

(library (sbank gobject gquark)
  (export g-quark-from-string
          g-quark-ctype)
  (import (rnrs base)
          (spells foreign)
          (sbank support shlibs))

  (define g-quark-ctype 'uint32)

  (define (g-quark-from-string s)
    (let* ((s-ptr (string->utf8z-ptr s))
           (quark (g-quark-from-string% s-ptr)))
      (free s-ptr)
      quark))
  
  (define-c-callouts libglib
    (g-quark-from-string% g-quark-ctype "g_quark_from_string" '(pointer)))
  
  )

