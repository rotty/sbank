;;; gtype.sls --- Low-level access to GType

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

(library (sbank gobject gtype)
  (export symbol->gtype gtype->symbol gtype-ctype)
  (import (rnrs base)
          (rnrs arithmetic bitwise)
          (sbank utils)
          (sbank shlibs))

  ;; Note these must be in sync with gtype.h
  (define-enum (gtype->symbol% symbol->gtype%)
    (invalid
      none
      interface
      char
      uchar
      boolean
      int
      uint
      long
      ulong
      int64
      uint64
      enum
      flags
      float
      double
      string
      pointer
      boxed
      param
      object))

  (define *fundamental-shift* 2)
  
  (define gtype->symbol
    (let-callouts libgobject ((fundamental% gtype-ctype "g_type_fundamental" (list gtype-ctype)))
      (lambda (gtype)
        (gtype->symbol% (fundamental% gtype)))))

  (define (symbol->gtype sym)
    (bitwise-arithmetic-shift (symbol->gtype% sym) *fundamental-shift*))

  (define gtype-ctype 'size_t))
