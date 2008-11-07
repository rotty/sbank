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
  (export symbol->gtype gtype->symbol
          pointer-gtype-ref pointer-gtype-set!
          gtype-ctype g-type-init)
  (import (rnrs base)
          (rnrs arithmetic bitwise)
          (spells foreign)
          (spells tracing)
          (sbank utils)
          (sbank shlibs)
          (sbank gobject boxed-values)
          (sbank gobject gtype simple))
  
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
        (if (= gtype (g-boxed-value-type))
            'boxed
            (gtype->symbol% (bitwise-arithmetic-shift-right
                             (fundamental% gtype)
                             *fundamental-shift*))))))

  (define (symbol->gtype sym)
    (case sym
      ((boxed) (g-boxed-value-type))
      (else
       (bitwise-arithmetic-shift (symbol->gtype% (case sym
                                                   ((utf8) 'string)
                                                   (else sym)))
                                 *fundamental-shift*))))

  (define pointer-gtype-ref
    (let ((ref (make-pointer-c-getter gtype-ctype)))
      (lambda (ptr i)
        (gtype->symbol (ref ptr i)))))

  (define pointer-gtype-set!
    (let ((set (make-pointer-c-setter gtype-ctype)))
      (lambda (ptr i v)
        (set ptr i (if (integer? v) v (symbol->gtype v))))))
  
  (define g-type-init (let-callouts libgobject ((init% 'void "g_type_init" '()))
                        init%)))
