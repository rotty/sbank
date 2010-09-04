;;; gtype.sls --- Low-level access to GType

;; Copyright (C) 2008-2010 Andreas Rottmann <a.rottmann@gmx.at>

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
#!r6rs

(library (sbank gobject gtype)
  (export symbol->gtype gtype->symbol
          pointer-gtype-ref pointer-gtype-set!
          gtype-ctype gtype-size
          g-type-init
          g-boxed-value-type

          g-boxed-free
          g-boxed-copy
          
          register-gtype-lookup! gtype-lookup)
  (import (rnrs base)
          (rnrs control)
          (rnrs arithmetic bitwise)
          (spells define-values)
          (spells misc)
          (spells foreign)
          (spells tracing)
          (sbank support utils)
          (sbank support shlibs))

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

  (define gtype-ctype 'size_t)
  (define gtype-size (c-type-sizeof gtype-ctype))

  (define gtype->symbol
    (lambda (gtype)
      (if (= gtype (g-boxed-value-type))
          'boxed
          (gtype->symbol% (bitwise-arithmetic-shift-right
                           (fundamental% gtype)
                           *fundamental-shift*)))))

  (define (symbol->gtype sym)
    (case sym
      ((boxed) (g-boxed-value-type))
      (else
       (bitwise-arithmetic-shift (symbol->gtype% (case sym
                                                   ((utf8) 'string)
                                                   ((int32) 'int)
                                                   ((uint32) 'uint)
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

  (define g-boxed-value-type
    (let ((type #f))
      (lambda ()
        (unless type
          (let ((name-ptr (string->utf8z-ptr "boxed-scm")))
            (set! type (g-pointer-type-register-static% name-ptr))
            (free name-ptr)))
        type)))

  (define-values (register-gtype-lookup! gtype-lookup)
    (let ((lookup-registry '()))
      (values
       (lambda (lookup)
         (set! lookup-registry (cons lookup lookup-registry)))
       (lambda (gtype)
         (or-map (lambda (lookup)
                   (lookup gtype))
                 lookup-registry)))))

  (define-c-callouts libgobject
    (g-type-init 'void "g_type_init" '())
    (g-pointer-type-register-static%
     gtype-ctype "g_pointer_type_register_static" '(pointer))
    (fundamental% gtype-ctype "g_type_fundamental" (list gtype-ctype))
    (g-boxed-copy 'pointer "g_boxed_copy" (list gtype-ctype 'pointer))
    (g-boxed-free 'void "g_boxed_free" (list gtype-ctype 'pointer)))

  )
