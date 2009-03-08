;;; gvalue.sls --- Low-level access to GValue

;; Copyright (C) 2008, 2009 Andreas Rottmann <a.rottmann@gmx.at>

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

(library (sbank gobject gvalue)
  (export g-value-alloc
          g-value-size
          g-value-init!
          g-value-new
          g-value-set!
          g-value-ref
          g-value-unset!
          g-value-free
          ->g-value

          ->g-value-array
          free-g-value-array)
  (import (for (rnrs) run expand)
          (srfi :2 and-let*)
          (spells foreign)
          (srfi :8 receive)
          (spells table)
          (spells define-values)
          (spells tracing)
          (only (spells assert) cout)
          (sbank support shlibs)
          (sbank support type-data)
          (sbank gobject genum)
          (sbank gobject gtype)
          (for (sbank typelib stypes) expand)
          (for (sbank support stypes) expand))

  (define-syntax define-accessors (stype-accessor-definer (typelib-stypes)))
  (define-syntax define-attributes (stype-attribute-definer (typelib-stypes)))

  (define-accessors "GValue"
    (g-value-gtype% "g_type"))

  (define-attributes "GValue"
    (g-value-size size))

  (define (g-value-alloc n)
    (let ((n-bytes (* n g-value-size)))
      (memset (malloc n-bytes) 0 n-bytes)))

  (define-callouts libgobject
    (g-value-unset! 'void "g_value_unset" '(pointer))
    (g-value-init% 'pointer "g_value_init" `(pointer ,gtype-ctype)))

  (define (g-value-init! gvalue type)
    (let ((gtype (if (symbol? type)
                     (symbol->gtype type)
                     type)))
      (g-value-init% gvalue gtype)))

  (define (g-value-new gtype)
    (g-value-init! (g-value-alloc 1) gtype))

  (define (g-value-free gvalue)
    (g-value-unset! gvalue)
    (free gvalue))

  (define (->g-value val gtype)
    (let ((gvalue (g-value-new gtype)))
      (g-value-set! gvalue val)
      gvalue))

  (define-values (register-value lookup-value)
    (let ((max-val (bitwise-arithmetic-shift 1 (* 8 (c-type-sizeof 'pointer))))
          (registered-values (make-table 'eqv))
          (count 0))
      (define (new-count)
        (unless (< count max-val)
          (error 'register-value "oops, out of value identifiers"))
        (+ count 1))
      (values
       (lambda (val)
         (let ((val-count count))
           (table-set! registered-values count val)
           (set! count (new-count))
           val-count))
       (lambda (val failure-thunk)
         (table-ref registered-values val failure-thunk)))))

  (define (to-ptr x)
    (if (eqv? x #f) (null-pointer) x))

  (define (from-ptr x)
    (if (null-pointer? x) #f x))

  (define g-value-set!
    (let-callouts libgobject
        ((set-object% 'void "g_value_set_object" '(pointer pointer))
         (set-bool% 'void "g_value_set_boolean" '(pointer int))
         (set-enum% 'void "g_value_set_enum" '(pointer int))
         (set-uint% 'void "g_value_set_uint" '(pointer uint))
         (set-int% 'void "g_value_set_int" '(pointer int))
         (set-string% 'void "g_value_set_string" '(pointer pointer))
         (set-pointer% 'void "g_value_set_pointer" '(pointer pointer)))
      (define (lose msg . irritants)
        (apply error 'g-value-set! msg irritants))
      (lambda (gvalue val)
        (let ((gtype (g-value-gtype% gvalue)))
          (define (enum->integer val)
            (cond ((integer? val) val)
                  (else
                   (or (and-let* ((enum-lookup (find-enum-lookup gtype)))
                         (enum-lookup val))
                       (lose "enum lookup failed" val gtype)))))
          (case (gtype->symbol (g-value-gtype% gvalue))
            ((int) (set-int% gvalue val))
            ((uint) (set-uint% gvalue val))
            ((boolean) (set-bool% gvalue (if val 1 0)))
            ((object) (set-object% gvalue (to-ptr val)))
            ((string)
             (let ((utf8z (string->utf8z-ptr val)))
               (set-string% gvalue utf8z)
               (free utf8z)))
            ((enum)
             (set-enum% gvalue (enum->integer val)))
            ((pointer) (set-pointer% gvalue (to-ptr val)))
            ((boxed)
             (let ((ptr (malloc 4)))
               (pointer-uint32-set! ptr 0 (register-value val))
               (set-pointer% gvalue ptr)))
            (else
             (lose "type not implemented"
                   (gtype->symbol (g-value-gtype% gvalue)))))))))

  (define (find-enum-lookup gtype)
    (and-let* ((type (gtype-lookup gtype)))
      (and (genum? type)
           (lambda (val)
             (genumerated-lookup type val)))))

  (define g-value-ref
    (let-callouts libgobject
        ((get-object% 'pointer "g_value_get_object" '(pointer))
         (get-pointer% 'pointer "g_value_get_pointer" '(pointer))
         (get-bool% 'int "g_value_get_boolean" '(pointer))
         (get-string% 'pointer "g_value_get_string" '(pointer))
         (get-int% 'int "g_value_get_int" '(pointer))
         (get-uint% 'uint "g_value_get_uint" '(pointer))
         (get-enum% 'int "g_value_get_enum" '(pointer)))
      (define (lose msg . irritants)
        (apply error 'g-value-ref msg irritants))
      (lambda (gvalue)
        (let ((gtype (g-value-gtype% gvalue)))
          (cond ((= gtype (g-boxed-value-type))
                 (let ((i (pointer-uint32-ref (get-pointer% gvalue) 0)))
                   (lookup-value i (lambda ()
                                     (lose "could not find boxed value" i)))))
                (else
                 (case (gtype->symbol gtype)
                   ((int)
                    (get-int% gvalue))
                   ((uint)
                    (get-uint% gvalue))
                   ((boolean)
                    (not (= 0 (get-bool% gvalue))))
                   ((object)
                    (from-ptr (get-object% gvalue)))
                   ((pointer)
                    (from-ptr (get-pointer% gvalue)))
                   ((string)
                    (utf8z-ptr/null->string (get-string% gvalue)))
                   ((enum)
                    (let ((val (get-enum% gvalue)))
                      (cond ((find-enum-lookup gtype)
                             => (lambda (lookup)
                                  (or (lookup val) val)))
                            (else
                             val))))
                   (else
                    (lose "not implemented for this type of value"
                          (gtype->symbol gtype))))))))))

  (define (utf8z-ptr/null->string ptr)
    (convert/null ptr utf8z-ptr->string #f))

  (define (convert/null ptr convert null-val)
    (if (null-pointer? ptr)
        null-val
        (convert ptr)))

  (define (free-g-value-array array size)
    (do ((i 0 (+ i 1)))
        ((>= i size))
      (g-value-unset! (pointer+ array (* i g-value-size))))
    (free array))

  (define (->g-value-array x value-gtype types)
    (define (lose msg . irritants)
      (apply error '->g-value-array msg irritants))
    (unless (or value-gtype types )
      (lose "neither value-gtype procedure nor element types given" x))
    (receive (len next init)
             (cond ((vector? x)
                    (values (vector-length x)
                            (lambda (i)
                              (values (vector-ref x i) (+ i 1)))
                            0))
                   ((list? x)
                    (values (length x)
                            (lambda (vals)
                              (values (car vals) (cdr vals)))
                            x))
                   (else
                    (lose "cannot convert to array" x)))
      (let ((array (g-value-alloc len)))
        (let loop ((i 0) (state init) (types types))
          (if (>= i len)
              array
              (let ((gv (pointer+ array (* i g-value-size))))
                (receive (val new-state) (next state)
                  (g-value-init! gv (or (and types (car types))
                                        (value-gtype val)))
                  (g-value-set! gv val)
                  (loop (+ i 1) new-state (and types (cdr types)))))))))))
