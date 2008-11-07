;;; gvalue.sls --- Low-level access to GValue

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


(library (sbank gobject gvalue)
  (export g-value-alloc
          g-value-size
          g-value-init!
          g-value-new
          g-value-set!
          g-value-ref
          g-value-unset!
          g-value-free
          g-value-typeof
          ->g-value)
  (import (rnrs)
          (spells foreign)
          (spells table)
          (spells define-values)
          (spells tracing)
          (only (spells assert) cout)
          (sbank shlibs)
          (sbank ctypes simple)
          (sbank type-data)
          (sbank gobject internals)
          (sbank gobject gtype)
          (sbank gobject boxed-values)
          (sbank typelib stypes)
          (sbank stypes))

  (define-syntax define-accessors (stype-accessor-definer (typelib-stypes)))
  (define-syntax define-attributes (stype-attribute-definer (typelib-stypes)))

  (define-accessors "GValue"
    (gvalue-gtype% "g_type"))

  (define-attributes "GValue"
    (g-value-size size))

  (define (g-value-alloc n)
    (let ((n-bytes (* n g-value-size)))
      (memcpy (malloc g-value-size) (make-bytevector n-bytes 0) n-bytes)))
  
  (define g-value-init!
    (let-callouts libgobject ((init% 'pointer "g_value_init" `(pointer ,gtype-ctype)))
      (lambda (gvalue gtype)
        (let ((gtype-val (cond ((integer? gtype) gtype)
                               ((genum? gtype) (genum-gtype gtype))
                               ((gobject-class? gtype) (gobject-class-gtype gtype))
                               (else
                                (case gtype
                                  ((boxed) (g-boxed-value-type))
                                  (else    (symbol->gtype gtype)))))))
          (init% gvalue gtype-val)))))

  (define (g-value-new gtype)
    (g-value-init! (g-value-alloc 1) gtype))
  
  (define g-value-unset!
    (let-callouts libgobject ((unset% 'void "g_value_unset" '(pointer)))
      unset%))

  (define (g-value-free gvalue)
    (g-value-unset! gvalue)
    (free gvalue))
  
  (define (g-value-typeof value type)
    (cond ((ginstance? value) 'object)
          ((boolean? value)   'boolean)
          ((integer? value)   'int)
          ((number? value)    'double)
          ((string? value)    'string)
          (else
           (cond ((genum? type) (genum-gtype type))
                 (else
                  (g-boxed-value-type))))))
  
  (define (->g-value val type)
    (let ((gvalue (g-value-new (g-value-typeof val type))))
      (g-value-set! gvalue val type)
      gvalue))

  (define-values (register-value lookup-value)
    (let ((max-val (bitwise-arithmetic-shift 1 (* 8 (c-type-sizeof 'pointer))))
          (registered-values (make-table 'eqv))
          (count 0))
      (define (new-count)
        (when (= count max-val)
          (error 'register-value
                 "oops, out of value identifiers -- time someone implements reclaiming them"))
        (+ count 1))
      (values
       (lambda (val)
         (let ((val-count count))
           (table-set! registered-values count val)
           (set! count (new-count))
           val-count))
       (lambda (val failure-thunk)
         (table-ref registered-values val failure-thunk)))))
  
  (define g-value-set!
    (let-callouts libgobject ((set-object% 'void "g_value_set_object" '(pointer pointer))
                              (set-bool% 'void "g_value_set_boolean" '(pointer int))
                              (set-enum% 'void "g_value_set_enum" '(pointer int))
                              (set-int% 'void "g_value_set_int" '(pointer int))
                              (set-string% 'void "g_value_set_string" '(pointer pointer))
                              (set-pointer% 'void "g_value_set_pointer" '(pointer pointer)))
      (lambda (gvalue val type)
        (cond
         ((ginstance? val)
          (set-object% gvalue (ginstance-ptr val)))
         ((boolean? val)
          (set-bool% gvalue (if val 1 0)))
         ((integer? val)
          (set-int% gvalue val))
         ((string? val)
          (let ((utf8z (string->utf8z-ptr val)))
            (set-string% gvalue utf8z)
            (free utf8z)))
         ((genum? type)
          (set-enum% gvalue
                     (if (symbol? val)
                         (or (genum-lookup type val)
                             (error 'g-value-set! "invalid value for enumeration" val type))
                         val)))
         (else
          (set-pointer% gvalue (integer->pointer (register-value val))))))))

  (define g-value-ref
    (let-callouts libgobject ((get-object% 'pointer "g_value_get_object" '(pointer))
                              (get-pointer% 'pointer "g_value_get_pointer" '(pointer))
                              (get-bool% 'int "g_value_get_boolean" '(pointer))
                              (get-string% 'pointer "g_value_get_string" '(pointer))
                              (get-int% 'int "g_value_get_int" '(pointer)))
      (define (lose msg . irritants)
        (apply error 'g-value-ref msg irritants))
      (lambda (gvalue type)
        (let ((gtype (gvalue-gtype% gvalue)))
          (cond ((= gtype (g-boxed-value-type))
                 (let ((i (pointer->integer (get-pointer% gvalue))))
                   (lookup-value i (lambda () (lose "could not find boxed value" i)))))
                (else
                 (case (gtype->symbol gtype)
                   ((int)
                    (get-int% gvalue))
                   ((boolean)
                    (not (= 0 (get-bool% gvalue))))
                   ((object)
                    (assert (gobject-class? type))
                    (make-ginstance type (get-object% gvalue)))
                   ((pointer)
                    (get-pointer% gvalue))
                   ((string)
                    (utf8z-ptr->string (get-string% gvalue)))
                   (else
                    (lose "not implemented for this type of value" (gtype->symbol gtype)))))))))))
