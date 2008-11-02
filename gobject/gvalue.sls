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
  (export g-value-new
          g-value-set!
          g-value-ref
          g-value-free
          ->g-value)
  (import (rnrs base)
          (rnrs bytevectors)
          (spells foreign)
          (sbank shlibs)
          (sbank ctypes)
          (sbank type-data)
          (sbank gobject internals)
          (sbank gobject gtype)
          (sbank typelib stypes)
          (sbank stypes))

  (define-syntax define-accessors (stype-accessor-definer (typelib-stypes)))
  (define-syntax define-attributes (stype-attribute-definer (typelib-stypes)))

  (define-accessors "GValue"
    (gvalue-gtype% "g_type"))

  (define-attributes "GValue"
    (gvalue-size size))
  
  (define g-value-new
    (let-callouts libgobject ((init% 'pointer "g_value_init" `(pointer ,gtype-ctype)))
      (let ((zero-bytes (make-bytevector gvalue-size 0)))
        (lambda (gtype)
          (let ((mem (memcpy (malloc gvalue-size) zero-bytes gvalue-size)))
            (init% mem (symbol->gtype gtype)))))))

  (define g-value-free
    (let-callouts libgobject ((unset% 'void "g_value_unset" '(pointer)))
      (lambda (gvalue)
        (unset% gvalue)
        (free gvalue))))

  (define (value-gtype value type)
    (cond ((ginstance? value) 'object)
          ((boolean? value)   'boolean)
          ((integer? value)   'int)
          ((number? value)    'double)
          (else
           (cond ((genum? type) 'enum)
                 (else
                  (error 'value-gtype "not implemented for this type of value" value type))))))
  
  (define (->g-value val type)
    (let ((gvalue (g-value-new (value-gtype val type))))
      (g-value-set! gvalue val type)
      gvalue))
  
  (define g-value-set!
    (let-callouts libgobject ((set-object% 'void "g_value_set_object" '(pointer pointer))
                              (set-bool% 'void "g_value_set_boolean" '(pointer int))
                              (set-enum% 'void "g_value_set_enum" '(pointer int))
                              (set-int% 'void "g_value_set_int" '(pointer int)))
      (lambda (gvalue val type)
        (cond
         ((ginstance? val)
          (set-object% gvalue (ginstance-ptr val)))
         ((boolean? val)
          (set-bool% gvalue (if val 1 0)))
         ((integer? val)
          (set-int% gvalue val))
         ((genum? type)
          (set-enum% gvalue (genum-lookup type val)))
         (else
          (error 'g-value-set! "not implemented for this type of value" val type))))))

  (define g-value-ref
    (let-callouts libgobject ((get-object% 'pointer "g_value_get_object" '(pointer pointer)))
      (lambda (gvalue)
        #f))))
