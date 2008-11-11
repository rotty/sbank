;;; basic.sls --- C type utilities.

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

;; These utilities are "simple" in the sense that they don't use GType
;; machinery or any shlibs.

;;; Code:

(library (sbank ctypes basic)
  (export vector->c-array ->c-array
          c-array-for-each
          free-c-array
          c-array->vector

          type-info->prim-type
          type-info/prim-type+procs
          out-converter/null
          back-converter/null

          value->gtype
          type->gtype

          deref-pointer
          set-pointer

          type-tag-symbol->prim-type
          type-tag->symbol
          symbol->type-tag

          raise-sbank-callout-error raise-sbank-callback-error

          null-ok-always-on?)
  (import (rnrs)
          (xitomatl srfi and-let*)
          (spells receive)
          (spells parameter)
          (spells foreign)
          (spells tracing)
          (sbank utils)
          (sbank type-data)
          (sbank conditions)
          (sbank stypes)
          (sbank typelib stypes)
          (sbank gobject gtype)
          (sbank gobject gvalue)
          (sbank gobject internals))

  (define (raise-sbank-callout-error msg . irritants)
    (raise (condition (make-sbank-callout-error)
                      (make-message-condition msg)
                      (make-irritants-condition irritants))))

  (define (raise-sbank-callback-error msg . irritants)
    (raise (condition (make-sbank-callback-error)
                      (make-message-condition msg)
                      (make-irritants-condition irritants))))

  (define-syntax define-accessors (stype-accessor-definer (typelib-stypes)))

  (define-accessors "GTypeInstance"
    (gtype-instance-class "g_class"))

  (define-accessors "GTypeClass"
    (gtype-class-gtype "g_type"))

  (define null-ok-always-on? (make-parameter #f))

  (define (type-info/prim-type+procs ti gtype-lookup)
    (let ((type (type-info-type ti))
          (null-ok? (type-info-null-ok? ti)))
      (cond
       ((symbol? type)
        (let ((prim-type (type-tag-symbol->prim-type type)))
          (case type
            ((int8 uint8 int16 uint16 int32 uint32
                   int64 uint64 int uint long ulong ssize size
                   float double)
             (values prim-type #f #f #f))
            ((boolean)
             (values prim-type
                     (lambda (val) (if val 1 0))
                     (lambda (val) (not (= val 0)))
                     #f))
            ((utf8)
             (values prim-type
                     (out-converter/null string->utf8z-ptr null-ok? #f)
                     (back-converter/null utf8z-ptr->string null-ok? #f)
                     free))
            ((void)
             (values 'void #f #f #f))
            ((pointer)
             (values 'pointer #f #f #f))
            ((gvalue)
             (values 'pointer
                     (lambda (v) (if (pointer? v) v (->g-value v #f)))
                     g-value-ref
                     #f))
            ((gtype)
             (values prim-type symbol->gtype gtype->symbol #f))
            (else
             (raise-sbank-callout-error "argument passing for this type not implemented" ti)))))
       ((genum? type)
        (values 'int                    ; Is int always OK?
                (lambda (val)
                  (if (symbol? val)
                      (or (genum-lookup type val)
                          (raise-sbank-callout-error
                           "invalid enumeration value" val (genum-symbols type)))
                      val))
                (lambda (val)
                  (or (genum-lookup type val) val))
                #f))
       ((array-type? type)
        (unless (or (array-size type) (array-is-zero-terminated? type))
          (raise-sbank-callout-error "cannot handle array without size information" type))
        (values 'pointer
                (out-converter/null ->c-array null-ok? #f)
                (back-converter/null c-array->vector null-ok? #f)
                (lambda (ptr)
                  (free-c-array ptr type #f))))
       ((gobject-class? type)
        (values 'pointer
                (out-converter/null ginstance-ptr null-ok? #f)
                (back-converter/null (ginstance-maker gtype-lookup type)
                                     null-ok?
                                     #f)
                #f))
       (else
        (raise-sbank-callout-error "argument/return type not yet implemented" type)))))

  (define (ginstance-maker gtype-lookup declared-class)
    (cond ((or (gobject-record-class? declared-class)
               (gobject-union-class? declared-class))
           (lambda (instance)
             (make-ginstance declared-class instance)))
          (else
           (lambda (instance)
             (let ((class
                     (or
                      (and-let* ((gtype (gtype-class-gtype (gtype-instance-class instance))))
                        (gtype-lookup gtype))
                      declared-class)))
               (make-ginstance class instance))))))


  ;; This returns and expects a converter that returns a pointer
  ;; (i.e. a converter for calling *out* to C); don't confuse this
  ;; with out-arguments, which are collected using a "back-converter"
  ;; (which is converting back to Scheme).
  (define (out-converter/null convert null-ok? null-val)
    (if (or (null-ok-always-on?) null-ok?)
        (lambda (val)
          (if (equal? val null-val)
              (integer->pointer 0)
              (convert val)))
        convert))

  (define (back-converter/null convert null-ok? null-val)
    (if (or (null-ok-always-on?) null-ok?)
        (lambda (ptr)
          (if (= (pointer->integer ptr) 0)
              null-val
              (convert ptr)))
        (lambda (ptr)
          (when (= (pointer->integer ptr) 0)
            (raise-sbank-callout-error "unexpect NULL pointer when converting back to Scheme"))
          (convert ptr))))

  ;; GValue utilities
  (define (value->gtype value)
    (cond ((ginstance? value) (gobject-class-gtype (ginstance-class value)))
          ((boolean? value)   'boolean)
          ((integer? value)   'int)
          ((number? value)    'double)
          ((string? value)    'string)))

  (define (type->gtype type)
    (cond ((genum? type)         (genum-gtype type))
          ((gobject-class? type) (gobject-class-gtype type))
          (else type)))

  ;; Note that these must match with gobject-introspection
  (define-enum (type-tag->symbol symbol->type-tag)
    (void boolean int8 uint8 int16 uint16 int32 uint32
          int64 uint64 int uint long ulong ssize size
          float double time_t gtype utf8 filename
          array interface glist gslist ghash error))

  (define (type-tag-symbol->prim-type sym)
    (case sym
      ((boolean) 'uint)
      ((utf8) 'pointer)
      ((gtype) gtype-ctype)
      (else sym)))


  (define (vector->c-array vec atype)
    (let ((len (vector-length vec)))
      (receive (prim-type element-size element-ref element-set!) (array-type-values atype)
        (let ((mem (malloc (* element-size (+ len (if (array-is-zero-terminated? atype) 1 0))))))
          (do ((i 0 (+ i 1)))
              ((>= i len))
            (element-set! mem (* i element-size) (vector-ref vec i)))
          (when (array-is-zero-terminated? atype)
            (element-set! mem
                          (* element-size len)
                          (array-terminator prim-type)))
          mem))))

  (define (c-array->vector ptr atype size)
    (receive (prim-type element-size element-ref element-set!) (array-type-values atype)
      (cond ((or size (array-size atype))
             => (lambda (size)
                  (do ((vec (make-vector size))
                       (i 0 (+ i 1)))
                      ((>= i size) vec)
                    (vector-set! vec i (element-ref ptr (* i element-size))))))
            ((array-is-zero-terminated? atype)
             (let ((terminator? (array-terminator-predicate atype)))
               (let loop ((offset 0) (elts '()))
                 (if (terminator? offset)
                     (list->vector (reverse elts))
                     (loop (+ offset element-size) (cons (element-ref ptr offset) elts))))))
            (else
             (error 'c-array->vector "cannot handle array without size information" atype)))))

  (define (->c-array val atype)
    (vector->c-array (->vector val) atype))

  (define (array-terminator prim-type)
    (case prim-type
      ((char uchar short ushort int uint long ulong size_t) 0)
      ((pointer) (integer->pointer 0))
      (else
       (raise-sbank-callout-error
        "zero-termination of arrays of this type not implemented" prim-type))))

  (define (array-terminator-predicate elt-prim-type)
    (let ((terminator (array-terminator elt-prim-type))
          (elt-ref (make-pointer-c-getter elt-prim-type)))
      (if (pointer? terminator)
          (lambda (ptr offset)
            (= (pointer->integer (pointer-ref-c-pointer ptr offset))
               (pointer->integer terminator)))
          (lambda (ptr offset)
            (= (elt-ref ptr offset) terminator)))))

  (define (c-array-for-each proc ptr atype size)
    (let ((element-size (array-element-size atype))
          (element-ti (array-element-type-info atype)))
      (cond ((or size (array-size atype))
             => (lambda (size)
                  (do ((i 0 (+ i 1)))
                      ((>= i size))
                    (proc (deref-pointer (pointer+ ptr (* i element-size)) element-ti)))))
            ((array-is-zero-terminated? atype)
             (let ((terminator? (array-terminator-predicate atype)))
               (let loop ((offset 0) (elts '()))
                 (unless (terminator? ptr offset)
                   (proc (deref-pointer (pointer+ ptr offset) element-ti))
                   (loop (+ offset element-size))))))
            (else
             (raise-sbank-callout-error "cannot iterate over array of unknown size" ptr atype)))))

  (define (free-c-array ptr atype size)
    (let ((cleanup (type-cleanup (array-element-type atype))))
      (when cleanup
        (c-array-for-each cleanup ptr atype size)
        (free ptr))))

  (define (type-cleanup type)
    (define (lose)
      (raise-sbank-callout-error "cleanup for this type not implemented" type))
    (cond ((symbol? type)
           (case type
             ((int8 uint8 int16 uint16 int32 uint32
                    int64 uint64 int uint long ulong ssize size
                    float double gtype)
              #f)
             ((utf8) free)
             ((gvalue) g-value-unset!)
             (else (lose))))
          ((genum? type) #f)
          ((array-type? type)
           (lambda (ptr)
             (free-c-array ptr type #f)))
          (else
           (lose))))

  (define array-type-values
    (let ((gvalue-zero-bytes (make-bytevector g-value-size 0)))
      (lambda (atype)
        (let* ((eti (array-element-type-info atype))
               (et (type-info-type eti)))
          (cond ((symbol? et)
                 (let* ((prim-type (type-tag-symbol->prim-type et))
                        (elt-size (array-element-size atype)))
                   (case et
                     ((utf8)
                      (values prim-type elt-size pointer-utf8z-ptr-ref pointer-utf8z-ptr-set!))
                     ((gtype)
                      (values prim-type
                              elt-size
                              pointer-gtype-ref
                              pointer-gtype-set!))
                     ((gvalue)
                      (values prim-type
                              elt-size
                              (lambda (ptr i)
                                (g-value-ref (pointer+ ptr i)))
                              (lambda (ptr i v)
                                (let ((gvalue (pointer+ ptr i)))
                                  (memcpy gvalue gvalue-zero-bytes g-value-size)
                                  (g-value-init! gvalue (value->gtype v))
                                  (g-value-set! gvalue v)))))
                     (else
                      (values prim-type
                              elt-size
                              (make-pointer-c-getter prim-type)
                              (make-pointer-c-setter prim-type))))))
                ((gobject-class? et)
                 (values 'pointer
                         (c-type-sizeof 'pointer)
                         (lambda (ptr i)
                           (make-ginstance et (pointer-ref-c-pointer ptr i)))
                         (lambda (ptr i v)
                           (pointer-set-c-pointer! ptr i (if (ginstance? v)
                                                             (ginstance-ptr v)
                                                             (ginstance-ptr
                                                              (send et (new v))))))))
                (else
                 (raise-sbank-callout-error "non-simple array element types not yet supported")))))))

  ;; Retrieve a Scheme representation of the memory pointed to by @1,
  ;; according to the type @2.
  (define (deref-pointer ptr ti)
    (let ((type (type-info-type ti)))
      (cond ((symbol? type)
             (case type
               ((gvalue) ptr)
               (else
                ((make-pointer-c-getter (type-tag-symbol->prim-type type)) ptr 0))))
            ((or (array-type? type) (gobject-class? type))
             (pointer-ref-c-pointer ptr 0))
            (else
             (error 'deref-pointer "not implemented for that type" ptr type)))))

  (define (set-pointer ptr ti val)
    (let ((type (type-info-type ti)))
      (cond ((symbol? type)
             ((make-pointer-c-setter (type-tag-symbol->prim-type type)) ptr 0 val))
            (else
             (error 'set-pointer "not implemented for that type" ptr ti)))))

  (define (array-element-size atype)
    (let ((eti (array-element-type-info atype)))
      (case (type-info-type eti)
        ((gvalue) g-value-size)
        ((gtype) gtype-size)
        (else
         (c-type-sizeof (type-info->prim-type eti #f))))))

  (define (type-info->prim-type ti out?)
    (let ((type (type-info-type ti)))
      (cond ((or out?
                 (type-info-is-pointer? ti)
                 (array-type? type)
                 (gobject-class? type))
             'pointer)
            ((genum? type)
             'int)
            ((symbol? type)
             (case type
               ((gtype) gtype-ctype)
               ((boolean) 'uint)
               (else type)))
            (else
             (raise-sbank-callout-error "argument/return type not yet implemented" type))))))