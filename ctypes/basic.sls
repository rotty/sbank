;;; basic.sls --- C type utilities.

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

;; These utilities are "simple" in the sense that they don't use GType
;; machinery or any shlibs.

;;; Code:
#!r6rs

(library (sbank ctypes basic)
  (export vector->c-array ->c-array
          c-array-for-each
          free-c-array
          c-array->vector

          bytevector-portion bytevector-portion? bytevector-portion-count
          malloc/set!

          out-converter/null
          back-converter/null

          value->gtype
          type->gtype
          type-info-gtype

          string->fnamez-ptr
          fnamez-ptr->string
          
          deref-pointer
          set-pointer

          type-tag-symbol->prim-type
          type-tag->symbol
          symbol->type-tag

          scope->symbol
          symbol->scope

          raise-sbank-callout-error raise-sbank-callback-error

          raise-gerror/free
          gerror-conditions/free

          always-allow-none?)
  (import (rnrs)
          (srfi :2 and-let*)
          (srfi :8 receive)
          (srfi :39 parameters)
          (spells foreign)
          (spells tracing)
          (sbank support utils)
          (sbank support type-data)
          (sbank support conditions)
          (for (sbank support stypes) expand)
          (sbank support shlibs)
          (for (sbank typelib stypes) expand)
          (sbank gobject gtype)
          (sbank gobject genum)
          (sbank gobject gvalue)
          (sbank gobject glist)
          (sbank gobject internals data))

  (define (raise-sbank-callout-error msg . irritants)
    (raise (condition (make-sbank-callout-error)
                      (make-message-condition msg)
                      (make-irritants-condition irritants))))

  (define (raise-sbank-callback-error msg . irritants)
    (raise (condition (make-sbank-callback-error)
                      (make-message-condition msg)
                      (make-irritants-condition irritants))))

  (define-syntax define-accessors (stype-accessor-definer (typelib-stypes)))

  (define always-allow-none? (make-parameter #f))

  (define-record-type (<bytevector-portion> bytevector-portion bytevector-portion?)
    (fields (immutable bv bytevector-portion-bv)
            (immutable start bytevector-portion-start)
            (immutable count bytevector-portion-count)))

  ;; This returns and expects a converter that returns a pointer
  ;; (i.e. a converter for calling *out* to C); don't confuse this
  ;; with out-arguments, which are collected using a "back-converter"
  ;; (which is converting back to Scheme).
  (define (out-converter/null convert null-ok? null-val)
    (if (or (always-allow-none?) null-ok?)
        (lambda (val)
          (if (equal? val null-val)
              (null-pointer)
              (convert val)))
        convert))

  (define (back-converter/null convert null-ok? null-val)
    (if (or (always-allow-none?) null-ok?)
        (lambda (ptr)
          (if (null-pointer? ptr)
              null-val
              (convert ptr)))
        (lambda (ptr)
          (when (null-pointer? ptr)
            (raise-sbank-callout-error
             "unexpect NULL pointer when converting back to Scheme"))
          (convert ptr))))

  ;; GValue utilities
  (define (value->gtype value)
    (cond ((ginstance? value) (gobject-class-gtype (ginstance-class value)))
          ((boolean? value)   'boolean)
          ((integer? value)   'int)
          ((number? value)    'double)
          ((string? value)    'string)))

  (define (type->gtype type)
    (cond ((genum? type)         (genumerated-gtype type))
          ((gobject-class? type) (gobject-class-gtype type))
          (else type)))

  (define (type-info-gtype ti)
    (type->gtype (type-info-type ti)))

  ;; Note that these must match with gobject-introspection,
  ;; (see girepository.h, GITypeTag)
  (define-enum (type-tag->symbol symbol->type-tag)
    (void boolean int8 uint8 int16 uint16 int32 uint32
          int64 uint64 short ushort int uint long ulong ssize size
          float double time_t gtype utf8 filename
          array interface glist gslist ghash error))

  (define (type-tag-symbol->prim-type sym)
    (case sym
      ((boolean) 'uint)
      ((size) 'size_t)
      ((ssize) 'ssize_t)
      ((utf8) 'pointer)
      ((gtype) gtype-ctype)
      (else sym)))

  ;; Must be in sync with GIScopeType
  (define-enum (scope->symbol symbol->scope)
    (invalid call async notified))

  (define (vector->c-array vec atype)
    (let ((len (vector-length vec)))
      (receive (prim-type element-size element-ref element-set!)
               (array-type-values atype)
        (let ((mem (malloc (* element-size
                              (+ len (if (array-is-zero-terminated? atype) 1 0))))))
          (do ((i 0 (+ i 1)))
              ((>= i len))
            (element-set! mem (* i element-size) (vector-ref vec i)))
          (when (array-is-zero-terminated? atype)
            (element-set! mem
                          (* element-size len)
                          (array-terminator prim-type)))
          mem))))

  (define (c-array->vector ptr atype size)
    (receive (prim-type element-size element-ref element-set!)
             (array-type-values atype)
      (let ((is-bytevector? (eq? prim-type 'uint8)))
        (cond ((or size (array-size atype))
               => (lambda (size)
                    (if is-bytevector?
                        (memcpy (make-bytevector size) ptr size)
                        (do ((vec (make-vector size))
                             (i 0 (+ i 1)))
                            ((>= i size) vec)
                          (vector-set! vec
                                       i
                                       (element-ref ptr (* i element-size)))))))
              ((array-is-zero-terminated? atype)
               (let ((terminator? (array-terminator-predicate prim-type)))
                 (let loop ((offset 0) (elts '()))
                   (if (terminator? ptr offset)
                       (if is-bytevector?
                           (u8-list->bytevector (reverse elts))
                           (list->vector (reverse elts)))
                       (loop (+ offset element-size)
                             (cons (element-ref ptr offset) elts))))))
              (else
               (error 'c-array->vector
                      "cannot handle array without size information" atype))))))

  (define (->c-array val atype)
    (define (lose msg . irritants)
      (apply error '->c-array msg irritants))
    (let ((elt-type (array-element-type atype)))
      (cond ((string? val)
             (unless (memq elt-type '(int8 uint8))
               (lose "cannot convert string to array of this type" val atype))
             (let* ((bytes (string->utf8 val))
                    (size (bytevector-length bytes)))
               (memcpy (malloc size) bytes size)))
            ((bytevector? val)
             (unless (memq elt-type '(int8 uint8))
               (lose "cannot convert bytevector to array of this type" val atype))
             (let ((size (bytevector-length val)))
               (memcpy (malloc size) val size)))
            ((bytevector-portion? val)
             (let ((count (bytevector-portion-count val)))
               (memcpy (malloc count)
                       (bytevector-portion-bv val)
                       (bytevector-portion-start val)
                       count)))
            (else
             (vector->c-array (->vector val) atype)))))

  (define (array-terminator prim-type)
    (case prim-type
      ((char uchar short ushort int uint long ulong size_t) 0)
      ((pointer utf8) (null-pointer))
      (else
       (raise-sbank-callout-error
        "zero-termination of arrays of this type not implemented" prim-type))))

  (define (array-terminator-predicate elt-prim-type)
    (let ((terminator (array-terminator elt-prim-type))
          (elt-ref (make-pointer-c-getter elt-prim-type)))
      (if (pointer? terminator) ;; FIXME:
          (lambda (ptr offset)
            (pointer=? (pointer-ptr-ref ptr offset)
                       terminator))
          (lambda (ptr offset)
            (= (elt-ref ptr offset) terminator)))))

  (define (c-array-for-each proc ptr atype size)
    (let ((element-size (array-element-size atype))
          (element-ti (array-element-type-info atype)))
      (cond ((or size (array-size atype))
             => (lambda (size)
                  (do ((i 0 (+ i 1)))
                      ((>= i size))
                    (proc (deref-pointer (pointer+ ptr (* i element-size))
                                         element-ti)))))
            ((array-is-zero-terminated? atype)
             (let ((terminator? (array-terminator-predicate
                                 (type-info->prim-type element-ti #f))))
               (let loop ((offset 0))
                 (unless (terminator? ptr offset)
                   (proc (deref-pointer (pointer+ ptr offset) element-ti))
                   (loop (+ offset element-size))))))
            (else
             (raise-sbank-callout-error
              "cannot iterate over array of unknown size" ptr atype)))))

  (define (free-c-array ptr atype size free-spec)
    (when (free-spec-set? (free-spec-shift free-spec))
      (let ((cleanup (type-cleanup (array-element-type atype))))
        (when cleanup
          (c-array-for-each cleanup ptr atype size))))
    (when (free-spec-set? free-spec)
      (free ptr)))

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
          ((genumerated? type) #f)
          ((array-type? type)
           (lambda (ptr)
             (free-c-array ptr type #f -1)))
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
                      (values prim-type
                              elt-size
                              pointer-utf8z-ptr-ref
                              pointer-utf8z-ptr-set!))
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
                                  (memset gvalue 0 g-value-size)
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
                           (make-ginstance et (pointer-ptr-ref ptr i)))
                         (lambda (ptr i v)
                           (pointer-ptr-set!
                            ptr i
                            (cond ((and (ginstance? v) (ginstance-is-a? v et))
                                   (ginstance-ptr v))
                                  (else
                                   (raise-sbank-callout-error
                                    "invalid element for array of this type value"
                                    v et)))))))
                (else
                 (raise-sbank-callout-error
                  "non-simple array element types not yet supported")))))))

  (define (fnamez-ptr->string ptr)
    (let ((error (malloc/set! 'pointer (null-pointer)))
          (bytes-written (malloc/set! 'size_t 0)))
      (let ((result (g-filename-to-utf8 ptr -1 (null-pointer)
                                        bytes-written error)))
        (when (null-pointer? result)
          (free bytes-written)
          (raise-gerror/free error))
        (utf8z-ptr->string result))))

  (define (string->fnamez-ptr s)
    (error 'string->fnamez-ptr "not yet implemented" s))

  ;; Retrieve a Scheme representation of the memory pointed to by @1,
  ;; according to the type @2.
  (define (deref-pointer ptr ti)
    (let ((type (type-info-type ti)))
      (cond ((symbol? type)
             (case type
               ((gvalue) ptr)
               (else
                ((make-pointer-c-getter (type-tag-symbol->prim-type type)) ptr 0))))
            ((gobject-record-class? type)
             ptr)
            ((or (gobject-class? type) (array-type? type))
             (pointer-ptr-ref ptr 0))
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

  ;; Allocate memory as needed for the type @2, store a representation
  ;; of @1 in it, and return a pointer to the allocated memory
  (define (malloc/set! type val)
    (cond ((symbol? type)
           (let ((type (type-tag-symbol->prim-type type)))
             (let ((mem (malloc (c-type-sizeof type))))
               ((make-pointer-c-setter type) mem 0 val)
               mem)))
          ((array-type? type)
           (let ((mem (malloc (c-type-sizeof 'pointer))))
             (pointer-ptr-set! mem 0 val)
             mem))
          (else
           (error 'malloc/set! "not implemented" type val))))

  (define-condition-type &gerror &error
    make-gerror gerror?
    (domain gerror-domain)
    (code gerror-code))

  (define (gerror-conditions/free etype gerror)
    (let ((domain (c-gerror-domain gerror))
          (code (c-gerror-code gerror))
          (message (utf8z-ptr->string (c-gerror-message gerror))))
      (gerror-free gerror)
      (list
       (make-message-condition message)
       (make-gerror domain code))))

  (define (raise-gerror/free who etype gerror . irritants)
    (let ((conditions (gerror-conditions/free etype gerror)))
      (raise (apply condition
                    (make-who-condition who)
                    (make-irritants-condition irritants)
                    conditions))))

  (define-c-callouts libglib
    (g-filename-to-utf8 'pointer "g_filename_to_utf8"
                        '(ssize_t pointer pointer pointer))
    (gerror-free 'void "g_error_free" '(pointer)))

  (define-accessors "GError"
    (c-gerror-domain "domain")
    (c-gerror-code "code")
    (c-gerror-message "message"))

)
