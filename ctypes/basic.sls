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
#!r6rs

(library (sbank ctypes basic)
  (export vector->c-array ->c-array
          c-array-for-each
          free-c-array
          c-array->vector

          bytevector-portion bytevector-portion? bytevector-portion-count
          malloc/set!

          type-info->prim-type
          type-info/prim-type+procs
          out-converter/null
          back-converter/null

          value->gtype
          type->gtype
          type-info-gtype

          deref-pointer
          set-pointer

          type-tag-symbol->prim-type
          type-tag->symbol
          symbol->type-tag

          raise-sbank-callout-error raise-sbank-callback-error

          raise-gerror/free
          gerror-conditions/free

          null-ok-always-on?)
  (import (rnrs)
          (xitomatl srfi and-let*)
          (spells receive)
          (spells parameter)
          (spells foreign)
          (spells tracing)
          (sbank support utils)
          (sbank type-data)
          (sbank support conditions)
          (for (sbank support stypes) expand)
          (sbank support shlibs)
          (for (sbank typelib stypes) expand)
          (sbank gobject gtype)
          (sbank gobject genum)
          (sbank gobject gvalue)
          (sbank gobject glist)
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

  (define-record-type (<bytevector-portion> bytevector-portion bytevector-portion?)
    (fields (immutable bv bytevector-portion-bv)
            (immutable start bytevector-portion-start)
            (immutable count bytevector-portion-count)))

  (define (type-info/prim-type+procs ti)
    (let ((type (type-info-type ti))
          (null-ok? (type-info-null-ok? ti)))
      (cond
       ((symbol? type)
        (let ((prim-type (type-tag-symbol->prim-type type)))
          (case type
            ((int8 uint8 int16 uint16 int32 uint32
                   int64 uint64 int uint long ulong ssize size time_t
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
            ((filename)
             (values prim-type
                     (out-converter/null string->fnamez-ptr null-ok? #f)
                     (back-converter/null fnamez-ptr->string null-ok? #f)
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
            ((gslist)
             (let ((elt-ti (car (type-info-parameters ti))))
               (receive (elt-pt elt-out elt-back elt-cleanup)
                        (type-info/prim-type+procs elt-ti)
                 (values 'pointer
                         (gslist-out-converter elt-out)
                         (gslist-back-converter
                          (make-gslist-class elt-out elt-back elt-cleanup))
                         (gslist-cleanup elt-cleanup)))))
            ((glist)
             (let ((elt-ti (car (type-info-parameters ti))))
               (receive (elt-pt elt-out elt-back elt-cleanup)
                        (type-info/prim-type+procs elt-ti)
                 (values 'pointer
                         (glist-out-converter elt-out)
                         (glist-back-converter
                          (make-glist-class elt-out elt-back elt-cleanup))
                         (glist-cleanup elt-cleanup)))))
            ((ghash)
             (let ((key-ti (car (type-info-parameters ti)))
                   (val-ti (cadr (type-info-parameters ti))))
               (let-values (((key-pt key-out key-back key-cleanup)
                             (type-info/prim-type+procs key-ti))
                            ((val-pt val-out val-back val-cleanup)
                             (type-info/prim-type+procs val-ti)))
                 (let ((class (make-ghash-class key-out val-out
                                                key-back val-back
                                                key-cleanup val-cleanup)))
                   (values
                    'pointer
                    (out-converter/null (ghash-out-converter class) null-ok? #f)
                    (back-converter/null (ghash-back-converter class) null-ok? #f)
                    (ghash-cleanup class))))))
            ((gtype)
             (values prim-type symbol->gtype gtype->symbol #f))
            (else
             (raise-sbank-callout-error
              "argument passing for this type not implemented" ti)))))
       ((genum? type)
        (values 'int                    ; Is int always OK?
                (lambda (val)
                  (if (symbol? val)
                      (or (genumerated-lookup type val)
                          (raise-sbank-callout-error
                           "invalid enumeration value"
                           val (genumerated-symbols type)))
                      val))
                (lambda (val)
                  (or (genumerated-lookup type val) val))
                #f))
       ((gflags? type)
        (values 'int
                (lambda (val)
                  (if (integer? val)
                      val
                      (gflags->integer type val)))
                (lambda (val)
                  (integer->gflags type val))
                #f))
       ((array-type? type)
        (unless (or (array-size type) (array-is-zero-terminated? type))
          (raise-sbank-callout-error
           "cannot handle array without size information" type))
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
       ((signature? type)
        (values 'pointer
                (out-converter/null (signature-callback type) null-ok? #f)
                (back-converter/null (signature-callout type) null-ok? #f)
                #f))
       (else
        (raise-sbank-callout-error
         "argument/return type not yet implemented" type)))))

  (define (ginstance-maker gtype-lookup declared-class)
    (cond ((or (gobject-record-class? declared-class)
               (gobject-union-class? declared-class))
           (lambda (instance)
             (make-ginstance declared-class instance)))
          (else
           (lambda (instance)
             (let ((class
                     (or
                      (and-let* ((gtype (gtype-class-gtype
                                         (gtype-instance-class instance))))
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
              (null-pointer)
              (convert val)))
        convert))

  (define (back-converter/null convert null-ok? null-val)
    (if (or (null-ok-always-on?) null-ok?)
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
                     (loop (+ offset element-size)
                           (cons (element-ref ptr offset) elts))))))
            (else
             (error 'c-array->vector
                    "cannot handle array without size information" atype)))))

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
               (lose "cannot convert bytevector to array of this type val atype"))
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
      ((pointer) (null-pointer))
      (else
       (raise-sbank-callout-error
        "zero-termination of arrays of this type not implemented" prim-type))))

  (define (array-terminator-predicate elt-prim-type)
    (let ((terminator (array-terminator elt-prim-type))
          (elt-ref (make-pointer-c-getter elt-prim-type)))
      (if (pointer? terminator)
          (lambda (ptr offset)
            (pointer=? (pointer-ptr-ref offset)
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
             (let ((terminator? (array-terminator-predicate atype)))
               (let loop ((offset 0) (elts '()))
                 (unless (terminator? ptr offset)
                   (proc (deref-pointer (pointer+ ptr offset) element-ti))
                   (loop (+ offset element-size))))))
            (else
             (raise-sbank-callout-error
              "cannot iterate over array of unknown size" ptr atype)))))

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
          ((genumerated? type) #f)
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
                            ptr i (if (ginstance? v)
                                      (ginstance-ptr v)
                                      (ginstance-ptr (send et (new v))))))))
                (else
                 (raise-sbank-callout-error
                  "non-simple array element types not yet supported")))))))

  (define (gslist-out-converter elt-convert)
    (lambda (lst)
      (cond ((eqv? lst #f)
             (null-pointer))
            ((pointer? lst)
             lst)
            ((gslist? lst)
             (ginstance-ptr lst))
            (else
             (let loop ((gslist (null-pointer))
                        (lst lst))
               (if (null? lst)
                   (g-slist-reverse gslist)
                   (loop (g-slist-prepend gslist
                                          (if elt-convert
                                              (elt-convert (car lst))
                                              (car lst)))
                         (cdr lst))))))))

  (define (gslist->list gslist elt-convert)
    (let loop ((lst '()) (gslist gslist))
      (if (null-pointer? gslist)
          (reverse lst)
          (loop (cons (elt-convert (g-slist-data gslist)) lst)
                (g-slist-next gslist)))))

  (define (gslist-back-converter class)
    (lambda (gslist)
      (make-ginstance class gslist)))

  (define (gslist-cleanup elt-cleanup)
    (lambda (gslist)
      (and elt-cleanup
           (let loop ((gslist gslist))
             (unless (null-pointer? gslist)
               (elt-cleanup (g-slist-data gslist))
               (loop (g-slist-next gslist)))))
      (g-slist-free gslist)))

  (define (glist-out-converter elt-convert)
    (lambda (lst)
      (cond ((eqv? lst #f)
             (null-pointer))
            ((pointer? lst)
             lst)
            ((glist? lst)
             (ginstance-ptr lst))
            (else
             (let loop ((glist (null-pointer))
                        (lst lst))
               (if (null? lst)
                   glist
                   (loop (g-list-append glist
                                        (if elt-convert
                                            (elt-convert (car lst))
                                            (car lst)))
                         (cdr lst))))))))

  (define (glist->list glist elt-convert)
    (let loop ((lst '()) (glist (g-list-last glist)))
      (if (null-pointer? glist)
          lst
          (loop (cons (elt-convert (g-list-data glist)) lst)
                (g-list-prev glist)))))

  (define (glist-back-converter class)
    (lambda (glist)
      (make-ginstance class glist)))

  (define (glist-cleanup elt-cleanup)
    (lambda (glist)
      (and elt-cleanup
           (let loop ((glist glist))
             (unless (null-pointer? glist)
               (elt-cleanup (g-list-data glist))
               (loop (g-list-next glist)))))
      (g-list-free glist)))

  (define (ghash-out-converter class)
    (lambda (val)
      (unless (ghash? val)
        (raise-sbank-callout-error "expected an GHash instance" val))
      (ginstance-ptr val)))

  (define (ghash-back-converter class)
    (lambda (ghash-ptr)
      (make-ginstance class ghash-ptr)))

  (define (ghash-cleanup class)
    (lambda (val)
      #f))

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
            ((or (array-type? type) (gobject-class? type))
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

  (define (type-info->prim-type ti out?)
    (let ((type (type-info-type ti)))
      (cond ((or out?
                 (array-type? type)
                 (gobject-class? type))
             'pointer)
            ((signature? type)
             'fpointer)
            ((genumerated? type)
             'int)
            ((symbol? type)
             (case type
               ((gtype) gtype-ctype)
               ((size) 'size_t)
               ((ssize) 'ssize_t)
               ((boolean) 'uint)
               ((utf8 filename gvalue gslist glist ghash) 'pointer)
               (else type)))
            (else
             (raise-sbank-callout-error
              "argument/return type not yet implemented" type)))))


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

  (define-callouts libglib
    (g-filename-to-utf8 'pointer "g_filename_to_utf8"
                        '(ssize_t pointer pointer pointer))
    (gerror-free 'void "g_error_free" '(pointer)))

  (define-accessors "GError"
    (c-gerror-domain "domain")
    (c-gerror-code "code")
    (c-gerror-message "message"))

)
