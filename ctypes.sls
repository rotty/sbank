;;; ctypes.sls --- Dealing with C types and callouts/callbacks

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


(library (sbank ctypes)
  (export make-callout
          arg/prim-type+steps
          
          utf8z-ptr->string
          ->utf8z-ptr/null

          array-type?
          make-array-type
          array-length-index

          type-tag-symbol->prim-type
          type-tag->symbol
          symbol->type-tag)
  
  (import (rnrs)
          (spells define-values)
          (spells receive)
          (spells foreign)
          (only (spells lists) iota)
          (only (spells assert) cerr cout)
          (sbank utils)
          (sbank gobject)
          (sbank conditions))

  (define (raise-sbank-callout-error msg . irritants)
    (raise (condition (make-sbank-callout-error)
                      (make-message-condition msg)
                      (make-irritants-condition irritants))))
  
  (define (args-pre-call! arg-vec vtypes flags)
    (do ((i 0 (+ i 1))
         (vtypes vtypes (cdr vtypes))
         (flags flags (cdr flags)))
        ((>= i (vector-length arg-vec)))
      (case (car flags)
        ((in-out) (vector-set! arg-vec i (malloc/set! (car vtypes) (vector-ref arg-vec i))))
        ((out) (vector-set! arg-vec i
                            (malloc (c-type-sizeof (type-tag-symbol->prim-type (car vtypes)))))))))

  (define (args-post-call! arg-vec vtypes flags)
    (do ((i 0 (+ i 1))
         (vtypes vtypes (cdr vtypes))
         (flags flags (cdr flags)))
        ((>= i (vector-length arg-vec)))
      (case (car flags)
        ((in-out out) (vector-set! arg-vec i (deref-pointer (vector-ref arg-vec i) (car vtypes)))))))
  
  (define (arg/prim-type+steps type i null-ok? out?)
    (cond
     ((symbol? type)
      (let ((prim-type (type-tag-symbol->prim-type type)))
        (case type
          ((int8 uint8 int16 uint16 int32 uint32
                 int64 uint64 int uint long ulong ssize size
                 float double)
           (values prim-type i #f #f))
          ((utf8)
           (values prim-type
                   (converter-setup/null-ok i string->utf8z-ptr null-ok? #f)
                   (and out? (converter-collect i utf8z-ptr->string))
                   (cleanup-step free i)))
          (else
           (raise-sbank-callout-error "argument passing for this type not implemented" type)))))
     ((genum? type)
      ;; FIXME: Is signed-int always OK?
      (values 'signed-int
              (converter-setup i (lambda (val)
                                   (if (symbol? val)
                                       (or (genum-lookup type val)
                                           (raise-sbank-callout-error
                                            "invalid enumeration value" val (genum-symbols type)))
                                       val)))
              (and out? (converter-collect i (lambda (val)
                                               (or (genum-lookup type val) val))))
              #f))
     ((array-type? type)
      (values 'pointer
              (array-arg-setup type i)
              (array-arg-collect type i)
              (array-arg-cleanup type i)))
     (else
      (raise-sbank-callout-error "complex argument types not yet implemented" type))))

  (define (cleanup-step cleanup-proc i)
    (lambda (arg-vec)
      (cleanup-proc (vector-ref arg-vec i))))
  
  (define (converter-setup/null-ok i convert null-ok? null-val)
    (lambda (args arg-vec)
      (vector-set! arg-vec i (if (and (or *null-ok-always-on*
                                          null-ok?)
                                      (equal? (car args) null-val))
                                 (integer->pointer 0)
                                 (convert (car args))))
      (cdr args)))

  (define (converter-setup i convert)
    (lambda (args arg-vec)
      (vector-set! arg-vec i (convert (car args)))
      (cdr args)))
  
  (define (converter-collect i converter)
    (lambda (arg-vec)
      (converter (vector-ref arg-vec i))))
  
  (define (array-arg-setup type i)
    (lambda (args arg-vec)
      (let* ((vec (cond ((vector? (car args)) (car args))
                        ((list? (car args)) (list->vector (car args)))
                        (else (raise-sbank-callout-error
                               "cannot convert argument to array" (car args)))))
             (array (vector->c-array vec type)))
        (vector-set! arg-vec i array)
        (cond ((array-length-index type)
               => (lambda (l-index)
                    (vector-set! arg-vec l-index (vector-length vec))))))
      (cdr args)))

  (define (array-arg-collect atype i)
    (lambda (arg-vec)
      (c-array->vector (vector-ref arg-vec i) atype)))

  (define (array-arg-cleanup type i)
    (lambda (arg-vec)
      (free-c-array (vector-ref arg-vec i) type)))
  
  (define (args-setup-procedure n-args steps)
    (define (lose msg . irritants)
      (apply raise-sbank-callout-error msg irritants))
    (if (equal? steps (iota n-args))
        #f
        (lambda (in-args)
          (let ((arg-vec (make-vector n-args)))
            (let loop ((args in-args) (steps steps))
              (cond ((null? steps)
                     (unless (null? args)
                       (lose "unprocessed arguments" args))
                     arg-vec)
                    ((null? args)
                     (lose "too few arguments" in-args))
                    ((integer? (car steps))
                     (vector-set! arg-vec (car steps) (car args))
                     (loop (cdr args) (cdr steps)))
                    ((eqv? (car steps) #f)
                     (loop args (cdr steps)))
                    (else
                     (loop ((car steps) args arg-vec) (cdr steps)))))))))

  (define (args-collect-procedure steps)
    (if (null? steps)
        #f
        (lambda (arg-vec)
          (let loop ((out-vals '()) (steps steps))
            (if (null? steps)
                out-vals
                (loop (cons ((car steps) arg-vec) out-vals) (cdr steps)))))))

  (define (args-cleanup-procedure steps)
    (if (null? steps)
        #f
        (lambda (arg-vec)
          (for-each (lambda (step) (step arg-vec)) steps))))
  
  (define (make-callout prim-ret prim-args setup-steps collect-steps cleanup-steps vtypes flags)
    (let ((prim-callout (make-c-callout prim-ret prim-args))
          (out-args? (exists (lambda (flag) (memq flag '(out in-out))) flags))
          (setup (args-setup-procedure (length prim-args) setup-steps))
          (collect (args-collect-procedure collect-steps))
          (cleanup (args-cleanup-procedure cleanup-steps)))
      (cond ((and setup collect out-args?)
             (lambda (ptr)
               (let ((do-callout (prim-callout ptr)))
                 (lambda args
                   (let ((arg-vec (setup args)))
                     (args-pre-call! arg-vec vtypes flags)
                     (let ((ret-val (apply do-callout (vector->list arg-vec))))
                       (args-post-call! arg-vec vtypes flags)
                       (let ((out-vals (collect arg-vec)))
                         (if cleanup (cleanup arg-vec))
                         (if (eq? prim-ret 'void)
                             (apply values out-vals)
                             (apply values ret-val out-vals)))))))))
            (setup
             (assert (not (or collect out-args?)))
             (lambda (ptr)
               (let ((do-callout (prim-callout ptr)))
                 (lambda args
                   (let* ((arg-vec (setup args))
                          (ret-val (apply do-callout (vector->list arg-vec))))
                     (if cleanup (cleanup arg-vec))
                     (if (eq? prim-ret 'void)
                         (values)
                         ret-val))))))
            (else
             (assert (not (or setup collect out-args?)))
             prim-callout))))

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
             (pointer-set-c-pointer! mem 0 val)
             mem))
          (else
           (error 'malloc/set! "not implemented" type val))))

  ;; Retrieve a Scheme representation of the memory pointed to by @1,
  ;; according to the type @2.
  (define (deref-pointer ptr type)
    (cond ((symbol? type)
           ((make-pointer-c-getter (type-tag-symbol->prim-type type)) ptr 0))
          ((array-type? type)
           (pointer-ref-c-pointer ptr 0))
          (else
           (error 'deref-pointer "not implemented for that type" ptr type))))
  
  (define-record-type array-type
    (fields (immutable element-type array-element-type)
            (immutable elements-pointers? array-elements-pointers?)
            (immutable is-zero-terminated array-is-zero-terminated?)
            (immutable size array-size)
            (immutable length-index array-length-index)))
  
  (define-syntax define-enum
    (syntax-rules ()
      ((define-enum val->symbol symbol->val (symbol ...))
       (define-values (val->symbol symbol->val)
         (let ((sym-vec '#(symbol ...)))
           (values (lambda (val) (vector-ref sym-vec val))
                   (lambda (sym) (vector-index eq? sym-vec sym))))))))

  ;; Note that these must match with gobject-introspection
  (define-enum type-tag->symbol symbol->type-tag
    (void boolean int8 uint8 int16 uint16 int32 uint32
          int64 uint64 int uint long ulong ssize size
          float double time_t gtype utf8 filename
          array interface glist gslist ghash error))

  (define (type-tag-symbol->prim-type sym)
    (case sym
      ((utf8) 'pointer)
      (else sym)))

  (define (utf8z-ptr->string ptr)
    (let ((size (do ((i 0 (+ i 1)))
                    ((= (pointer-ref-c-unsigned-char ptr i) 0) i))))
      (utf8->string (memcpy (make-bytevector size) ptr size))))
  
  (define (->utf8z-ptr/null who s)
    (cond ((string? s) (string->utf8z-ptr s))
          ((eqv? s #f)
           (integer->pointer 0))
          (else
           (assertion-violation who "invalid argument" s))))

  (define (string->utf8z-ptr s)
    (let* ((bytes (string->utf8 s))
           (bytes-len (bytevector-length bytes))
           (result (malloc (+ bytes-len 1))))
      (memcpy result bytes bytes-len)
      (pointer-set-c-char! result bytes-len 0)
      result))

  (define (array-type-values atype)
    (cond ((symbol? (array-element-type atype))
           (let* ((prim-type (type-tag-symbol->prim-type (array-element-type atype)))
                  (elt-size (c-type-sizeof prim-type)))
             (case (array-element-type atype)
               ((utf8)
                (values prim-type elt-size pointer-utf8z-ptr-ref pointer-utf8z-ptr-set!))
               (else
                (values prim-type
                        elt-size
                        (make-pointer-c-getter prim-type)
                        (make-pointer-c-setter prim-type))))))
          (else
           (raise-sbank-callout-error "non-simple array element types not yet supported"))))
  
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

  (define (c-array->vector ptr atype)
    (receive (prim-type element-size element-ref element-set!) (array-type-values atype)
      (cond ((array-is-zero-terminated? atype)
             (let ((terminator?
                    (let ((terminator (array-terminator prim-type)))
                      (if (pointer? terminator)
                          (lambda (offset)
                            (= (pointer->integer (pointer-ref-c-pointer ptr offset))
                               (pointer->integer terminator)))
                          (lambda (offset)
                            (= (element-ref ptr offset) terminator))))))
               (let loop ((offset 0) (elts '()))
                 (if (terminator? offset)
                     (list->vector (reverse elts))
                     (loop (+ offset element-size) (cons (element-ref ptr offset) elts))))))
            ((array-size atype)
             => (lambda (size)
                  (do ((vec (make-vector size))
                       (i 0 (+ i 1)))
                      ((>= i size) vec)
                    (vector-set! vec i (element-ref ptr (* i element-size)))))))))


  (define (array-terminator prim-type)
    (case prim-type
      ((char uchar short ushort int uint long ulong) 0)
      ((pointer) (integer->pointer 0))
      (else
       (raise-sbank-callout-error
        "zero-termination of arrays of this type not implemented" prim-type))))
  
  (define (free-c-array ptr atype)
    (cerr "free-c-array not implemented, you are leaking memory :-P\n"))
  

  (define (pointer-utf8z-ptr-set! ptr i val)
    (pointer-set-c-pointer! ptr i (if (pointer? val)
                                      val
                                      (string->utf8z-ptr val))))

  (define (pointer-utf8z-ptr-ref ptr i)
    (let ((utf8z-ptr (pointer-ref-c-pointer ptr i)))
      (if (= (pointer->integer utf8z-ptr) 0)
          #f
          (utf8z-ptr->string utf8z-ptr))))

  (define *null-ok-always-on* #f))
