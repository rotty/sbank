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
          make-callback
          arg-callout-steps
          arg-callback-steps
          
          utf8z-ptr->string
          string->utf8z-ptr
          ->utf8z-ptr/null
          malloc/set!

          type-tag-symbol->prim-type
          type-tag->symbol
          symbol->type-tag

          gerror? make-gerror gerror-domain gerror-code
          raise-gerror/free

          null-ok-always-on?)
  
  (import (rnrs)
          (spells define-values)
          (spells receive)
          (spells parameter)
          (spells foreign)
          (only (spells lists) iota)
          (only (spells assert) cerr cout)
          (only (spells misc) unspecific)
          (spells tracing)
          (sbank utils)
          (sbank type-data)
          (sbank stypes)
          (sbank shlibs)
          (sbank typelib stypes)
          (sbank gobject internals)
          (sbank conditions))

  (define-syntax debug
    (syntax-rules ()
      ((debug <expr> ...)
       (for-each display (list "DEBUG: " <expr> ... "\n")))))
  
  ;;(define-syntax debug (syntax-rules () (begin)))

  (define-syntax define-accessors (stype-accessor-definer (typelib-stypes)))

  (define-accessors "GError"
    (c-gerror-domain "domain")
    (c-gerror-code "code")
    (c-gerror-message "message"))

  (define-condition-type &gerror &error
    make-gerror gerror?
    (domain gerror-domain)
    (code gerror-code))

  (define (array-element-size atype)
    (let ((eti (array-element-type-info atype)))
      (c-type-sizeof (type-info->prim-type eti #f))))

  (define null-ok-always-on? (make-parameter #f))
  
  (define (raise-sbank-callout-error msg . irritants)
    (raise (condition (make-sbank-callout-error)
                      (make-message-condition msg)
                      (make-irritants-condition irritants))))

  (define (raise-sbank-callback-error msg . irritants)
    (raise (condition (make-sbank-callback-error)
                      (make-message-condition msg)
                      (make-irritants-condition irritants))))
  
  (define (args-pre-call! arg-vec arg-types flags)
    (do ((i 0 (+ i 1))
         (arg-types arg-types (cdr arg-types))
         (flags flags (cdr flags)))
        ((>= i (vector-length arg-vec)))
      (let* ((ti (car arg-types))
             (prim-type (type-info-type ti)))
        (case (car flags)
          ((in-out) (vector-set! arg-vec i (malloc/set! prim-type (vector-ref arg-vec i))))
          ((out) (vector-set! arg-vec i
                              (malloc (c-type-sizeof (type-tag-symbol->prim-type prim-type)))))))))

  (define (args-post-call! arg-vec arg-types flags)
    (do ((i 0 (+ i 1))
         (arg-types arg-types (cdr arg-types))
         (flags flags (cdr flags)))
        ((>= i (vector-length arg-vec)))
      (case (car flags)
        ((in-out out)
         (vector-set! arg-vec i (deref-pointer (vector-ref arg-vec i) (car arg-types)))))))
  
  (define (arg-callout-steps ti i out?)
    (let ((type (type-info-type ti)))
      (cond
       ((array-type? type)
        (values (array-arg-setup type i (type-info-null-ok? ti))
                (and out? (array-arg-collect type i))
                (array-arg-cleanup type i)))
       ((gerror-type? type)
        (values (gerror-arg-setup type i)
                #f
                (gerror-arg-cleanup type i)))
       (else
        (receive (prim-type out-convert back-convert cleanup)
                 (type-info/prim-type+procs ti)
          (values (if out-convert (converter-setup i out-convert) i)
                  (and out? (if back-convert (converter-collect i back-convert) i))
                  (and cleanup (cleanup-step i cleanup))))))))

  (define (arg-callback-steps ti i)
    (let ((type (type-info-type ti)))
      (receive (prim-type out-convert back-convert cleanup)
               (type-info/prim-type+procs ti)
        (values (if back-convert
                    (lambda (arg-vec)
                      (back-convert (vector-ref arg-vec i)))
                    i)
                (lambda (arg-vec val)
                  (set-pointer (vector-ref arg-vec i) ti (if out-convert (out-convert val) val)))))))
  
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

  (define (type-info/prim-type+procs ti)
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
            (else
             (raise-sbank-callout-error "argument passing for this type not implemented" type)))))
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
                (back-converter/null (lambda (ptr)
                                       (make-ginstance type ptr))
                                     null-ok?
                                     #f)
                #f))
       (else
        (raise-sbank-callout-error "argument/return type not yet implemented" type)))))
  
  (define (cleanup-step i cleanup-proc)
    (lambda (arg-vec)
      (cleanup-proc (vector-ref arg-vec i))))

  ;; This returns and expects a converter that returns a pointer
  (define (converter-setup/null i convert null-ok? null-val)
    (let ((convert/null (out-converter/null convert null-ok? null-val)))
      (lambda (args arg-vec)
        (vector-set! arg-vec i (convert (car args)))
        (cdr args))))

  (define (converter-setup i convert)
    (lambda (args arg-vec)
      (vector-set! arg-vec i (convert (car args)))
      (cdr args)))
  
  (define (converter-collect i convert)
    (lambda (arg-vec)
      (convert (vector-ref arg-vec i))))

  (define (converter-collect/null i convert null-ok? null-val)
    (let ((convert/null (back-converter/null convert null-ok? null-val)))
      (lambda (arg-vec)
        (convert/null (vector-ref arg-vec i)))))
  
  (define (array-arg-setup atype i null-ok?)
    (let ((convert (out-converter/null (lambda (val)
                                         (->c-array val atype)) null-ok? #f)))
      (lambda (args arg-vec)
        (let ((vec (->vector (car args))))
          (vector-set! arg-vec i (convert vec))
          (cond ((array-length-index atype)
                 => (lambda (l-index)
                      (vector-set! arg-vec l-index (vector-length vec)))))
          (cdr args)))))

  (define (get-array-length atype arg-vec)
    (cond ((array-length-index atype)
           => (lambda (l-index)
                (vector-ref arg-vec l-index)))))
  
  (define (array-arg-collect atype i)
    (lambda (arg-vec)
      (c-array->vector (vector-ref arg-vec i) atype (get-array-length atype arg-vec))))
  
  (define (array-arg-cleanup atype i)
    (lambda (arg-vec)
      (free-c-array (vector-ref arg-vec i) atype (get-array-length atype arg-vec))))

  (define (gerror-arg-setup etype i)
    (lambda (args arg-vec)
      (vector-set! arg-vec i (malloc/set! 'pointer (integer->pointer 0)))
      args))

  (define (gerror-arg-cleanup etype i)
    (lambda (arg-vec)
      (let* ((gerror-ptr (vector-ref arg-vec i))
             (gerror (pointer-ref-c-pointer gerror-ptr 0)))
        (free gerror-ptr)
        (unless (= (pointer->integer gerror) 0)
          (raise (apply condition
                        (make-sbank-callout-error)
                        (gerror-conditions/free etype gerror)))))))

  (define (raise-gerror/free who etype gerror . irritants)
    (let ((conditions (gerror-conditions/free etype gerror)))
      (raise (apply condition
                    (make-who-condition who)
                    (make-irritants-condition irritants)
                    conditions))))

  (define (gerror-conditions/free etype gerror)
    (let ((domain (c-gerror-domain gerror))  
          (code (c-gerror-code gerror))
          (message (utf8z-ptr->string (c-gerror-message gerror))))
      (gerror-free gerror)
      (list
       (make-message-condition message)
       (make-gerror domain code))))
  
  (define gerror-free
    ((make-c-callout 'void '(pointer)) (dlsym libglib "g_error_free")))

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


  (define (make-callout rti arg-types setup-steps collect-steps cleanup-steps flags)
    (let ((prim-callout
           (make-c-callout (type-info->prim-type rti #f)
                           (map (lambda (type flag)
                                  (type-info->prim-type type (memq flag '(out in-out))))
                                arg-types
                                flags)))
          (out-args? (exists (lambda (flag) (memq flag '(out in-out))) flags))
          (setup (args-setup-procedure (length arg-types) setup-steps))
          (collect (args-collect-procedure collect-steps))
          (cleanup (args-cleanup-procedure cleanup-steps))
          (ret-consume (ret-type-consumer rti)))
      (cond ((and setup collect out-args?)
             (lambda (ptr)
               (let ((do-callout (prim-callout ptr)))
                 (lambda args
                   (let ((arg-vec (setup args)))
                     (args-pre-call! arg-vec arg-types flags)
                     (let ((ret-val (apply do-callout (vector->list arg-vec))))
                       (args-post-call! arg-vec arg-types flags)
                       (let ((out-vals (collect arg-vec)))
                         (if cleanup (cleanup arg-vec))
                         (if (and (eqv? ret-consume #f))
                             (apply values out-vals)
                             (apply values
                                    (if (procedure? ret-consume) (ret-consume ret-val) ret-val)
                                    out-vals)))))))))
            (setup
             (assert (not (or collect out-args?)))
             (lambda (ptr)
               (let ((do-callout (prim-callout ptr)))
                 (lambda args
                   (let* ((arg-vec (setup args))
                          (ret-val (apply do-callout (vector->list arg-vec))))
                     (if cleanup (cleanup arg-vec))
                     (if (eqv? ret-consume #f)
                         (unspecific)
                         (if (procedure? ret-consume) (ret-consume ret-val) ret-val)))))))
            ((procedure? ret-consume)
             (assert (not (or setup collect out-args? cleanup)))
             (lambda (ptr)
               (lambda args
                 (let ((do-callout (prim-callout ptr)))
                   (ret-consume (apply do-callout args))))))
            (else
             (assert (and (not (or setup collect out-args?)) (boolean? ret-consume)))
             prim-callout))))

  (define (make-callback rti arg-types prepare-steps store-steps flags)
    (receive (prim-ret ret-out-convert ret-back-convert cleanup)
             (type-info/prim-type+procs rti)
      (let ((prim-callback
             (make-c-callback prim-ret
                              (map (lambda (type flag)
                                     (type-info->prim-type type (memq flag '(out in-out))))
                                   arg-types
                                   flags))))
        (cond ((and (not ret-out-convert) (equal? prepare-steps (iota (length arg-types))))
               prim-callback)
              (else
               (let ((arg-len (length arg-types)))
                 (lambda (proc)
                   (prim-callback
                    (make-callback-wrapper proc prim-ret ret-out-convert arg-types
                                           prepare-steps store-steps flags)))))))))


  (define (make-callback-wrapper proc prim-ret ret-out-convert arg-types
                                  prepare-steps store-steps flags)
    (let ((arg-len (length arg-types))
          (out-args? (exists (lambda (flag) (memq flag '(out in-out))) flags)))
      (lambda args
        (assert (= arg-len (length args)))
        (let* ((arg-vec (list->vector args))
               (args (let loop ((args '()) (steps prepare-steps))
                       (if (null? steps)
                           (reverse args)
                           (loop (cons
                                  (cond ((integer? (car steps))
                                         (vector-ref arg-vec ))
                                        (else
                                         ((car steps) arg-vec)))
                                  args)
                                 (cdr steps))))))
          (receive ret-values (apply proc args)
            (let loop ((vals (if (and (eq? prim-ret 'void) out-args?)
                                 ret-values
                                 (cdr ret-values)))
                       (steps store-steps))
              (cond ((null? vals)
                     (unless (null? steps)
                       (raise-sbank-callback-error "called procedure returned not enough values"
                                                   ret-values prim-ret))
                     (cond ((eq? prim-ret 'void)
                            (unspecific))
                           (ret-out-convert
                            (ret-out-convert (car ret-values)))
                           (else
                            (car ret-values))))
                    ((null? steps)
                     (raise-sbank-callback-error "called procedure returned too many values"
                                                 ret-values prim-ret))
                    (else
                     (loop ((car steps) (car vals) arg-vec))))))))))
  
  (define (ret-type-consumer rti)
    (cond ((eq? (type-info-type rti) 'void)
           #f)
          (else
           (receive (prim-type out-convert back-convert cleanup)
                    (type-info/prim-type+procs rti)
             (cond (back-convert
                    (lambda (val)
                      (let ((result (back-convert val)))
                        (if cleanup (cleanup val))
                        result)))
                   (else
                    (assert (not cleanup))
                    #t))))))
  
  (define (type-info->prim-type ti out?)
    (let ((type (type-info-type ti)))
      (cond ((or out?
                 (type-info-is-pointer? ti)
                 (array-type? type)
                 (gobject-class? type))
             'pointer)
            ((genum? type)
             'int)
            (else
             (type-tag-symbol->prim-type type)))))
    
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
  (define (deref-pointer ptr ti)
    (let ((type (type-info-type ti)))
      (cond ((symbol? type)
             ((make-pointer-c-getter (type-tag-symbol->prim-type type)) ptr 0))
            ((array-type? type)
             (pointer-ref-c-pointer ptr 0))
            (else
             (error 'deref-pointer "not implemented for that type" ptr type)))))

  (define (set-pointer ptr ti val)
    (let ((type (type-info-type ti)))
      (cond ((symbol? type)
             ((make-pointer-c-setter (type-tag-symbol->prim-type type)) ptr 0 val))
            (else
             (error 'set-pointer "not implemented for that type" ptr ti)))))
  
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
      ((boolean) 'uint)
      ((utf8) 'pointer)
      ((int) 'int)
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
    (let* ((eti (array-element-type-info atype))
           (et (type-info-type eti)))
      (cond ((symbol? et)
             (let* ((prim-type (type-tag-symbol->prim-type et))
                    (elt-size (c-type-sizeof prim-type)))
               (case et
                 ((utf8)
                  (values prim-type elt-size pointer-utf8z-ptr-ref pointer-utf8z-ptr-set!))
                 (else
                  (values prim-type
                          elt-size
                          (make-pointer-c-getter prim-type)
                          (make-pointer-c-setter prim-type))))))
            (else
             (raise-sbank-callout-error "non-simple array element types not yet supported")))))
  
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
                     (loop (+ offset element-size) (cons (element-ref ptr offset) elts)))))))))

  (define (->c-array val atype)
    (vector->c-array (->vector val) atype))

  (define (->vector val)
    (cond ((vector? val) val)
          ((list? val) (list->vector val))
          (else (raise-sbank-callout-error
                 "cannot convert argument to array" val))))
  
  (define (array-terminator prim-type)
    (case prim-type
      ((char uchar short ushort int uint long ulong) 0)
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
    (c-array-for-each (type-cleanup (array-element-type atype)) ptr atype size))

  (define (type-cleanup type)
    (define (lose)
      (raise-sbank-callout-error "cleanup for this type not implemented" type))
    (cond ((symbol? type)
           (case type
             ((int8 uint8 int16 uint16 int32 uint32
                    int64 uint64 int uint long ulong ssize size
                    float double)
              #f)
             ((utf8) free)
             (else (lose))))
          ((genum? type) #f)
          ((array-type? type)
           (lambda (ptr)
             (free-c-array ptr type #f)))
          (else
           (lose))))
  
  (define (pointer-utf8z-ptr-set! ptr i val)
    (pointer-set-c-pointer! ptr i (if (pointer? val)
                                      val
                                      (string->utf8z-ptr val))))

  (define (pointer-utf8z-ptr-ref ptr i)
    (let ((utf8z-ptr (pointer-ref-c-pointer ptr i)))
      (if (= (pointer->integer utf8z-ptr) 0)
          #f
          (utf8z-ptr->string utf8z-ptr))))
  
  (define (pointer+ p n)
    (integer->pointer (+ (pointer->integer p) n))))
