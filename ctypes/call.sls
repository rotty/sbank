;;; call.sls --- Dealing with C types and callouts/callbacks

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


(library (sbank ctypes call)
  (export make-callout
          make-callback
          arg-callout-steps
          arg-callback-steps

          malloc/set!

          gerror? make-gerror gerror-domain gerror-code
          raise-gerror/free)

  (import (rnrs)
          (xitomatl srfi and-let*)
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
          (sbank ctypes basic)
          (sbank typelib stypes)
          (sbank gobject internals)
          (sbank gobject gtype)
          (sbank gobject gvalue)
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

  (define (args-pre-call! arg-vec arg-types flags)
    (do ((i 0 (+ i 1))
         (arg-types arg-types (cdr arg-types))
         (flags flags (cdr flags)))
        ((>= i (vector-length arg-vec)))
      (let ((prim-type (type-info->prim-type (car arg-types) #f)))
        (case (car flags)
          ((in-out) (vector-set! arg-vec i (malloc/set! prim-type (vector-ref arg-vec i))))
          ((out) (vector-set! arg-vec i
                              (malloc (c-type-sizeof prim-type))))))))

  (define (args-post-call! arg-vec arg-types flags)
    (do ((i 0 (+ i 1))
         (arg-types arg-types (cdr arg-types))
         (flags flags (cdr flags)))
        ((>= i (vector-length arg-vec)))
      (case (car flags)
        ((in-out out)
         (vector-set! arg-vec i (deref-pointer (vector-ref arg-vec i) (car arg-types)))))))

  (define (arg-callout-steps ti i flag gtype-lookup)
    (let ((type (type-info-type ti)))
      (cond
       ((array-type? type)
        (values (and (not (eq? 'out flag)) (array-arg-setup type i (type-info-null-ok? ti)))
                (and (not (eq? 'in flag)) (array-arg-collect type i))
                (array-arg-cleanup type i)))
       ((gerror-type? type)
        (unless (eq? flag 'in)
          (raise-sbank-callout-error "GError arguments must have direction 'in'" ti flag))
        (values (gerror-arg-setup type i)
                #f
                (gerror-arg-cleanup type i)))
       (else
        (receive (prim-type out-convert back-convert cleanup)
                 (type-info/prim-type+procs ti gtype-lookup)
          (values (and (not (eq? 'out flag)) (if out-convert (converter-setup i out-convert) i))
                  (and (not (eq? 'in flag)) (if back-convert (converter-collect i back-convert) i))
                  (and cleanup (cleanup-step i cleanup))))))))

  (define (arg-callback-steps ti i gtype-lookup)
    (let ((type (type-info-type ti)))
      (receive (prim-type out-convert back-convert cleanup)
               (type-info/prim-type+procs ti gtype-lookup)
        (values (if back-convert
                    (lambda (arg-vec)
                      (back-convert (vector-ref arg-vec i)))
                    i)
                (lambda (arg-vec val)
                  (set-pointer (vector-ref arg-vec i) ti (if out-convert (out-convert val) val)))))))

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
        (cond ((pointer? (car args))
               (vector-set! arg-vec i (car args))
               (cdr args))
              (else
               (let ((vec (->vector (car args))))
                 (vector-set! arg-vec i (convert vec))
                 (cond ((array-length-index atype)
                        => (lambda (l-index)
                             (vector-set! arg-vec l-index (vector-length vec)))))
                 (cdr args)))))))

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

  (define-callouts libglib
    (gerror-free 'void "g_error_free" '(pointer)))
  
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


  (define (make-callout rti arg-types setup-steps collect-steps cleanup-steps flags gtype-lookup)
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
          (ret-consume (ret-type-consumer rti gtype-lookup)))
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

  (define (make-callback rti arg-types prepare-steps store-steps flags gtype-lookup)
    (receive (prim-ret ret-out-convert ret-back-convert cleanup)
             (type-info/prim-type+procs rti gtype-lookup)
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

  (define (ret-type-consumer rti gtype-lookup)
    (cond ((eq? (type-info-type rti) 'void)
           #f)
          (else
           (receive (prim-type out-convert back-convert cleanup)
                    (type-info/prim-type+procs rti gtype-lookup)
             (cond (back-convert
                    (lambda (val)
                      (let ((result (back-convert val)))
                        (if cleanup (cleanup val))
                        result)))
                   (else
                    (assert (not cleanup))
                    #t))))))

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

)
