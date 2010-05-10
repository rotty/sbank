;;; call.sls --- Dealing with C types and callouts/callbacks

;; Copyright (C) 2008, 2009, 2010 Andreas Rottmann <a.rottmann@gmx.at>

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

(library (sbank ctypes call)
  (export type-info/prim-type+procs

          make-callout
          make-callback
          arg-callout-steps
          arg-callback-steps)

  (import (rnrs base)
          (rnrs control)
          (rnrs arithmetic fixnums)
          (rnrs enums)
          (rnrs conditions)
          (rnrs exceptions)
          (rnrs bytevectors)
          (srfi :2 and-let*)
          (srfi :8 receive)
          (srfi :39 parameters)
          (wak foof-loop)
          (spells foreign)
          (except (srfi :1 lists) for-each map)
          (only (spells assert) cerr cout)
          (only (spells misc) unspecific)
          (spells tracing)
          (sbank support utils)
          (sbank support type-data)
          (for (sbank support stypes) expand)
          (sbank support shlibs)
          (sbank ctypes basic)
          (for (sbank typelib stypes) expand)
          (sbank gobject internals)
          (sbank gobject gtype)
          (sbank gobject genum)
          (sbank gobject gvalue)
          (sbank gobject glist)
          (sbank gobject ghash)
          (sbank support conditions))

  (define-syntax debug
    (syntax-rules ()
      ((debug <expr> ...)
       (for-each display (list "DEBUG: " <expr> ... "\n")))))

  ;;(define-syntax debug (syntax-rules () (begin)))

  (define type-info/prim-type+procs
    (case-lambda
      ((ti free-spec null-ok?)
       (let ((type (type-info-type ti))
             (null-ok? (or null-ok? (type-info-null-ok? ti))))
         (cond
          ((symbol? type)
           (let ((prim-type (type-tag-symbol->prim-type type)))
             (case type
               ((int8 uint8 int16 uint16 int32 uint32 int64 uint64
                      short ushort int uint long ulong ssize size time_t
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
                        (and (free-spec-set? free-spec) free)))
               ((filename)
                (values prim-type
                        (out-converter/null string->fnamez-ptr null-ok? #f)
                        (back-converter/null fnamez-ptr->string null-ok? #f)
                        (and (free-spec-set? free-spec) free)))
               ((void)
                (values 'void #f #f #f))
               ((pointer)
                (values 'pointer #f #f #f))
               ((gvalue)
                (values 'pointer
                        ;; FIXME: pointers ain't disjunctive
                        (lambda (v) (if (pointer? v) v (->g-value v #f)))
                        g-value-ref
                        #f))
               ((gslist)
                (let ((elt-ti (car (type-info-parameters ti))))
                  (receive (elt-pt elt-out elt-back elt-cleanup)
                           (type-info/prim-type+procs
                            elt-ti
                            (free-spec-shift free-spec))
                    (values 'pointer
                            (gslist-out-converter elt-out)
                            (gslist-back-converter elt-back)
                            (gslist-cleanup elt-cleanup free-spec)))))
               ((glist)
                (let ((elt-ti (car (type-info-parameters ti))))
                  (receive (elt-pt elt-out elt-back elt-cleanup)
                           (type-info/prim-type+procs
                            elt-ti
                            (free-spec-shift free-spec))
                    (values 'pointer
                            (glist-out-converter elt-out)
                            (glist-back-converter elt-back)
                            (glist-cleanup elt-cleanup free-spec)))))
               ((ghash)
                (let ((key-ti (car (type-info-parameters ti)))
                      (val-ti (cadr (type-info-parameters ti)))
                      (elt-fspec (free-spec-shift free-spec)))
                  (let-values
                      (((key-pt key-out key-back key-cleanup)
                        (type-info/prim-type+procs key-ti elt-fspec))
                       ((val-pt val-out val-back val-cleanup)
                        (type-info/prim-type+procs val-ti elt-fspec)))
                    (let ((class (make-ghash-class key-out val-out
                                                   key-back val-back
                                                   key-cleanup val-cleanup)))
                      (values
                       'pointer
                       (out-converter/null (ghash-out-converter class) null-ok? #f)
                       (back-converter/null (ghash-back-converter class) null-ok? #f)
                       (ghash-cleanup class free-spec))))))
               ((gtype)
                (values prim-type symbol->gtype gtype->symbol #f))
               (else
                (raise-sbank-callout-error
                 "argument passing for this type not implemented" ti)))))
          ((genum? type)
           (values 'int                 ; Is int always OK?
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
                   (back-converter/null (lambda (ptr)
                                          (c-array->vector ptr type #f))
                                        null-ok?
                                        #f)
                   (and (free-spec-any? free-spec)
                        (lambda (ptr)
                          (free-c-array ptr type #f free-spec)))))
          ((gobject-class? type)
           (values 'pointer
                   (out-converter/null ginstance-ptr null-ok? #f)
                   (back-converter/null (ginstance-maker gtype-lookup type)
                                        null-ok?
                                        #f)
                   #f))
          ((signature? type)
           (values 'pointer
                   (lambda (val)
                     (raise-sbank-callout-error
                      "scheme callback -> C function pointer conversion not supported in this context"))
                   (back-converter/null (signature-callout type) null-ok? #f)
                   #f))
          (else
           (raise-sbank-callout-error
            "argument/return type not yet implemented" type)))))
      ((ti free-spec)
       (type-info/prim-type+procs ti free-spec #f))
      ((ti)
       (type-info/prim-type+procs ti -1 #f))))

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

  (define (gslist-back-converter elt-back)
    (lambda (gslist)
      (gslist->list gslist elt-back)))

  (define (gslist-cleanup elt-cleanup free-spec)
    (lambda (gslist)
      (when elt-cleanup
        (let loop ((gslist gslist))
          (unless (null-pointer? gslist)
            (elt-cleanup (g-slist-data gslist))
            (loop (g-slist-next gslist)))))
      (when (free-spec-set? free-spec)
        (g-slist-free gslist))))

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

  (define (glist-back-converter elt-back)
    (lambda (glist)
      (glist->list glist elt-back)))

  (define (glist-cleanup elt-cleanup free-spec)
    (lambda (glist)
      (when elt-cleanup
        (let loop ((glist glist))
          (unless (null-pointer? glist)
            (elt-cleanup (g-list-data glist))
            (loop (g-list-next glist)))))
      (when (free-spec-set? free-spec)
        (g-list-free glist))))

  (define (ghash-out-converter class)
    (lambda (val)
      (unless (ghash? val)
        (raise-sbank-callout-error "expected an GHash instance" val))
      (ginstance-ptr val)))

  (define (ghash-back-converter class)
    (lambda (ghash-ptr)
      (make-ginstance class ghash-ptr)))

  (define (ghash-cleanup class free-spec)
    (lambda (val)
      #f))

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
               (make-ginstance/guarded class instance 'ref))))))


  (define-syntax define-accessors (stype-accessor-definer (typelib-stypes)))
  
  (define-accessors "GTypeInstance"
    (gtype-instance-class "g_class"))

  (define-accessors "GTypeClass"
    (gtype-class-gtype "g_type"))

  
  (define (args-pre-call! arg-vec arg-types flags)
    (do ((i 0 (+ i 1))
         (arg-types arg-types (cdr arg-types))
         (flags flags (cdr flags)))
        ((>= i (vector-length arg-vec)))
      (let* ((ti (car arg-types))
             (type (type-info-type ti)))
        (cond
         ((gobject-record-class? type)
          (when (enum-set-member? 'out (car flags))
            (vector-set! arg-vec i (ginstance-ptr (send type (alloc))))))
         (else
          (let ((prim-type (type-info->prim-type ti #f)))
            (arg-flags-case (car flags)
              ((in-out)
               (vector-set! arg-vec i (malloc/set! prim-type (vector-ref arg-vec i))))
              ((out)
               (vector-set! arg-vec i (malloc (c-type-sizeof prim-type)))))))))))

  (define (args-post-call! arg-vec arg-types flags)
    (do ((i 0 (+ i 1))
         (arg-types arg-types (cdr arg-types))
         (flags flags (cdr flags)))
        ((>= i (vector-length arg-vec)))
      (arg-flags-case (car flags)
        ((in-out out)
         (let* ((ti (car arg-types))
                (ptr (vector-ref arg-vec i))
                (val (deref-pointer ptr ti)))
           (unless (gobject-record-class? (type-info-type ti))
             (free ptr))
           (vector-set! arg-vec i val))))))

  (define (arg-callout-steps has-self-ptr? ti i flags gtype-lookup)
    (define-syntax step-values
      (syntax-rules ()
        ((_ setup collect cleanup)
         (values (and (not (enum-set-member? 'out flags)) setup)
                 (and (not (enum-set-member? 'in flags)) collect)
                 cleanup))))
    (let ((type (type-info-type ti)))
      (cond
       ;; Arrays are special-cased for length-parameter handling
       ((array-type? type)
        (step-values (array-arg-setup type flags i (type-info-null-ok? ti))
                     (array-arg-collect type i)
                     (array-arg-cleanup type flags i)))
       ((gerror-type? type)
        (unless (enum-set-member? 'in flags)
          (raise-sbank-callout-error
           "GError arguments must have direction 'in'" ti flags))
        (values (gerror-arg-setup type i)
                #f
                (gerror-arg-cleanup type i)))
       ((signature? type)
        (unless (enum-set-member? 'in flags)
          (raise-sbank-callout-error
           "callback arguments must have direction 'in'" ti flags))
        (values (callback-arg-setup ti i)
                #f
                (callback-arg-cleanup has-self-ptr? ti i)))
       ((and (need-container-copy? flags)
             (type-container-copy-proc type))
        => (lambda (copy)
             (receive (prim-type out-convert back-convert cleanup)
                      (type-info/prim-type+procs ti (fxior (arg-flags->free-spec flags) 1))
               (step-values (if out-convert (converter-setup/copy i out-convert copy) i)
                            (if back-convert (converter-collect i back-convert) i)
                            (and cleanup (cleanup-step/copy i cleanup))))))
       (else
        (receive (prim-type out-convert back-convert cleanup)
                 (type-info/prim-type+procs ti (arg-flags->free-spec flags))
          (step-values (if out-convert (converter-setup i out-convert) i)
                       (if back-convert (converter-collect i back-convert) i)
                       (and cleanup (cleanup-step i cleanup))))))))

  (define (arg-callback-steps ti i gtype-lookup)
    (let ((type (type-info-type ti)))
      (receive (prim-type out-convert back-convert cleanup)
               (type-info/prim-type+procs ti)
        (values (if back-convert
                    (lambda (arg-vec)
                      (back-convert (vector-ref arg-vec i)))
                    i)
                (lambda (arg-vec val)
                  (set-pointer (vector-ref arg-vec i) ti
                               (if out-convert (out-convert val) val)))))))

  (define (cleanup-step i cleanup-proc)
    (lambda (arg-vec info-vec)
      (cleanup-proc (vector-ref arg-vec i))))

  (define (cleanup-step/copy i cleanup-proc)
    (lambda (arg-vec info-vec)
      (cleanup-proc (vector-ref info-vec i))))
  
  ;; This returns and expects a converter that returns a pointer
  (define (converter-setup/null i convert null-ok? null-val)
    (let ((convert/null (out-converter/null convert null-ok? null-val)))
      (lambda (args arg-vec info-vec)
        (vector-set! arg-vec i (convert (car args)))
        (cdr args))))

  (define (converter-setup i convert)
    (lambda (args arg-vec info-vec)
      (vector-set! arg-vec i (convert (car args)))
      (cdr args)))

  (define (converter-setup/copy i convert copy)
    (lambda (args arg-vec info-vec)
      (let ((val (convert (car args))))
        (vector-set! arg-vec i val)
        (vector-set! info-vec i (copy val))
        (cdr args))))
  
  (define (converter-collect i convert)
    (lambda (arg-vec)
      (convert (vector-ref arg-vec i))))

  (define (converter-collect/null i convert null-ok? null-val)
    (let ((convert/null (back-converter/null convert null-ok? null-val)))
      (lambda (arg-vec)
        (convert/null (vector-ref arg-vec i)))))

  (define (need-container-copy? arg-flags)
    (and (enum-set-member? 'in arg-flags)
         (enum-set-member? 'transfer-container-ownership arg-flags)))

  (define (type-container-copy-proc type)
    (case type
      ((glist)  g-list-copy)
      ((gslist) g-slist-copy)
      ((ghash)  g-hash-table-ref)
      (else
       (raise-sbank-callout-error
        "requested copy procedure for non-container"
        type))))


;;; Arrray handling

  (define (array-arg-setup atype flags i null-ok?)
    (let ((convert (out-converter/null (lambda (val)
                                         (->c-array val atype)) null-ok? #f))
          (copy-container? (need-container-copy? flags)))
      (lambda (args arg-vec info-vec)
        (cond ((pointer? (car args))
               (vector-set! arg-vec i (car args))
               (cdr args))
              (else
               (let* ((val (cond ((or (vector? (car args))
                                      (bytevector? (car args))
                                      (bytevector-portion? (car args)))
                                  (car args))
                                 ((string? (car args))
                                  (string->utf8 (car args)))
                                 (else
                                  (->vector (car args)))))
                      (ptr (convert val)))
                 (vector-set! arg-vec i ptr)
                 (vector-set! info-vec i
                              (if copy-container?
                                  (memcpy
                                   (malloc (array-sizeof atype (vec-length val)))
                                   ptr)
                                  ptr))
                 (cond ((array-length-index atype)
                        => (lambda (l-index)
                             (vector-set! arg-vec l-index (vec-length val)))))
                 (cdr args)))))))

  (define (array-sizeof atype len)
    (* len (c-type-sizeof (type-info->prim-type (array-element-type atype)))))

  (define (get-array-length atype arg-vec)
    (cond ((array-length-index atype)
           => (lambda (l-index)
                (vector-ref arg-vec l-index)))
          (else
           #f)))

  (define (array-arg-collect atype i)
    (lambda (arg-vec)
      (c-array->vector (vector-ref arg-vec i) atype
                       (get-array-length atype arg-vec))))

  (define (array-arg-cleanup atype flags i)
    (let* ((copied-container? (need-container-copy? flags))
           (free-spec (fxior (arg-flags->free-spec flags)
                             (if copied-container? #b1 #b0))))
      (lambda (arg-vec info-vec)
        (free-c-array (vector-ref (if copied-container? info-vec arg-vec) i)
                      atype
                      (get-array-length atype arg-vec)
                      free-spec))))


;;; GError handling

  (define (gerror-arg-setup etype i)
    (lambda (args arg-vec info-vec)
      (vector-set! arg-vec i (malloc/set! 'pointer (null-pointer)))
      args))

  (define (gerror-arg-cleanup etype i)
    (lambda (arg-vec info-vec)
      (let* ((gerror-ptr (vector-ref arg-vec i))
             (gerror (pointer-ptr-ref gerror-ptr 0)))
        (free gerror-ptr)
        (unless (null-pointer? gerror)
          (raise (apply condition
                        (make-sbank-callout-error)
                        (gerror-conditions/free etype gerror)))))))

;;; Callback handling

  (define (out-converter/null* convert null-ok? null-val)
    (if (or (null-ok-always-on?) null-ok?)
        (lambda (val)
          (if (equal? val null-val)
              (values (null-pointer) #f)
              (convert val)))
        convert))

  (define (callback-arg-setup ti i)
    (let* ((convert (out-converter/null* (signature-callback (type-info-type ti))
                                         (type-info-null-ok? ti)
                                         #f))
           (closure-i (type-info-closure-index ti))
           (destroy-i (type-info-destroy-index ti))
           (setup-destroy
            (cond ((and destroy-i
                        (eq? (type-info-scope ti) 'notified))
                   (lambda (arg-vec reclaim)
                     (vector-set! arg-vec
                                  destroy-i
                                  (callback-destroy-notify reclaim))))
                  (destroy-i
                   (lambda (arg-vec reclaim)
                     (vector-set! arg-vec destroy-i (null-pointer))))
                  (else
                   (lambda (arg-vec reclaim)
                     #f)))))
      (lambda (args arg-vec info-vec)
        (receive (ptr reclaim) (convert (car args))
          (vector-set! arg-vec i ptr)
          (vector-set! info-vec i reclaim)
          (when closure-i
            (vector-set! arg-vec closure-i (null-pointer)))
          (setup-destroy arg-vec reclaim)
          (cdr args)))))

  (define (callback-arg-cleanup has-self-ptr? ti i)
    (case (type-info-scope ti)
      ((call)
       (lambda (argv-vec info-vec)
         ((vector-ref info-vec i))))
      (else
       #f)))

;;; The three main steps: setup, collect and cleanup

  (define (args-setup-procedure n-args steps)
    (define (lose msg . irritants)
      (apply raise-sbank-callout-error msg irritants))
    (if (equal? steps (iota n-args))
        #f
        (lambda (in-args)
          (let ((arg-vec (make-vector n-args))
                (info-vec (make-vector n-args)))
            (let loop ((args in-args) (steps steps))
              (cond ((null? steps)
                     (unless (null? args)
                       (lose "unprocessed arguments" args))
                     (values arg-vec info-vec))
                    ((integer? (car steps))
                     (vector-set! arg-vec (car steps) (car args))
                     (loop (cdr args) (cdr steps)))
                    ((eqv? (car steps) #f)
                     (loop args (cdr steps)))
                    (else
                     (loop ((car steps) args arg-vec info-vec)
                           (cdr steps)))))))))

  (define (args-collect-procedure rti ret-flags gtype-lookup steps)
    (let ((ret-consume (ret-type-consumer rti ret-flags gtype-lookup)))
      (cond ((and (not ret-consume) (null? steps))
             #f)
            (else
             (lambda (ret-val arg-vec)
               (loop ((for step (in-list steps))
                      (for results
                           (listing-reverse
                            (initial
                             (cond ((not ret-consume) '())
                                   ((procedure? ret-consume)
                                    (list (ret-consume ret-val arg-vec)))
                                   (else (list ret-val))))
                            (cond ((fixnum? step)
                                   (vector-ref arg-vec step))
                                  (else
                                   (step arg-vec))))))
                 => (reverse results)))))))

  (define (args-cleanup-procedure steps)
    (if (null? steps)
        #f
        (lambda (arg-vec info-vec)
          (for-each (lambda (step) (step arg-vec info-vec)) steps))))



  (define (vec-length x)
    (cond ((vector? x) (vector-length x))
          ((bytevector? x) (bytevector-length x))
          ((bytevector-portion? x) (bytevector-portion-count x))
          (else (error 'vec-length "called with non-vector" x))))

  ;;; The callout dispatcher
  (define (make-callout rti arg-types
                        setup-steps collect-steps cleanup-steps
                        ret-flags flags
                        gtype-lookup)
    (let ((prim-callout
           (make-c-callout (type-info->prim-type rti #f)
                           (map (lambda (type aflags)
                                  (type-info->prim-type
                                   type
                                   (arg-flags-any? aflags (arg-flags out in-out))))
                                arg-types
                                flags)))
          (out-args? (any (lambda (aflags)
                            (arg-flags-any? aflags (arg-flags out in-out)))
                          flags))
          (setup (args-setup-procedure (length arg-types) setup-steps))
          (collect (args-collect-procedure
                    rti ret-flags gtype-lookup
                    collect-steps))
          (cleanup (args-cleanup-procedure cleanup-steps)))
      (cond (out-args?
             ;; The most general, complex case
             (lambda (ptr)
               (let ((do-callout (prim-callout ptr)))
                 (lambda args
                   (receive (arg-vec info-vec)
                            (if setup
                                (setup args)
                                (values (list->vector args) #f))
                     (args-pre-call! arg-vec arg-types flags)
                     (let ((ret-val (apply do-callout (vector->list arg-vec))))
                       (args-post-call! arg-vec arg-types flags)
                       (let ((results (if collect (collect ret-val arg-vec) '())))
                         (when cleanup (cleanup arg-vec info-vec))
                         (apply values results))))))))
            (setup
             (lambda (ptr)
               (let ((do-callout (prim-callout ptr)))
                 (lambda args
                   (receive (arg-vec info-vec) (setup args)
                     (let ((ret-val (apply do-callout (vector->list arg-vec))))
                       (when cleanup (cleanup arg-vec info-vec))
                       (if collect
                           (apply values (collect ret-val #f))
                           ret-val)))))))
            (collect
             (assert (not cleanup))
             (lambda (ptr)
               (lambda args
                 (let ((do-callout (prim-callout ptr)))
                   (apply values (collect (apply do-callout args) #f))))))
            (else
             prim-callout))))

  (define (make-callback rti arg-types prepare-steps store-steps flags gtype-lookup)
    (receive (prim-ret ret-out-convert ret-back-convert cleanup)
             (type-info/prim-type+procs rti)
      (let ((prim-callback
             (make-c-callback
              prim-ret
              (map (lambda (type aflags)
                     (type-info->prim-type
                      type
                      (arg-flags-any? aflags (arg-flags out in-out))))
                   arg-types
                   flags))))
        (cond ((and (not ret-out-convert)
                    (equal? prepare-steps (iota (length arg-types))))
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
          (out-args? (any (lambda (aflags)
                            (arg-flags-any? aflags (arg-flags out in-out)))
                          flags)))
      (lambda args
        (assert (= arg-len (length args)))
        (let* ((arg-vec (list->vector args))
               (args (let loop ((args '()) (steps prepare-steps))
                       (if (null? steps)
                           (reverse args)
                           (loop (cons
                                  (cond ((integer? (car steps))
                                         (vector-ref arg-vec (car steps)))
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
                       (raise-sbank-callback-error
                        "called procedure returned not enough values"
                        ret-values prim-ret))
                     (cond ((eq? prim-ret 'void)
                            (unspecific))
                           (ret-out-convert
                            (ret-out-convert (car ret-values)))
                           (else
                            (car ret-values))))
                    ((null? steps)
                     (raise-sbank-callback-error
                      "called procedure returned too many values"
                      ret-values prim-ret))
                    (else
                     (loop ((car steps) (car vals) arg-vec))))))))))

  (define (ret-type-consumer rti flags gtype-lookup)
    (let ((type (type-info-type rti)))
      (cond ((eq? type 'void)
             #f)
            ((array-type? type)
             (let ((free-spec (arg-flags->free-spec flags)))
               (lambda (rv arg-vec)
                 (let ((len (get-array-length type arg-vec)))
                   (let ((result (c-array->vector rv type len)))
                     (free-c-array rv type len free-spec)
                     result)))))
            (else
             (receive (prim-type out-convert back-convert cleanup)
                      (type-info/prim-type+procs rti (arg-flags->free-spec flags) #t)
               (cond (back-convert
                      (lambda (val arg-vec)
                        (let ((result (back-convert val)))
                          (and cleanup (cleanup val))
                          result)))
                     (else
                      (assert (not cleanup))
                      #t)))))))

)

;; Local Variables:
;; scheme-indent-styles: ((arg-flags-case 1) foof-loop)
;; End:
