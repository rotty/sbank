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

          list->callout-options
          callout-options
          
          signature-callout
          signature-callback
          
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
          (only (spells misc) unspecific)
          (spells tracing) ;debug
          (only (spells assert) cout) ;ditto
          (sbank support utils)
          (sbank support type-data)
          (for (sbank support stypes) expand)
          (sbank support shlibs)
          (sbank ctypes basic)
          (for (sbank typelib stypes) expand)
          (sbank gobject internals data)
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

  
;;; Retrieving conversion procedures from a type-info
  
  (define (type-info/prim-type+procs ti free-spec gtype-lookup)
    (let ((type (type-info-type ti))
          (allow-none? (type-info-allow-none? ti)))
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
                      (out-converter/null string->utf8z-ptr allow-none? #f)
                      (back-converter/null utf8z-ptr->string allow-none? #f)
                      (and (free-spec-set? free-spec) free)))
             ((filename)
              (values prim-type
                      (out-converter/null string->fnamez-ptr allow-none? #f)
                      (back-converter/null fnamez-ptr->string allow-none? #f)
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
                          (free-spec-shift free-spec)
                          gtype-lookup)
                  (values 'pointer
                          (gslist-out-converter elt-out)
                          (gslist-back-converter elt-back)
                          (gslist-cleanup elt-cleanup free-spec)))))
             ((glist)
              (let ((elt-ti (car (type-info-parameters ti))))
                (receive (elt-pt elt-out elt-back elt-cleanup)
                         (type-info/prim-type+procs
                          elt-ti
                          (free-spec-shift free-spec)
                          gtype-lookup)
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
                      (type-info/prim-type+procs key-ti elt-fspec gtype-lookup))
                     ((val-pt val-out val-back val-cleanup)
                      (type-info/prim-type+procs val-ti elt-fspec gtype-lookup)))
                  (let ((class (make-ghash-class key-out val-out
                                                 key-back val-back
                                                 key-cleanup val-cleanup)))
                    (values
                      'pointer
                      (out-converter/null (ghash-out-converter class) allow-none? #f)
                      (back-converter/null (ghash-back-converter class) allow-none? #f)
                      (ghash-cleanup class free-spec))))))
             ((gtype)
              (values prim-type symbol->gtype gtype->symbol #f))
             (else
              (raise-sbank-callout-error
               "argument passing for this type not implemented" ti)))))
        ((genum? type)
         (values 'int                   ; Is int always OK?
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
                 (out-converter/null ->c-array allow-none? #f)
                 (back-converter/null (lambda (ptr)
                                        (c-array->vector ptr type #f))
                                      allow-none?
                                      #f)
                 (and (free-spec-any? free-spec)
                      (lambda (ptr)
                        (free-c-array ptr type #f free-spec)))))
        ((gobject-class? type)
         (values 'pointer
                 (out-converter/null ginstance-ptr allow-none? #f)
                 (back-converter/null (ginstance-maker gtype-lookup type)
                                      allow-none?
                                      #f)
                 #f))
        ((signature? type)
         (values 'pointer
                 (lambda (val)
                   (raise-sbank-callout-error
                    "scheme callback -> C function pointer conversion not supported in this context"))
                 ;;++FIXME: signature-callout invocation wrong
                 (back-converter/null (signature-callout type) allow-none? #f)
                 #f))
        (else
         (raise-sbank-callout-error
          "argument/return type not yet implemented" type)))))

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


  
  (define-enumeration callout-option
    (throws is-constructor is-static)
    callout-options)

  (define list->callout-options (enum-set-constructor (callout-options)))
  
  (define (signature-callout signature container options)
    (receive (real-atis setup collect cleanup)
             (signature-callout-values signature container options)
      (make-callout (signature-rti signature)
                    real-atis
                    setup
                    collect
                    cleanup
                    gtype-lookup)))

  (define (signature-callout-values signature container options)
    (let* ((constructor? (enum-set-member? 'is-constructor options))
           (has-self-ptr?
            (and container
                 (not (or constructor?
                          (enum-set-member? 'is-static options)))))
           (throws? (enum-set-member? 'throws options))
           (atis (append
                  (if has-self-ptr?
                      (list (make-arg-info container #t #f '() 'in))
                      '())
                  (signature-atis signature)
                  (if throws?
                      (list (make-arg-info gerror-type #t #f '() 'in))
                      '())))
           (rti (signature-rti signature))
           (gtype-lookup (signature-gtype-lookup signature))
           (length-indices (arg-infos-length-indices (cons rti atis)))
           (closure-indices (filter-map arg-info-closure-index atis))
           (destroy-indices (filter-map arg-info-destroy-index atis)))
      (loop continue ((for ti (in-list (reverse atis)))
                      (with setup-steps '())
                      (with collect-steps '())
                      (with cleanup-steps '())
                      (for i (down-from (length atis))))
        => (values atis setup-steps collect-steps cleanup-steps)
        (let ((length? (memv i length-indices))
              (closure? (memv i closure-indices))
              (destroy? (memv i destroy-indices)))
          (receive (setup collect cleanup)
                   (arg-callout-steps ti i gtype-lookup)
            (cond ((or length? closure? destroy?)
                   (continue (=> setup-steps (cons #f setup-steps))))
                  (else
                   (continue
                    (=> setup-steps (cons setup setup-steps))
                    (=> collect-steps (if collect
                                          (cons collect collect-steps)
                                          collect-steps))
                    (=> cleanup-steps (if cleanup
                                          (cons cleanup cleanup-steps)
                                          cleanup-steps))))))))))

  (define (arg-infos-length-indices type-infos)
    (filter-map (lambda (ti)
                  (let ((type (type-info-type ti)))
                    (and (array-type? type)
                         (array-length-index type))))
                type-infos))

  (define (arg-infos-have-out-args? type-infos)
    (any (lambda (ti)
           (memq (arg-info-direction ti) '(out in-out)))
         type-infos))

  
  (define (signature-callback signature)
    (define (callback-maker)
      (receive (prepare-steps store-steps)
               (signature-callback-values signature)
        (make-callback (signature-rti signature)
                       (signature-atis signature)
                       prepare-steps
                       store-steps
                       (signature-gtype-lookup signature))))
    (signature-cached-callback signature callback-maker))

  (define (signature-callback-values signature)
    (let* ((type-infos (signature-atis signature))
           (length-indices (arg-infos-length-indices type-infos))
           (closure-indices (filter-map arg-info-closure-index type-infos))
           (gtype-lookup (signature-gtype-lookup signature)))
      (loop continue ((with prepare-steps '())
                      (with store-steps '())
                      (for ti (in-list type-infos))
                      (for i (down-from (length type-infos))))
        => (values prepare-steps store-steps)
        (let ((ignore? (or (memv i length-indices)
                           (memv i closure-indices))))
          (receive (prepare store)
                   (arg-callback-steps ti i gtype-lookup)
            (cond (ignore?
                   (continue))
                  (else
                   (continue (=> prepare-steps (cons prepare prepare-steps))
                             (=> store-steps
                                 (if (eq? 'out (arg-info-direction ti))
                                     (cons store store-steps)
                                     store-steps))))))))))

  
  (define (args-pre-call! arg-vec arg-types)
    (loop ((for i (up-from 0))
           (for ti (in-list arg-types)))
      (let ((type (type-info-type ti))
            (direction (arg-info-direction ti)))
        (cond
          ((gobject-record-class? type)
           (when (eq? 'out direction)
             (vector-set! arg-vec i (ginstance-ptr (send type (alloc))))))
          (else
           (let ((prim-type (type-info->prim-type ti #f)))
             (case direction
               ((in-out)
                (vector-set! arg-vec i (malloc/set! prim-type (vector-ref arg-vec i))))
               ((out)
                (vector-set! arg-vec i (malloc (c-type-sizeof prim-type)))))))))))

  (define (args-post-call! arg-vec arg-types)
    (loop ((for i (up-from 0))
           (for ti (in-list arg-types)))
      (case (arg-info-direction ti)
        ((in-out out)
         (let* ((ptr (vector-ref arg-vec i))
                (val (deref-pointer ptr ti)))
           (unless (gobject-record-class? (type-info-type ti))
             (free ptr))
           (vector-set! arg-vec i val))))))

  (define (arg-callout-steps ti i gtype-lookup)
    (let ((type (type-info-type ti))
          (direction (arg-info-direction ti)))
      (define-syntax step-values
        (syntax-rules ()
          ((_ setup collect cleanup)
           (values (and (not (eq? 'out direction)) setup)
                   (and (not (eq? 'in direction)) collect)
                   cleanup))))
      (cond
       ;; Arrays are special-cased for length-parameter handling
       ((array-type? type)
        (step-values (array-arg-setup ti i)
                     (array-arg-collect ti i)
                     (array-arg-cleanup ti i)))
       ((gerror-type? type)
        (unless (eq? 'in direction)
          (raise-sbank-callout-error
           "GError arguments must have direction 'in'" ti direction))
        (values (gerror-arg-setup type i)
                #f
                (gerror-arg-cleanup type i)))
       ((signature? type)
        (unless (eq? 'in direction)
          (raise-sbank-callout-error
           "callback arguments must have direction 'in'" ti direction))
        (values (callback-arg-setup ti i gtype-lookup)
                #f
                (callback-arg-cleanup ti i)))
       ((and (need-container-copy? ti)
             (type-container-copy-proc type))
        => (lambda (copy)
             (receive (prim-type out-convert back-convert cleanup)
                      (type-info/prim-type+procs
                       ti
                       (fxior (arg-info-free-spec ti) 1)
                       gtype-lookup)
               (step-values
                (if out-convert (converter-setup/copy i out-convert copy) i)
                (if back-convert (converter-collect i back-convert) i)
                (and cleanup (cleanup-step/copy i cleanup))))))
       (else
        (receive (prim-type out-convert back-convert cleanup)
                 (type-info/prim-type+procs ti (arg-info-free-spec ti) gtype-lookup)
          (step-values (if out-convert (converter-setup i out-convert) i)
                       (if back-convert (converter-collect i back-convert) i)
                       (and cleanup (cleanup-step i cleanup))))))))

  (define (arg-callback-steps ti i gtype-lookup)
    (let ((type (type-info-type ti)))
      (receive (prim-type out-convert back-convert cleanup)
               (type-info/prim-type+procs ti (arg-info-free-spec ti) gtype-lookup)
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
  (define (converter-setup/null i convert allow-none? null-val)
    (let ((convert/null (out-converter/null convert allow-none? null-val)))
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

  (define (converter-collect/null i convert allow-none? null-val)
    (let ((convert/null (back-converter/null convert allow-none? null-val)))
      (lambda (arg-vec)
        (convert/null (vector-ref arg-vec i)))))

  (define (need-container-copy? arg-info)
    (and (eq? 'in (arg-info-direction arg-info))
         (eq? 'transfer-container-ownership (arg-info-ownership arg-info))))

  (define (type-container-copy-proc type)
    (case type
      ((glist)  g-list-copy)
      ((gslist) g-slist-copy)
      ((ghash)  g-hash-table-ref)
      (else
       (raise-sbank-callout-error
        "requested copy procedure for non-container"
        type))))


;;; Array handling

  (define (array-arg-setup ti i)
    (let* ((atype (type-info-type ti))
           (convert (out-converter/null (lambda (val)
                                          (->c-array val atype))
                                        (type-info-allow-none? ti)
                                        #f))
           (copy-container? (need-container-copy? ti)))
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

  (define (array-arg-collect ti i)
    (let ((atype (type-info-type ti)))
      (lambda (arg-vec)
        (c-array->vector (vector-ref arg-vec i)
                         atype
                         (get-array-length atype arg-vec)))))

  (define (array-arg-cleanup ti i)
    (let ((atype (type-info-type ti)))
      (let* ((copied-container? (need-container-copy? ti))
             (free-spec (fxior (arg-info-free-spec ti)
                               (if copied-container? #b1 #b0))))
        (lambda (arg-vec info-vec)
          (free-c-array (vector-ref (if copied-container? info-vec arg-vec) i)
                        atype
                        (get-array-length atype arg-vec)
                        free-spec)))))


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

  (define (out-converter/null* convert allow-none? null-val)
    (if (or (always-allow-none?) allow-none?)
        (lambda (val)
          (if (equal? val null-val)
              (values (null-pointer) #f)
              (convert val)))
        convert))

  (define (callback-arg-setup ti i gtype-lookup)
    (let* ((convert (out-converter/null* (signature-callback (type-info-type ti))
                                         (type-info-allow-none? ti)
                                         #f))
           (closure-i (arg-info-closure-index ti))
           (destroy-i (arg-info-destroy-index ti))
           (setup-destroy
            (cond ((and destroy-i
                        (eq? (arg-info-scope ti) 'notified))
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

  (define (callback-arg-cleanup ti i)
    (case (arg-info-scope ti)
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

  (define (args-collect-procedure rti gtype-lookup steps)
    (let ((ret-consume (ret-type-consumer rti gtype-lookup)))
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
                        gtype-lookup)
    (let ((prim-callout
           (make-c-callout (type-info->prim-type rti #f)
                           (map (lambda (ti)
                                  (type-info->prim-type
                                   ti
                                   (memq (arg-info-direction ti) '(out in-out))))
                                arg-types)))
          (out-args? (arg-infos-have-out-args? arg-types))
          (setup (args-setup-procedure (length arg-types) setup-steps))
          (collect (args-collect-procedure rti gtype-lookup collect-steps))
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
                     (args-pre-call! arg-vec arg-types)
                     (let ((ret-val (apply do-callout (vector->list arg-vec))))
                       (args-post-call! arg-vec arg-types)
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

  (define (make-callback rti arg-types prepare-steps store-steps gtype-lookup)
    (receive (prim-ret ret-out-convert ret-back-convert cleanup)
             (type-info/prim-type+procs rti
                                        (arg-info-free-spec rti)
                                        gtype-lookup)
      (let ((prim-callback
             (make-c-callback
              prim-ret
              (map (lambda (ti)
                     (type-info->prim-type ti (memq (arg-info-direction ti)
                                                    '(out in-out))))
                   arg-types))))
        (cond ((and (not ret-out-convert)
                    (equal? prepare-steps (iota (length arg-types))))
               prim-callback)
              (else
               (let ((arg-len (length arg-types)))
                 (lambda (proc)
                   (prim-callback
                    (make-callback-wrapper proc prim-ret ret-out-convert arg-types
                                           prepare-steps store-steps)))))))))


  (define (make-callback-wrapper proc prim-ret ret-out-convert arg-types
                                 prepare-steps store-steps)
    (let ((arg-len (length arg-types))
          (out-args? (arg-infos-have-out-args? arg-types)))
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

  (define (ret-type-consumer rti gtype-lookup)
    (let ((type (type-info-type rti)))
      (cond ((eq? type 'void)
             #f)
            ((array-type? type)
             (let ((free-spec (arg-info-free-spec rti)))
               (lambda (rv arg-vec)
                 (let ((len (get-array-length type arg-vec)))
                   (let ((result (c-array->vector rv type len)))
                     (free-c-array rv type len free-spec)
                     result)))))
            (else
             (receive (prim-type out-convert back-convert cleanup)
                      (type-info/prim-type+procs rti
                                                 (arg-info-free-spec rti)
                                                 gtype-lookup)
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
;; scheme-indent-styles: ((type-flags-case 1) foof-loop)
;; End:
