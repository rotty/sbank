;;; internals.sls --- GObject mapping implementation.

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

;;; Code:
#!r6rs

(library (sbank gobject internals)
  (export make-gobject-class gobject-class?
          gobject-class-namespace
          gobject-class-name
          gobject-class-gtype
          gobject-class-parent
          gobject-class-interfaces
          gobject-class-get-signal-callback
          gobject-class-get-signal-signature
          gobject-class-get-signal-rti
          gobject-class-get-property-info
          gobject-class-decorate
          gobject-simple-class-decorate
          gobject-method-overrider
          make-gobject-new*

          make-gobject-record-class gobject-record-class?
          make-gobject-union-class gobject-union-class?

          make-ginstance ginstance? ginstance-ptr ginstance-class
          make-ginstance/guarded
          ginstance=?
          ginstance-is-a?

          g-object-attach-destructor
          gobject-collect

          gsequence?
          make-gsequence-class

          gslist?
          make-gslist-class

          glist?
          make-glist-class

          ghash?
          make-ghash-class
          ghash->alist

          send-message
          send

          make-genum genum?
          make-gflags gflags?

          genumerated-lookup
          genumerated-values
          genumerated-symbols
          genumerated-gtype

          gerror-type
          gerror-type?)
  (import (rnrs base)
          (rnrs control)
          (rnrs lists)
          (rnrs records syntactic)
          (rnrs mutable-pairs)
          (rnrs conditions)
          (srfi :2 and-let*)
          (spells alist)
          (srfi :8 receive)
          (spells tracing)
          (only (spells assert) cout)
          (spells foreign)
          (sbank support utils)
          (sbank support shlibs)
          (sbank support callback-pool)
          (sbank support ptr-table)
          (sbank support type-data)
          (sbank ctypes call)
          (sbank gobject gtype)
          (sbank gobject gquark)
          (sbank gobject genum)
          (sbank gobject gvalue)
          (sbank gobject gparam)
          (sbank gobject glist)
          (sbank gobject ghash)
          (sbank gobject internals data))

  ;;
  ;; Object system
  ;;

  ;; An attachment contains a bunch of metadata associated with a
  ;; GObject instance over its whole lifetime, starting from when the
  ;; is first needed (and thus created). This is opposed to the
  ;; `ginstance' wrapper, which is (re)created as needed.
  (define-record-type gobject-attachment
    (fields (mutable destroy-hook))
    (protocol (lambda (p)
                (lambda ()
                  (p '())))))


  (define gobject-attachment-quark
    (let ((quark (g-quark-from-string "sbank:attachment")))
      (lambda ()
        quark)))

  (define gobject-attachments (make-ptr-table))

  (define (g-object-get-attachment obj-ptr)
    (and-let* ((att-ptr (g-object-get-qdata obj-ptr (gobject-attachment-quark)))
               ((not (null-pointer? att-ptr))))
      (ptr-table-ref gobject-attachments att-ptr #f)))

  (define (gobject-attachment-destructor att-ptr)
    (callback-destroy-notify
     (lambda ()
       (let ((att (ptr-table-ref gobject-attachments att-ptr #f)))
         (for-each (lambda (thunk)
                     (thunk))
                   (gobject-attachment-destroy-hook att))
         (ptr-table-remove! gobject-attachments att-ptr)))))

  (define (g-object-get/create-attachment obj-ptr)
    (or (g-object-get-attachment obj-ptr)
        (let* ((att (make-gobject-attachment))
               (att-ptr (ptr-table-add! gobject-attachments att)))
          (g-object-set-qdata-full obj-ptr
                                   (gobject-attachment-quark)
                                   att-ptr
                                   (gobject-attachment-destructor att-ptr))
          att)))

  ;;@ Register @2 as destructor for @1. Note that @2 should not close
  ;; over @1 (or some data structure containing a reference to @1),
  ;; otherwise @1 will become uncollectable.
  (define (g-object-attach-destructor obj-ptr proc)
    (let ((att (g-object-get/create-attachment obj-ptr)))
      (gobject-attachment-destroy-hook-set!
       att
       (cons proc (gobject-attachment-destroy-hook att)))))

  (define (gobject-class-get-signal-signature class signal)
    (lookup-signal class signal))

  (define (gobject-class-get-signal-callback class signal)
    (let ((signature (lookup-signal class signal)))
      (and signature (signature-callback signature))))

  (define (gobject-class-get-signal-rti class signal)
    (let ((signature (lookup-signal class signal)))
      (and signature (signature-rti signature))))

  (define (gobject-class-get-property-info class property)
    (lookup-property class property))

  (define (gobject-class-decorate class
                                  constructors-decorator
                                  methods-decorator
                                  signals-decorator)
    (make-gobject-class
     (gobject-class-namespace class)
     (gobject-class-name class)
     (gobject-class-gtype class)
     (lambda (new-class)
       (receive (parent interfaces constructors methods signals properties)
                ((gobject-class-load-members class) class)
         (values parent
                 interfaces
                 (constructors-decorator constructors)
                 (methods-decorator methods)
                 (signals-decorator signals)
                 properties)))))

  (define (gobject-simple-class-decorate class
                                         constructors-decorator
                                         methods-decorator)
    ((cond ((gobject-record-class? class) make-gobject-record-class)
           ((gobject-union-class? class)  make-gobject-union-class)
           (else
            (error 'gobject-simple-class-decorate
                   "cannot decorate this type of class" class)))
     (gobject-class-namespace class)
     (gobject-class-name class)
     (gobject-class-gtype class)
     (gobject-simple-class-ref class)
     (gobject-simple-class-unref class)
     (lambda (new-class)
       (receive (parent interfaces constructors methods signals properties)
                ((gobject-class-load-members class) class)
         (values (constructors-decorator constructors)
                 (methods-decorator methods))))))

  (define (apply-override typelib method override)
    (cons (car override)
          (if (lazy-entry? (cdr method))
              (make-lazy-entry
               (lambda (class)
                 (let ((next-method ((lazy-entry-proc (cdr method)) class)))
                   ((cdr override) typelib next-method))))
              ((cdr override) typelib (cdr method)))))

  (define (gobject-method-overrider typelib overrides)
    (lambda (methods)
      (let loop ((result methods) (overrides overrides))
        (if (null? overrides)
            result
            (cond ((assq (caar overrides) methods)
                   => (lambda (method)
                        (loop (cons (apply-override typelib
                                                    method
                                                    (car overrides))
                                    (filter (lambda (m)
                                              (not (eq? m method)))
                                            result))
                              (cdr overrides))))
                  (else
                   (loop (cons (cons (caar overrides)
                                     ((cdar overrides) typelib #f))
                               result)
                         (cdr overrides))))))))

  (define lookup-property (make-gobject-class-lookup gobject-class-properties))
  (define lookup-signal (make-gobject-class-lookup gobject-class-signals))

  (define (make-gobject-new* type->gtype)
    (lambda (class)
      (lambda props/vals
        (let ((n (length props/vals)))
          (when (odd? n)
            (error 'gobject-new*
                   "odd number of colum/value arguments" props/vals))
          (let ((parameters
                 (if (= n 0) (null-pointer) (g-param-alloc (/ n 2)))))
            (let loop ((i 0) (props/vals props/vals))
              (cond ((null? props/vals)
                     (let ((obj
                            (make-ginstance/guarded
                             class
                             (g-object-newv (gobject-class-gtype class)
                                            i
                                            parameters)
                             'sink)))
                       (do ((j 0 (+ j 1)))
                           ((>= j i))
                         (let ((param (pointer+ parameters
                                                (* j g-param-size))))
                           (free (g-param-name param))
                           (g-value-unset! (g-param-value param))))
                       (unless (= n 0)
                         (free parameters))
                       obj))
                    (else
                     (let* ((prop (car props/vals))
                            (prop-name (string->utf8z-ptr (symbol->string prop)))
                            (param (pointer+ parameters (* i g-param-size)))
                            (gv (g-param-value param))
                            (gtype (type->gtype
                                    (gobject-class-property-type class prop)))
                            (val (cadr props/vals)))
                       (g-param-name-set! param prop-name)
                       (g-value-init! gv gtype)
                       (g-value-set! gv (cond ((ginstance? val)
                                               (ginstance-ptr val))
                                              (else
                                               val))))
                     (loop (+ i 1) (cddr props/vals))))))))))

  (define (gobject-class-property-type class prop)
    (property-info-type (gobject-class-get-property-info class prop)))

  (define-c-callouts libgobject
    (g-object-newv 'pointer "g_object_newv" (list gtype-ctype 'uint 'pointer))
    (g-object-is-floating 'int "g_object_is_floating" '(pointer))
    (g-object-get-qdata 'pointer "g_object_get_qdata" `(pointer ,g-quark-ctype))
    (g-object-set-qdata-full 'void "g_object_set_qdata_full"
                             `(pointer ,g-quark-ctype pointer fpointer)))

  )
