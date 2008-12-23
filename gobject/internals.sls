;;; internals.sls --- GObject mapping implementation.

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

;;; Code:

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
          gobject-method-overrider
          make-gobject-new/props

          make-gobject-record-class gobject-record-class?
          make-gobject-union-class gobject-union-class?

          make-ginstance ginstance? ginstance-ptr ginstance-class
          ginstance=?

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

          gerror-type? make-gerror-type
          gerror? make-gerror gerror-domain gerror-code)
  (import (rnrs base)
          (rnrs control)
          (rnrs lists)
          (rnrs records syntactic)
          (rnrs mutable-pairs)
          (rnrs conditions)
          (xitomatl srfi and-let*)
          (spells alist)
          (spells receive)
          (spells tracing)
          (only (spells assert) cout)
          (spells foreign)
          (sbank type-data)
          (sbank support utils)
          (sbank support shlibs)
          (sbank gobject gtype)
          (sbank gobject genum)
          (sbank gobject gvalue)
          (sbank gobject gparam)
          (sbank gobject glist)
          (sbank gobject ghash))

  ;;
  ;; Object system
  ;;

  (define-record-type ginstance
    (fields (immutable class ginstance-class)
            (immutable ptr ginstance-ptr)))

  (define (ginstance=? x y)
    (unless (and (ginstance? x) (ginstance? y))
      (error 'ginstance=? "invalid argument types" x y))
    (pointer=? (ginstance-ptr x) (ginstance-ptr y)))

  (define-record-type gobject-class
    ;;(opaque #t)
    (fields namespace
            name
            gtype
            (mutable load-members)
            (mutable parent)
            (mutable interfaces)
            (mutable constructors)
            (mutable methods)
            (mutable signals)
            (mutable properties))
    (protocol (lambda (p)
                (lambda (namespace name gtype load-members)
                  (p namespace name gtype load-members #f #f #f #f #f #f)))))

  ;; A simple class is one that is not part of the GObject class
  ;; hierarchy, e.g. plain structs and unions. It has therefore no
  ;; parent, no interfaces, no signals and no properties.
  (define-record-type gobject-simple-class
    (parent gobject-class)
    (protocol (lambda (n)
                (lambda (namespace name gtype load-members)
                  (let ((p (n namespace name gtype
                              (lambda (class)
                                (receive (constructors methods) (load-members class)
                                  (values #f '() constructors methods '() '()))))))
                    (p))))))

  (define-record-type gobject-record-class
    (parent gobject-simple-class))

  (define-record-type gobject-union-class
    (parent gobject-simple-class))

  (define-record-type gsequence-class
    (parent gobject-simple-class)
    (fields elt-out elt-back elt-cleanup)
    (protocol
     (lambda (n)
       (lambda (namespace name constructors methods
                          elt-out elt-back elt-cleanup)
         (let ((p (n namespace name #f (lambda (class)
                                         (values constructors methods)))))
           (p elt-out elt-back elt-cleanup))))))

  (define-record-type gmapping-class
    (parent gobject-simple-class)
    (fields key-out val-out key-back val-back key-cleanup val-cleanup)
    (protocol
     (lambda (n)
       (lambda (namespace name constructors methods
                          key-out val-out
                          key-back val-back
                          key-cleanup val-cleanup)
         (let ((p (n namespace name #f (lambda (class)
                                         (values constructors methods)))))
           (p key-out val-out key-back val-back key-cleanup val-cleanup))))))

  (define-record-type gslist-class
    (parent gsequence-class)
    (protocol (lambda (n)
                (lambda (elt-out elt-back elt-cleanup)
                  (let ((p (n "GLib" "SList" '() '() elt-out elt-back elt-cleanup)))
                    (p))))))

  (define-record-type glist-class
    (parent gsequence-class)
    (protocol (lambda (n)
                (lambda (elt-out elt-back elt-cleanup)
                  (let ((p (n "GLib" "List" '() '() elt-out elt-back elt-cleanup)))
                    (p))))))

  (define-record-type ghash-class
    (parent gmapping-class)
    (protocol
     (lambda (n)
       (lambda (key-out val-out key-back val-back key-cleanup val-cleanup)
         (let ((p (n "GLib" "HashTable"
                     '()
                     `((foreach . ,ghash-foreach)
                       (->alist . ,ghash->alist))
                     key-out val-out key-back val-back key-cleanup val-cleanup)))
           (p))))))

  (define (gsequence? x)
    (and (ginstance? x)
         (gsequence-class? (ginstance-class x))))

  (define (gslist? x)
    (and (ginstance? x)
         (gslist-class? (ginstance-class x))))

  (define (glist? x)
    (and (ginstance? x)
         (gslist-class? (ginstance-class x))))

  (define (ghash? x)
    (and (ginstance? x)
         (ghash-class? (ginstance-class x))))

  (define (ghash-foreach ghash proc)
    (unless (ghash? ghash)
      (error 'ghash-foreach "need a GHash instance" ghash))
    (let* ((class (ginstance-class ghash))
           (key-back (gmapping-class-key-back class ))
           (val-back (gmapping-class-val-back class)))
      (g-hash-table-foreach (ginstance-ptr ghash)
                            (lambda (key val user-data)
                              (proc (key-back key) (val-back val)))
                            (null-pointer))))

  (define (ghash->alist ghash)
    (cond ((eqv? ghash #f) '())
          (else
           (let ((result '()))
             (ghash-foreach ghash (lambda (key val)
                                    (set! result (cons (cons key val) result))))
             result))))

  (define-record-type gerror-type)

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

  (define (apply-override method override)
    (cons (car override)
          (if (lazy-entry? (cdr method))
              (make-lazy-entry
               (lambda (class)
                 (let ((next-method ((lazy-entry-proc (cdr method)) class)))
                   ((cdr override) next-method))))
              ((cdr override) (cdr method)))))

  (define (gobject-method-overrider overrides)
    (lambda (methods)
      (let loop ((result methods) (overrides overrides))
        (if (null? overrides)
            result
            (cond ((assq (caar overrides) methods)
                   => (lambda (method)
                        (loop (cons (apply-override method (car overrides))
                                    (filter (lambda (m)
                                              (not (eq? m method)))
                                            result))
                              (cdr overrides))))
                  (else
                   (loop (cons (cons (caar overrides)
                                     ((cdar overrides) #f))
                               result)
                         (cdr overrides))))))))

  (define lookup-method (make-gobject-class-lookup gobject-class-methods))
  (define lookup-property (make-gobject-class-lookup gobject-class-properties))
  (define lookup-signal (make-gobject-class-lookup gobject-class-signals))

  (define (make-gobject-class-lookup accessor)
    (define (lookup class name)
      (cond ((assq name (accessor class))
             => (lambda (entry)
                  (when (lazy-entry? (cdr entry))
                    (set-cdr! entry ((lazy-entry-proc (cdr entry)) class)))
                  (cdr entry)))
            ((and-let* ((parent (gobject-class-parent class)))
               (gobject-class-force! parent)
               (lookup parent name)) => values)
            (else
             (let loop ((ifaces (gobject-class-interfaces class)))
               (cond ((null? ifaces) #f)
                     ((let ((iface (car ifaces)))
                        (gobject-class-force! iface)
                        (lookup iface name)) => values)
                     (else
                      (loop (cdr ifaces))))))))
    lookup)

  (define (gobject-class-force! class)
    (cond ((gobject-class-load-members class)
           => (lambda (loader)
                (receive (parent interfaces constructors methods signals properties)
                         (loader class)
                  (gobject-class-parent-set! class parent)
                  (gobject-class-interfaces-set! class interfaces)
                  (gobject-class-constructors-set! class constructors)
                  (gobject-class-methods-set! class methods)
                  (gobject-class-signals-set! class signals)
                  (gobject-class-properties-set! class properties)
                  (gobject-class-load-members-set! class #f))))))

  (define (make-gobject-new/props type->gtype)
    (lambda (class)
      (lambda props/vals
        (let ((n (length props/vals)))
          (when (odd? n)
            (error 'gobject-new/props
                   "odd number of colum/value arguments" props/vals))
          (let ((parameters
                 (if (= n 0) (null-pointer) (g-param-alloc (/ n 2)))))
            (let loop ((i 0) (props/vals props/vals))
              (cond ((null? props/vals)
                     (make-ginstance
                      class
                      (g-object-newv (gobject-class-gtype class) i parameters)))
                    (else
                     (let* ((prop (car props/vals))
                            (prop-name (string->utf8z-ptr (symbol->string prop)))
                            (param (pointer+ parameters (* i g-param-size)))
                            (gv (g-param-value param))
                            (gtype (type->gtype
                                    (gobject-class-property-type class prop))))
                       (g-param-name-set! param prop-name)
                       (g-value-init! gv gtype)
                       (g-value-set! gv (cadr props/vals)))
                     (loop (+ i 1) (cddr props/vals))))))))))

  (define (gobject-class-property-type class prop)
    (property-info-type (gobject-class-get-property-info class prop)))

  (define (send-message obj msg . args)
    (cond ((ginstance? obj)
           (let ((method (send-message (ginstance-class obj) msg)))
             (apply method obj args)))
          (else
           (gobject-class-force! obj)
           (cond ((assq msg (gobject-class-constructors obj))
                  => (lambda (entry)
                       (when (lazy-entry? (cdr entry))
                         (set-cdr! entry ((lazy-entry-proc (cdr entry)) obj)))
                       (apply (cdr entry) args)))
                 ((lookup-method obj msg)
                  => (lambda (proc)
                       (unless (null? args)
                         (error 'send-message
                                "cannot send message with arguments to class"
                                obj msg args))
                       proc))
                 (else
                  (error 'send-message "message not understood" obj msg args))))))

  (define-syntax send
    (syntax-rules ()
      ((send obj (msg arg ...) ...)
       (begin (send-message obj 'msg arg ...) ...))))


  (define-condition-type &gerror &error
    make-gerror gerror?
    (domain gerror-domain)
    (code gerror-code))

  (define-callouts libgobject
    (g-object-newv 'pointer "g_object_newv" (list gtype-ctype 'uint 'pointer))))
