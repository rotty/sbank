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
          gobject-class-size
          gobject-class-decorate
          gobject-method-overrider

          make-gobject-record-class gobject-record-class?
          make-gobject-union-class gobject-union-class?
          
          make-ginstance ginstance? ginstance-ptr ginstance-class

          send-message
          send

          make-genum genum? genum-lookup genum-values genum-symbols genum-gtype

          gerror-type? make-gerror-type)
  (import (rnrs base)
          (rnrs control)
          (rnrs lists)
          (rnrs records syntactic)
          (rnrs mutable-pairs)
          (xitomatl srfi and-let*)
          (spells alist)
          (spells receive)
          (spells tracing)
          (only (spells assert) cout)
          (spells foreign)
          (sbank type-data)
          (sbank utils))

  ;;
  ;; Object system
  ;;

  (define-record-type ginstance
    (fields (immutable class ginstance-class)
            (immutable ptr ginstance-ptr)))

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

  (define-record-type gobject-record-class
    (parent gobject-class))

  (define-record-type gobject-union-class
    (parent gobject-class))

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

  (define (gobject-class-size class)
    ;; FIXME: gross hack, we need to calc size based on fields
    (* 14 (c-type-sizeof 'pointer)))
  
  (define (gobject-class-decorate class
                                  constructors-decorator
                                  methods-decorator
                                  signals-decorator)
    (make-gobject-class (gobject-class-namespace class)
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
              (make-lazy-entry (lambda (class)
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


  ;;
  ;; Enumerations and flags
  ;;
  (define-record-type genum
    (fields gtype symbols values)
    (protocol (lambda (p)
                (lambda (gtype alist)
                  (p gtype (list->vector (map car alist)) (list->vector (map cdr alist)))))))

  ;; Note: this could be made more efficient by using sorted vectors
  ;; (but only in one direction)
  (define (genum-lookup enum sym-or-val)
    (if (symbol? sym-or-val)
        (cond ((vector-index eq? (genum-symbols enum) sym-or-val)
               => (lambda (i) (vector-ref (genum-values enum) i)))
              (else #f))
        (cond ((vector-index eqv? (genum-values enum) sym-or-val)
               => (lambda (i) (vector-ref (genum-symbols enum) i)))
              (else #f)))))
