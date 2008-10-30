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
          gobject-class-parent
          gobject-class-get-signal-callback
          gobject-class-get-property-info
          gobject-class-decorate

          make-ginstance ginstance? ginstance-ptr ginstance-class

          send-message
          send
          
          make-genum genum? genum-lookup genum-values genum-symbols

          gerror-type? make-gerror-type)
  (import (rnrs base)
          (rnrs control)
          (rnrs lists)
          (rnrs records syntactic)
          (rnrs mutable-pairs)
          (spells alist)
          (spells receive)
          (spells tracing)
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
            (mutable load-members) 
            (mutable parent)
            (mutable constructors)
            (mutable methods)
            (mutable signals)
            (mutable properties))
    (protocol (lambda (p)
                (lambda (namespace name load-members)
                  (p namespace name load-members #f #f #f #f #f)))))


  (define-record-type gerror-type)
  
  (define (gobject-class-get-signal-callback class signal)
    (let ((signature (lookup-signal class signal)))
      (and signature (signature-callback signature))))

  (define gobject-class-get-property-info
    (lambda (class property)
      (lookup-property class property)))
  
  (define (gobject-class-decorate class constructors-decorator methods-decorator signals-decorator)
    (make-gobject-class (gobject-class-namespace class)
                        (gobject-class-name class)
                        (lambda (new-class)
                          (gobject-class-force! class)
                          (values
                           (gobject-class-parent class)
                           (constructors-decorator (gobject-class-constructors class))
                           (methods-decorator (gobject-class-methods class))
                           (signals-decorator (gobject-class-signals class))
                           (gobject-class-properties class)))))

  (define lookup-method (make-gobject-class-lookup gobject-class-methods))
  (define lookup-property (make-gobject-class-lookup gobject-class-properties))
  (define lookup-signal (make-gobject-class-lookup gobject-class-signals))
  
  (define (make-gobject-class-lookup accessor)
    (define (lookup class name)
      (cond ((assq name (accessor class))
             => (lambda (entry)
                  (when (lazy-entry? (cdr entry))
                    (set-cdr! entry ((lazy-entry-proc (cdr entry)))))
                  (cdr entry)))
            ((gobject-class-parent class) => (lambda (parent)
                                               (gobject-class-force! parent)
                                               (lookup parent name)))
            (else #f)))
    lookup)

  (define (gobject-class-force! class)
    (cond ((gobject-class-load-members class)
           => (lambda (loader)
                (receive (parent constructors methods signals properties) (loader class)
                  (gobject-class-parent-set! class parent)
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
                         (set-cdr! entry ((lazy-entry-proc (cdr entry)))))
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
  (define-record-type (genum make-genum% genum?)
    (fields
     (immutable syms genum-symbols)
     (immutable vals genum-values)))

  ;; Note: this could be made more efficient by using sorted vectors
  ;; (but only in one direction)
  (define (genum-lookup enum sym-or-val)
    (if (symbol? sym-or-val)
        (cond ((vector-index eq? (genum-symbols enum) sym-or-val)
               => (lambda (i) (vector-ref (genum-values enum) i)))
              (else #f))
        (cond ((vector-index eqv? (genum-values enum) sym-or-val)
               => (lambda (i) (vector-ref (genum-symbols enum) i)))
              (else #f))))

  (define (make-genum alist)
    (make-genum% (list->vector (map car alist)) (list->vector (map cdr alist)))))
